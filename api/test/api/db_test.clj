(ns api.db-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [api.db :refer :all]
            [monger.core :as mg]
            [monger.db :as mdb]
            [clj-time.core :as t]))

(defonce test_conn (mg/connect))
(defonce test_db (mg/get-db test_conn "test_db"))

(def default_habit {:name "test habit"
                    :description "test description"
                    :unit_name_singular "test unit"
                    :unit_name_plural "test units"
                    :time_of_day :ANYTIME})

(defn add-habit-to-test-db
  "Add a habit to the test database"
  [habit]
  (add-habit {:db test_db :habit habit}))

(defn compare_clojure_habit_with_db_habit
  "Returns true iff all fields of `clojure_habit` have the same value in `db_habit`.
  Checks Keyword values in `clojure_habit` against the Keyword-ed version of the value in `db_habit`
  since Keywords are converted to Strings by Monger.
  Note that this function doesn't care if `db_habit` has a field that `clojure_habit` doesn't have."
  [clojure_habit db_habit]
  (every? (fn [key]
           (let [clj_val (key clojure_habit)
                 db_val (key db_habit)]
             (if (= (type clj_val) clojure.lang.Keyword)
               (= clj_val (keyword db_val))
               (= clj_val db_val))))
          (keys clojure_habit)))

(deftest add-habit-test
  (is (= 0 (count (get-habits {:db test_db}))))
  (let [habit_1 (assoc default_habit
                     :type_name "good_habit"
                     :target_frequency {:type_name "total_week_frequency"
                                        :week 6})
        _ (add-habit-to-test-db habit_1)
        all_habits (get-habits {:db test_db})]
    (is (= 1 (count all_habits)))
    (is (some #(compare_clojure_habit_with_db_habit habit_1 %) all_habits) "Habit 1 not added properly")
    (is (every? #(= false (:suspended %)) all_habits) ":suspended field not set to false")
    (is (every? #(not (nil? (:_id %))) all_habits) ":_id field not set")
    (let [habit_2 (assoc default_habit
                         :type_name "bad_habit"
                         :threshold_frequency {:type_name "every_x_days_frequency"
                                               :days 4
                                               :times 3})
          _ (add-habit-to-test-db habit_2)
          all_habits (get-habits {:db test_db})]
      (is (= 2 (count all_habits)))
      (is (some #(compare_clojure_habit_with_db_habit habit_1 %) all_habits) "Habit 1 not added properly")
      (is (some #(compare_clojure_habit_with_db_habit habit_2 %) all_habits) "Habit 2 not added properly")
      (is (every? #(= false (:suspended %)) all_habits) ":suspended field not set to false")
      (is (every? #(not (nil? (:_id %))) all_habits) ":_id field not set"))))

(deftest get-specific-day-of-week-target-frequency-stats-test
  (is (= (count (get-habits {:db test_db})) 0))
  (let [habit_name "habit 1"
        habit_desc "Good habit, specific day of week freq"
        habit_type "good_habit"
        habit_freq {:monday 1
                    :tuesday 1
                    :wednesday 1
                    :thursday 1
                    :friday 1
                    :saturday 1
                    :sunday 1
                    :type_name "specific_day_of_week_frequency"}
        habit_freq_type :target_frequency
        _ (add-habit-to-test-db {:name habit_name
                                 :description habit_desc
                                 :type_name habit_type
                                 habit_freq_type habit_freq})
        all_habits (get-habits {:db test_db})
        habit_from_db (first all_habits)
        id (str (:_id habit_from_db))]
    (is (= 1 (count all_habits)))
    (are [value key] (= value (key habit_from_db))
         habit_name :name
         habit_desc :description
         habit_freq habit_freq_type)
    (is (not= "" id) "Habit should've been given an id")
    (testing "Get frequency stats"
      (testing "with no habit data"
        (is (= [nil] (get-frequency-stats {:db test_db}))))
      (testing "with a successful habit record yesterday"
        (let [_ (set-habit-data {:db test_db
                                 :habit_id id
                                 :amount 4
                                 :date-time (t/minus (t/today-at 0 0) (t/days 1))})
               stats (first (get-frequency-stats {:db test_db}))]
            (are [value key] (= value (key stats))
                 id :habit_id
                 1 :total_fragments
                 1 :successful_fragments
                 4 :total_done
                 1 :fragment_streak)
            (testing "and a failure habit record the day before"
               (let [_ (set-habit-data {:db test_db
                                        :habit_id id
                                        :amount 0
                                        :date-time (t/minus (t/today-at 0 0) (t/days 2))})
                     stats (first (get-frequency-stats {:db test_db}))]
                  (are [value key] (= value (key stats))
                       id :habit_id
                       2 :total_fragments
                       1 :successful_fragments
                       4 :total_done
                       1 :fragment_streak))))))))

(defn each-fixture
  "Drop test database before and after each test"
  [f]
  (mdb/drop-db test_db)
  (f)
  (mdb/drop-db test_db))

(use-fixtures :each each-fixture)
