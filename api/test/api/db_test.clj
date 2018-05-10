(ns api.db-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [api.db :refer :all]
            [api.freq-stats-util :refer [default-frequency-stats]]
            [monger.core :as mg]
            [monger.db :as mdb]
            [clj-time.core :as t]))

; Useful variables
(def test_conn (mg/connect))
(def test_db (mg/get-db test_conn "test_db"))
(def default_habit {:name "test habit" :description "test description" :unit_name_singular "test unit"
                    :unit_name_plural "test units" :time_of_day :ANYTIME})
(def today (t/today-at 0 0))

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

(defn supermap?
  "Returns true iff all fields of `map2` have the same value in `map1`.
  I.e., `map1` is a supermap of `map2`. Patent pending."
  [map1 map2]
  (nil? (first (diff map2 map1))))

(deftest add-habit-test
  (testing "No habits"
    (is (= 0 (count (get-habits {:db test_db})))))
  (let [habit_1 (assoc default_habit
                       :type_name "good_habit"
                       :target_frequency {:type_name "total_week_frequency"
                                          :week 6})
        _ (add-habit-to-test-db habit_1)
        all_habits (get-habits {:db test_db})]
    (testing "One habit"
      (is (= 1 (count all_habits)) "There should be one habit in the db")
      (is (some #(compare_clojure_habit_with_db_habit habit_1 %) all_habits) "Habit 1 not added properly")
      (is (every? #(= false (:suspended %)) all_habits) ":suspended field not set to false")
      (is (every? #(not (nil? (:_id %))) all_habits) ":_id field not set"))
    (let [habit_2 (assoc default_habit
                         :type_name "bad_habit"
                         :threshold_frequency {:type_name "every_x_days_frequency"
                                               :days 4
                                               :times 3})
          _ (add-habit-to-test-db habit_2)
          all_habits (get-habits {:db test_db})]
      (testing "Two habits"
        (is (= 2 (count all_habits)) "There should be two habits in the db")
        (is (some #(compare_clojure_habit_with_db_habit habit_1 %) all_habits) "Habit 1 not added properly")
        (is (some #(compare_clojure_habit_with_db_habit habit_2 %) all_habits) "Habit 2 not added properly")
        (is (every? #(= false (:suspended %)) all_habits) ":suspended field not set to false")
        (is (every? #(not (nil? (:_id %))) all_habits) ":_id field not set")))))

(deftest get-frequency-stats-test
  (testing "No habits added yet"
    (is (= 0 (count (get-habits {:db test_db})))))
  (testing "Good habit, specific day of week frequency"
    (let [habit (assoc default_habit
                       :type_name "good_habit"
                       :target_frequency {:type_name "specific_day_of_week_frequency"
                                          :monday 2 :tuesday 2 :wednesday 2 :thursday 2
                                          :friday 2 :saturday 2 :sunday 2})
          final_habit (add-habit-to-test-db habit)
          habit_id (:_id final_habit)
          habit_id_str (str habit_id)]
      (testing "with no habit data"
        (is (= 1 (count (get-habits {:db test_db}))) "There should only be one habit so far")
        (is (= [(assoc default-frequency-stats :habit_id habit_id)]
               (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})))
        (is (= [(assoc default-frequency-stats :habit_id habit_id)]
               (get-frequency-stats {:db test_db})) "`habit_ids` should be an optional param"))
      (testing "with a successful habit record yesterday"
        (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 4
                                 :date-time (t/minus today (t/days 1))})
              stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
          (is (= stats [{:habit_id habit_id
                         :total_fragments 1 :successful_fragments 1 :total_done 4
                         :current_fragment_streak 1 :best_fragment_streak 1
                         :current_fragment_total 0 :current_fragment_goal 2 :current_fragment_days_left 1}])))
        (testing "and a failure habit record the day before"
          (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 1
                                   :date-time (t/minus today (t/days 2))})
                stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
            (is (= stats [{:habit_id habit_id
                           :total_fragments 2 :successful_fragments 1 :total_done 5
                           :current_fragment_streak 1 :best_fragment_streak 1
                           :current_fragment_total 0 :current_fragment_goal 2 :current_fragment_days_left 1}])))
          (testing "and at 11pm today the user did 3 units"
            (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 3 :date-time (t/plus today (t/hours 23))})
                  stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
              (is (= stats [{:habit_id habit_id
                             :total_fragments 2 :successful_fragments 1 :total_done 8
                             :current_fragment_streak 1 :best_fragment_streak 1
                             :current_fragment_total 3 :current_fragment_goal 2 :current_fragment_days_left 1}]))))))))
  (testing "Good habit, total week frequency"
    (let [habit (assoc default_habit
                       :type_name "good_habit"
                       :target_frequency {:type_name "total_week_frequency"
                                          :week 5})
          final_habit (add-habit-to-test-db habit)
          habit_id (:_id final_habit)
          habit_id_str (str habit_id)]
      (testing "with last week as a failure"
        (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 3
                                 ; 7 days ago was in the previous week, which has now ended
                                 :date-time (t/minus today (t/days 7))})
              stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
          (is (supermap? stats [{:habit_id habit_id
                                 ; this week hasn't ended so we only track last week
                                 :total_fragments 1 :successful_fragments 0 :total_done 3
                                 :current_fragment_streak 0 :best_fragment_streak 0
                                 :current_fragment_total 0 :current_fragment_goal 5}])))
        (testing "and the week before as a success"
          (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 5
                                   :date-time (t/minus today (t/days 14))})
                stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
            (is (supermap? stats [{:habit_id habit_id
                                   :total_fragments 2 :successful_fragments 1 :total_done 8
                                   :current_fragment_streak 0 :best_fragment_streak 1
                                   :current_fragment_total 0 :current_fragment_goal 5}])))
          (testing "and today the user did 6 units"
            (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 6 :date-time today})
                  stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
              (is (supermap? stats [{:habit_id habit_id
                                     :total_fragments 2 :successful_fragments 1 :total_done 14
                                     :current_fragment_streak 0 :best_fragment_streak 1
                                     :current_fragment_total 6 :current_fragment_goal 5}]))))))))
  (testing "Good habit, every x days frequency"
    (let [habit (assoc default_habit
                       :type_name "good_habit"
                       :target_frequency {:type_name "every_x_days_frequency"
                                          :times 3 :days 5})
          final_habit (add-habit-to-test-db habit)
          habit_id (:_id final_habit)
          habit_id_str (str habit_id)]
      (testing "with the last fragment as a success"
        (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 200
                                 :date-time (t/minus today (t/days 5))})
              stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
          (is (supermap? stats [{:habit_id habit_id
                                 :total_fragments 1 :successful_fragments 1 :total_done 200
                                 :current_fragment_streak 1 :best_fragment_streak 1
                                 :current_fragment_total 0 :current_fragment_goal 3}])))
        (testing "and three fragments ago as a failure"
          (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 2
                                   :date-time (t/minus today (t/days 15))})
                stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
            (is (supermap? stats [{:habit_id habit_id
                                   :total_fragments 3 :successful_fragments 1 :total_done 202
                                   :current_fragment_streak 1 :best_fragment_streak 1
                                   :current_fragment_total 0 :current_fragment_goal 3}])))))))
  (testing "Bad habit, total week frequency"
    (let [habit (assoc default_habit
                       :type_name "bad_habit"
                       :threshold_frequency {:type_name "total_week_frequency"
                                             :week 20})
          final_habit (add-habit-to-test-db habit)
          habit_id (:_id final_habit)
          habit_id_str (str habit_id)]
      (testing "with last week as a success"
        (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 0
                                 :date-time (t/minus today (t/days 7))})
              stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
          (is (supermap? stats [{:habit_id habit_id
                                 :total_fragments 1 :successful_fragments 1 :total_done 0
                                 :current_fragment_streak 1 :best_fragment_streak 1
                                 :current_fragment_total 0 :current_fragment_goal 20}])))
        (testing "and the week before that as a success"
          (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 20
                                   :date-time (t/minus today (t/days 14))})
                stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
            (is (supermap? stats [{:habit_id habit_id
                                   :total_fragments 2 :successful_fragments 2 :total_done 20
                                   :current_fragment_streak 2 :best_fragment_streak 2
                                   :current_fragment_total 0 :current_fragment_goal 20}])))
          (testing "and three weeks ago as a failure"
            (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 21
                                     :date-time (t/minus today (t/days 21))})
                  stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
              (is (supermap? stats [{:habit_id habit_id
                                     :total_fragments 3 :successful_fragments 2 :total_done 41
                                     :current_fragment_streak 2 :best_fragment_streak 2
                                     :current_fragment_total 0 :current_fragment_goal 20}])))))))))

(defn drop-test-db-fixture
  "Drop test database before and after each test"
  [f]
  (mdb/drop-db test_db)
  (f)
  (mdb/drop-db test_db))

(use-fixtures :each drop-test-db-fixture)
