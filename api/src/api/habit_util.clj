(ns api.habit-util
  "A namespace for holding utilities related to habits and their fields.")

(defn get-frequency
  "Returns the target/threshold `frequency` object of a habit."
  [habit]
  (if (= (:type_name habit) "good_habit")
    (:target_frequency habit)
    (:threshold_frequency habit)))
