(ns contextual.helper)

(defn reward
  [hidden-probability fn:rand-proba]
  (reduce
   (fn [acc _]
     (+ acc (if (< (fn:rand-proba) hidden-probability) 1 0)))
   0
   (range 10)))