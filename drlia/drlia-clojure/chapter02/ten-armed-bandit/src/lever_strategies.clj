(ns lever-strategies
  (:require [math :as m]))

(defn- get-best-lever
  "
  user=> (get-best-lever [1.0 1.5])
  1
  "
  [averages]
  (first
   (apply max-key second (map-indexed vector averages))))

(defn get-best-lever-epsilon-greedy
  [epsilon]
  (fn [levers fn:rand-proba]
    (if
      (< (fn:rand-proba) epsilon)
      (rand-int (count levers))
      (get-best-lever (map second levers)))))

(defn get-best-lever-softmax
  [tau]
  (fn [levers fn:rand-proba]
    (-> (m/softmax (map second levers) tau)
        (m/rand-choice fn:rand-proba)
        second)))