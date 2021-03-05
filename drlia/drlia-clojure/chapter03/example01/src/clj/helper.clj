(ns helper
  (:import (gridworld GridWorldModel))
  (:require [nd :as nd]
            [grid-world :as g]))

(defn Y [q-vals q-index target-val]
  (nd/row-array-from-1d-vector
   (assoc q-vals q-index target-val)))

(defn as-action [index]
  (cond
    (= index 0)
    "u"
    (= index 1)
    "r"
    (= index 2)
    "d"
    (= index 3)
    "l"))

(defn rand-proba [] (/ (rand-int 100) 100))

(defn choose [output]
  (apply max-key second (map-indexed (fn [i v] [i v]) output)))

(defn X [board]
  (nd/row-array-from-1d-vector
   (mapv #(+ % (- (/ (rand-int 1000) 100000.0) 0.005))
         (flatten (g/display board)))))

(defn feed-forward [model X]
  (nd/network-output-to-1d-vector
   (.feedForward model X)))

(defn calc-Q [q-2 reward]
  (if (= -1.0 reward)
    (+ -1.0 (* 0.9 q-2))
    reward))

(defn dec-epsilon [epsilon] ; todo make it proportional to the total number of episodes
  (if (> epsilon 0.2)
    (- epsilon 0.1)
    epsilon))