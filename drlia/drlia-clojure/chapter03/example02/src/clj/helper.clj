(ns helper
  (:import (gridworld GridWorldModel))
  (:require [nd :as nd]
            [grid-world :as g]))

(defn Y [q-vals q-index target-val]
  (assoc q-vals q-index target-val))

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
  (mapv #(+ % (- (/ (rand-int 1000) 100000.0) 0.005))
        (flatten (g/display board))))

(defn feed-forward [model X]
  (nd/network-output-to-1d-vector ; todo dependency to nd move to run.js
   (.feedForward model X)))

(defn calc-Q [q-2 reward]
  (if (= -1.0 reward)
    (+ -1.0 (* 0.9 q-2))
    reward))

(defn dec-epsilon [epsilon episodes]
  (if (> epsilon 0.2)
    (- epsilon (/ 1.0 episodes))
    epsilon))

(defn select-randomly
  [coll]
  (vec
    (take (+ 2 (rand-int 30))
          (shuffle coll))))