(ns go
  (:require [bandit :as b]
            [lever-strategies :as s]))

(defn rand-proba [] (/ (rand-int 100) 100))

(defn generate-probas []
  (vec (map (fn [_] (float (rand-proba))) (range 10))))

(def get-best-lever (s/get-best-lever-epsilon-greedy 0.2))
(def get-best-lever (s/get-best-lever-softmax 1.12))

(defn go-bandit [times]
  (b/run (generate-probas)
         times
         rand-proba
         get-best-lever))

(defn spit-running-averages [times]
  (spit "running-averages" (second (go-bandit times))))