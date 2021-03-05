(ns bandit-test
  (:require [bandit :as b]
            [lever-strategies :as l])
  (:use [clojure.test :as t]))

(defn static-probas [probabilities]
  (let [probas (atom probabilities)]
    (fn []
      (let [first (first @probas)]
        (swap! probas pop)
        first))))

(defn testrun []
  (b/run
   [0.7 0.8]
   2
   (static-probas '(0.1
                    0.1 0.1 0.1 0.1 0.9
                    0.9 0.9 0.9 0.9 0.9
                    0.3
                    0.1 0.1 0.1 0.9 0.9
                    0.9 0.9 0.9 0.9 0.9))
   (l/get-best-lever-epsilon-greedy 0.0)))

(deftest test-testrun
  (is
   (=
    (testrun)
    [[[0.7 [0 0.0]] [0.8 [2 3.5]]] '(4.0 3.5)])))

(defn testrun-2 []
  (b/run
   [0.4 0.2 0.4]
   1
   (static-probas '(0.5
                    0.1 0.1 0.1 0.1 0.9
                    0.9 0.9 0.9 0.9 0.9))
   (l/get-best-lever-softmax 1.12)))

(deftest test-testrun-2
  (is
   (=
    (testrun-2)
    [[[0.4 [0 0.0]] [0.2 [1 4.0]] [0.4 [0 0.0]]] '(4.0)])))
