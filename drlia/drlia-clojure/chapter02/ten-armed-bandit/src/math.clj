(ns math)

(defn softmax [values tau]
  (let [new-vals (map #(Math/exp %) (map #(/ %1 tau) values))
        summed   (reduce + new-vals)]
    (vec (map #(/ %1 summed) new-vals))))

(defn rand-choice
  [values fn:rand-proba]
  (let [proba (fn:rand-proba)]
    (reduce-kv
     (fn [[accumulated index] key value] ; todo index necessary?
       (let [new-accumulated (+ accumulated value)]
         (if (> new-accumulated proba)
           (reduced [value key])
           [new-accumulated key])))
     [0. 0]
     values)))