(ns contextual.math)

(defn rand-proba [] (/ (rand-int 100) 100))

(defn rand-choice
  [values-which-add-up-to-1 fn:rand-proba]
  (second
    (let [proba (fn:rand-proba)]
      (reduce-kv
       (fn [[accumulated index] key value]
         (let [new-accumulated (+ accumulated value)]
           (if (> new-accumulated proba)
             (reduced [value key])
             [new-accumulated key])))
       [0. 0]
       values-which-add-up-to-1))))

(defn softmax [values tau]
  (let [new-vals (map #(Math/exp %) (map #(/ %1 tau) values))
        summed   (reduce + new-vals)]
    (mapv #(/ %1 summed) new-vals)))

(defn matrix [x y f]
  (mapv
   (fn [_]
     (mapv
      f
      (range x)))
   (range y)))

(defn- new-average
  [[old-k old-average] new-val]
  (let [new-k (inc old-k)]
    [new-k
     (float
      (/ (+ new-val (* old-average old-k))
         new-k))]))

(defn add-average
  "
  averages: list, where most recent average is appended to the front

  user=> (add-average '(4. 6.) 1.)
  (3.0 4.0 6.0)
  "
  [averages new-payoff]
  (conj averages
        (float
         (if (empty? averages)
           new-payoff
           (/
            (+ new-payoff
               (* (first averages) (count averages)))
            (inc (count averages)))))))