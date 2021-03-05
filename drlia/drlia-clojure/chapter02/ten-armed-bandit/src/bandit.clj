(ns bandit)

(defn- pull-lever
  [hidden-probability fn:rand-proba]
  (reduce
   (fn [acc _]
     (+ acc (if (< (fn:rand-proba) hidden-probability) 1 0)))
   0
   (range 10)))

(defn- new-average
  [[old-k old-average] new-val]
  (let [new-k (inc old-k)]
    [new-k
     (float
      (/ (+ new-val (* old-average old-k))
         new-k))]))

(defn- add-average
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

(defn run-reducer
  [fn:rand-proba fn:get-best-lever]
  (fn [[bandits averages] _]
    (let [probas                (vec (map first bandits))
          levers                (vec (map second bandits))
          selected-lever        (fn:get-best-lever levers fn:rand-proba)
          payoff                (pull-lever (get probas selected-lever) fn:rand-proba)
          new-average           (new-average (get levers selected-lever) payoff)]
      [(vec (assoc-in bandits [selected-lever 1] new-average))
       (add-average averages payoff)])))

(defn generate-bandits [probas]
  (vec (map (fn [proba] [proba [0 0.0]]) probas)))

(defn run
  [probas times fn:rand-proba fn:get-best-lever]
  (let [bandits (generate-bandits probas)]
    (->
     (reduce (run-reducer fn:rand-proba fn:get-best-lever)
             [bandits '()]
             (range times))
     (update 1 reverse))))
