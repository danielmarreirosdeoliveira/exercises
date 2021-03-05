(ns go.agent.alphabeta.score)


(defn score-situation
  [{{gostrings :gostrings} :board}]
  (reduce-kv
   (fn [acc k v]
     ((if (= (:color v) :black) + -)
       acc (count k)))
   0 gostrings))