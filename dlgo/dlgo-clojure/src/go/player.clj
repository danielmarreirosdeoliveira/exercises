(ns go.player)

(defn other [player] (if (= :white player) :black :white))