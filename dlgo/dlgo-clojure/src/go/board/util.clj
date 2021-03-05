(ns go.board.util
  (:use [clojure.set]
        [go.board.board]))


(defn- flat-matrix-row-from-string
  [string i]
  (remove nil?
          (map-indexed
           (fn [idx s]
             (if (not= s \.)
               [[(inc idx) i]
                (if (= \O s)
                  :white
                  :black)]
               nil))
           string)))


(defn- flat-matrix-from-string
  [string]
  (into []
        (apply concat
               (map-indexed
                (fn [idx line]
                  (flat-matrix-row-from-string (clojure.string/trim line) (inc idx)))
                (clojure.string/split-lines string)))))


(defn board-from-string [string size]
  (reduce (fn [board [stone color]]
            (set-stone {:board board :player color} stone))
          {:gostrings {} :size size}
          (flat-matrix-from-string string)))