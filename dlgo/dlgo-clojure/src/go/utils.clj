(ns go.utils)


(defn permutations
  "
  size: 2
  '([1 1] [1 2] [2 1] [2 2]) ; with no guarantee for order
  "
  [size]
  (apply concat
         (map
          (fn [[l r]]
            (map vector (repeat l) r))
          (map vector (range 1 (inc size)) (repeat (range 1 (inc size)))))))


(defn pair-by-predicate [p coll]
  [(filter p coll)
   (remove p coll)])


(defn debug-id [v]
  (prn "DEBUG" v)
  v)