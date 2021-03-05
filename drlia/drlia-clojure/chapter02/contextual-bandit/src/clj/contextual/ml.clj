(ns contextual.ml
  "General Machine Learning Tools")

(defn one-hot [N pos]
  (assoc (vec (replicate N 0.0)) pos 1.0))