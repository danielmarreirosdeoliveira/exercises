(ns contextual.nd
  (:import (org.nd4j.linalg.factory Nd4j)
           (org.nd4j.linalg.api.ndarray INDArray)))

(defn as-nd-array [array-of-double] ; todo used?
  (Nd4j/createFromArray (double-array array-of-double)))

(defn row-array-from-1d-vector [what]
  (Nd4j/createFromArray (into-array [(into-array what)])))

(defn network-output-to-1d-vector [nd-array]
  (vec (.toDoubleVector (get (.toArray nd-array) 2))))