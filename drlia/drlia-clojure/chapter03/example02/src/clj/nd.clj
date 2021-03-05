(ns nd
  (:import (org.nd4j.linalg.factory Nd4j)
           (org.nd4j.linalg.api.ndarray INDArray)))

(defn row-array-from-1d-vector [what]
  (Nd4j/createFromArray (into-array [(into-array what)])))

(defn network-output-to-1d-vector [nd-array]
  (vec (.toDoubleVector (get (.toArray nd-array) 3))))


(defn row-vectors-to-nd
  [what]
  (let [t (into-array (map into-array what))]
    (Nd4j/createFromArray t)))