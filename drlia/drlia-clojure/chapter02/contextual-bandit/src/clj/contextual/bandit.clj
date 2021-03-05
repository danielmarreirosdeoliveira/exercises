(ns contextual.bandit
  (:import (org.deeplearning4j.nn.multilayer MultiLayerNetwork))
  (:require [contextual.math :as m]
            [contextual.nd :as nd]
            [contextual.ml :as ml]
            [contextual.helper :as h]))

(defn make-arms [arms fun:rand-proba]
  "generates a matrix of random-values
  where the rows represent states and columns represent arms
  "
  (m/matrix arms arms (fn [_] (fun:rand-proba))))

(defn predict [model input]
  (nd/network-output-to-1d-vector
   (.feedForward ^MultiLayerNetwork model input)))

(defn run [^MultiLayerNetwork model
           num-arms
           num-iterations
           fun:rand-proba]
  (let [arms             (make-arms num-arms fun:rand-proba)]
    (reduce
     (fn [averages _]
       (let [current-state                         (rand-int num-arms)
             current-state-one-hot-encoded         (ml/one-hot num-arms current-state)
             input                                 (nd/row-array-from-1d-vector current-state-one-hot-encoded)
             predicted                             (predict model input)
             softmaxed                             (m/softmax predicted 1.12)
             selected                              (m/rand-choice softmaxed fun:rand-proba)
             hidden-proba                          (get-in arms [current-state selected])
             reward                                (h/reward hidden-proba fun:rand-proba)
             adjusted                              (assoc predicted selected reward)
             new-averages                          (m/add-average averages reward)]

         (.fit model input (nd/row-array-from-1d-vector (double-array adjusted)))
         (prn (first new-averages))
         new-averages))
     '()
     (range num-iterations))))