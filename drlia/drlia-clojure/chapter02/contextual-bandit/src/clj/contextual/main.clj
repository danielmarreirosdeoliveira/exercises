(ns contextual.main
  (:import (contextual BanditModel))
  (:require [contextual.bandit :as b]
            [contextual.math :as m])
  (:gen-class))

(defn -main
  [& args]
  (let [num-arms                    10
        num-iterations              100000
        fun:rand-proba              m/rand-proba
        model                       (BanditModel/create num-arms)
        _                           (.init model)]
    (b/run model num-arms num-iterations fun:rand-proba)))