(ns go.agent.random
  (:use [go.utils]
        [go.move]
        [go.draw]))


(defn agent-move
  []
  (fn inner
    ([{board :board :as situation} skip-prompt]
     (if (not skip-prompt) (println "Bot will calc move"))

     (let [rand-x                   (inc (rand-int (:size board)))
           rand-y                   (inc (rand-int (:size board)))
           [status :as move-result] (move situation [rand-x rand-y])]
       (if (= :ok status)
         move-result
         (do
           (println status)
           (Thread/sleep 1000)
           (cursor-up)
           (kill-line)
           (cursor-left-side)
           (inner situation true)))))
    ([situation]
     (inner situation false))))