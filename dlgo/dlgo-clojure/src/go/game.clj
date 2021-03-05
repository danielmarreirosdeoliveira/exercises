(ns go.game
  "Manages Moves,
  Board History and Players
  "
  (:use [go.player]
        [go.draw]))


(defn- draw-scene [{player                            :player
                    {size :size gostrings :gostrings} :board}]
  (clear-screen)

  (draw-gostrings gostrings size)
  (print (str "\n" player "\n"))

  (print "================================================================\n"))


(defn game [player-agent-config start-situation]
  (loop [[situation num-consecutive-passes] [start-situation 0]]
    (let [player-o        (:player situation)]

      (draw-scene situation)

      (let [[status {board :board :as situation}]                               ((player-o player-agent-config) situation)
            num-consecutive-passes                                              (if (= :pass status) (inc num-consecutive-passes) 0)]

        (println status)
        (Thread/sleep 1100)
        (cond
          (= :resign status)           (println (str "player " player-o " resigned - ending game"))
          (= num-consecutive-passes 2) (println "2 consecutive passes - ending game")
          :else                        (recur [situation num-consecutive-passes]))))))