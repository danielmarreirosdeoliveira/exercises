(ns go.agent.alphabeta
  (:use [go.move]
        [go.agent.alphabeta.abprune]
        [go.agent.alphabeta.score]))


(defn agent-move-reducer
  [player depth]
  (fn [[best-result selected-move] possible-move]
    (let [ab-result                 (ab possible-move possible-moves score-situation depth)
          should-be-new-best-move   ((if (= player :white) < >) ab-result best-result)]
      (if
        should-be-new-best-move
        [ab-result possible-move]
        [best-result selected-move]))))


(defn agent-move
  [depth]
  (fn
    [{player :player :as situation}]
    [:ok
     (second
      (reduce (agent-move-reducer player depth)
              [(if (= player :white) 999 -999) nil] (possible-moves situation)))]))