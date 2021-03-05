(ns go.agent.alphabeta-test
  (:use [clojure.test]
        [go.agent.alphabeta]
        [go.board.util]))


(deftest move-test-1
  (let [given-board (board-from-string
                     ".X
                      .O
                     " 2)]
    (is
     (=
      [:ok
       {:player  :black
        :board
        (board-from-string
         "O.
          .O" 2)
        :history (list given-board)}]
      ((agent-move 0)
        {:player :white
         :board given-board})))))


(deftest move-test-2
  (let [given-board (board-from-string
                     "...
                      OXO
                      OXO
                     " 3)]
    (is
     (=
      [:ok
       {:player  :black
        :board
        (board-from-string
         ".O.
          O.O
          O.O" 3)
        :history (list given-board)}]
      ((agent-move 1)
        {:player :white
         :board given-board})))))




