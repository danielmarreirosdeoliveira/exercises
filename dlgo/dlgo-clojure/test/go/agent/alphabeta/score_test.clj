(ns go.agent.alphabeta.score-test
  (:use [clojure.test]
        [go.board.util]
        [go.agent.alphabeta.score]))


(deftest eval-board-test-1
  (is (= 2
       (score-situation
        {:board (board-from-string
           "X.
            X." 2)}))))

(deftest eval-board-test-2
  (is (= -2
         (score-situation
          {:board (board-from-string
           "O.
            O." 2)}))))

(deftest eval-board-test-3
  (is (= -1
         (score-situation
          {:board (board-from-string
           "O.
            OX" 2)}))))