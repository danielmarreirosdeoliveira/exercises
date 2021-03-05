(ns go.all-tests
  (:use [clojure.test]
        [go.board.util-test]
        [go.move-test]
        [go.board.board-test]
        [go.agent.alphabeta.abprune-test]
        [go.agent.alphabeta-test]
        [go.agent.alphabeta.score-test]))

(let [{test :test pass :pass fail :fail error :error}
      (run-tests 'go.board.board-test 'go.move-test 'go.board.util-test 'go.agent.alphabeta.abprune-test
                 'go.agent.alphabeta.score-test 'go.agent.alphabeta-test)]
      (if
        (or (> error 0) (> fail 0))
        (System/exit 1)
        (System/exit 0)))