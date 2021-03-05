(ns go.board.util-test
  (:use [clojure.test]
        [go.board.util]))


(deftest board-from-string-test
  (is
   (=
    (board-from-string
     "O.X
      O.X"
     3)
    {:size      3
     :gostrings {#{[1 1] [1 2]} {:color :white :liberties #{[2 2] [1 3] [2 1]}}
                 #{[3 1] [3 2]} {:color :black :liberties #{[2 2] [3 3] [2 1]}}}})))


(deftest board-from-string-test-2
  (is
   (=
    (board-from-string
     "O.O
      X.X"
     3)
    {:size      3
     :gostrings {#{[1 1]} {:color :white :liberties #{[2 1]}}
                 #{[3 1]} {:color :white :liberties #{[2 1]}}
                 #{[1 2]} {:color :black :liberties #{[2 2] [1 3]}}
                 #{[3 2]} {:color :black :liberties #{[2 2] [3 3]}}}})))


(deftest board-from-string-test-3
  (is
   (=
    (board-from-string
     "X.X
      XXX"
     3)
    {:size      3
     :gostrings {#{[1 1] [1 2] [2 2] [3 1] [3 2]}
                 {:color :black :liberties #{[2 1] [1 3] [2 3] [3 3]}}}})))