(ns go.board.board-test
  (:use
    [clojure.test]
    [go.board.board]
    [go.board.util]))

;; note: these tests are inductive, since board-from-string
;; itself uses set-stone. so here we say,
;; assuming that board-from-string works for a given board
;; and for the result board of set-stone, set-stone should produce that
;; result.

(deftest calc-liberties-at-bottom-right-corner
  (is
   (=
    (set-stone
     {:board  (board-from-string
               "..
                ..
               " 2)
      :player :black}
     [2 2])
    (board-from-string
     "..
      .X
     " 2))))


(deftest connect-two-black-stones
  (is
   (=
    (set-stone
     {:board  (board-from-string
               "X..
                ...
               " 3)
      :player :black}
     [1 2])
    (board-from-string
     "X..
      X..
     " 3))))

(deftest connect-two-white-stones
  (is
   (=
    (set-stone
     {:board
      (board-from-string
       "O.
        ..
       " 2)
      :player :white}
     [1 2])
    (board-from-string
     "O.
      O.
     " 2))))

(deftest place-white-between-white-and-black
  (is
   (=
    (set-stone
     {:board
      (board-from-string
       "O.X
       " 4)
      :player :white}
     [2 1])
    (board-from-string
     "OOX
     " 4))))

(deftest connect-two-black-strings
  (is
   (=
    (set-stone
     {:board
      (board-from-string
       "X.X.
        X.X.
       " 4)
      :player :black}
     [2 1])
    (board-from-string
     "XXX.
      X.X.
     " 4))))

(deftest connect-four-strings-and-ignore-one-external-stone
  (is
   (=
    (set-stone
     {:board
      (board-from-string
       ".O..
        X.X.
        .O..
        ...X
       " 7)
      :player :white}
     [2 2])
    (board-from-string
     ".O..
      XOX.
      .O..
      ...X
     " 7))))

(deftest capture-white-stone
  (is
   (=
    (set-stone
     {:board
      (board-from-string
       "O..
        X..
       " 3)
      :player :black}
     [2 1])
    (board-from-string
     ".X.
      X..
     " 3))))

(deftest capture-two-white-strings
  (is
   (=
    (set-stone
     {:board
      (board-from-string
       "O.OOX..
        X.XX...
       " 7)
      :player :black}
     [2 1])
    (board-from-string
     ".X..X..
      X.XX...
     " 7))))