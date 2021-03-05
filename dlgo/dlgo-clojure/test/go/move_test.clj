(ns go.move-test
  (:use [clojure.test]
        [go.board.util]
        [go.move]))


(deftest
  is-self-capture
  (is
   (=
    (move
     {:history '()
      :player  :white
      :board   (board-from-string
                "X.X
                 XXX
                " 3)}
     [2 1])
    [:self-capture
     {:board   (board-from-string
                "X.X
                 XXX
                " 3)
      :player  :white
      :history '()}])))


(deftest
  is-not-self-capture
  (is
   (=

    (move
     {:history '()
      :player  :white
      :board   (board-from-string
                "X..
                 XXX
                " 3)}
     [2 1])
    [:ok
     {:board   (board-from-string
                "XO.
                 XXX
                " 3)
      :player  :black
      :history (list
                (board-from-string
                 "X..
                  XXX
                 " 3))}])))


(deftest is-an-eye
  (is
   (=

    (move
     {:history '()
      :player  :white
      :board   (board-from-string
                ".O
                 O." 2)}
     [1 1])
    [:eye
     {:board   (board-from-string
                ".O
                 O.
                " 2)
      :player  :white
      :history '()}])))


(deftest is-not-eye
  (is
   (=

    (move
     {:history '()
      :player  :white
      :board   (board-from-string
                "O.
                 .." 2)}
     [2 1])

    [:ok
     {:board   (board-from-string
                "OO
                 ..
                " 2)
      :player  :black
      :history (list
                (board-from-string
                 "O.
                  .." 2))}])))


(deftest violates-ko
  (is
   (=
    (move
     {:history (list
                (board-from-string
                 "OO
                  .." 2))
      :player  :white
      :board   (board-from-string
                "O.
                 .." 2)}
     [2 1])
    [:ko
     {:board   (board-from-string
                "O.
                 ..
                " 2)
      :player  :white
      :history (list
                (board-from-string
                 "OO
                  .." 2))}])))


(deftest is-occupied
  (is
   (=

    (move
     {:history (list
                (board-from-string
                 "O.
                  .." 2))
      :player  :black
      :board   (board-from-string
                "O.
                 .." 2)}
     [1 1])
    [:occupied
     {:board   (board-from-string
                "O.
                 ..
                " 2)
      :player  :black
      :history (list
                (board-from-string
                 "O.
                  .." 2))}])))


(deftest possible-moves-1
  (is
   (=
    (possible-moves
     {:history '()
      :player  :black
      :board   (board-from-string
                ".O
                 .O" 2)})

    (list
      {:board   (board-from-string
                 "XO
                  .O
                 " 2)
       :player  :white
       :history (list
                 (board-from-string
                  ".O
                   .O" 2))}
      {:board   (board-from-string
                 ".O
                  XO
                 " 2)
       :player  :white
       :history (list
                 (board-from-string
                  ".O
                   .O" 2))}))))