(ns go.agent.alphabeta.abprune-test
  (:use [clojure.test]
        [go.player]
        [go.move]
        [go.agent.alphabeta.abprune]))


(defn next-moves [{player :player}]
  (list {:player (other player)} {:player (other player)}))

(defn eval-fn
  [atm:eval-val]
  (fn [_]
    (let [prev (first @atm:eval-val)]
      (swap! atm:eval-val #(drop 1 %))
      prev)))

(deftest no-next-moves
  (let [eval-val (atom '(10))]
    (is (= 10
           (ab nil (fn [_] '()) (eval-fn eval-val) 9)))))

(deftest simple-maximize-1
  (let [eval-val (atom '(10 13))]
    (is
     (= 13
        (ab {:board {:gostrings #{} :size 3} :player :black} next-moves (eval-fn eval-val) 1)))))

(deftest simple-maximize-2
  (let [eval-val (atom '(13 10))]
    (is
     (= 13
        (ab {:board {:gostrings #{} :size 3} :player :black} next-moves (eval-fn eval-val) 1)))))

(deftest simple-minimize-1
  (let [eval-val (atom '(10 13))]
    (is
     (= 10
        (ab {:board {:gostrings #{} :size 3} :player :white} next-moves (eval-fn eval-val) 1)))))

(deftest simple-minimize-2
  (let [eval-val (atom '(13 10))]
    (is
     (= 10
        (ab {:board {:gostrings #{} :size 3} :player :white} next-moves (eval-fn eval-val) 1)))))

(deftest prune-on-maximize
  (let [eval-val (atom '(13 10 14 15))]
    (is
     (= 13
        (ab {:board {:gostrings #{} :size 3} :player :white} next-moves (eval-fn eval-val) 2)))
    (is (= (first @eval-val) 15))))

(deftest prune-on-minimize
  (let [eval-val (atom '(9 10 8 7))]
    (is
     (= 9
        (ab {:board {:gostrings #{} :size 3} :player :black} next-moves (eval-fn eval-val) 2)))
    (is (= (first @eval-val) 7))))

(deftest intented-use-case
  (let [eval-val (atom '(13 10 14 14 17))]
    (is
     (=
      13
      (ab {:board  {:gostrings {#{[2 1]} {:liberties #{[1 1] [1 2] [2 2]} :color :black}}
                    :size      2}
           :player :white}
          possible-moves
          (eval-fn eval-val)
          2)))
    (is (= (first @eval-val) 17))))

