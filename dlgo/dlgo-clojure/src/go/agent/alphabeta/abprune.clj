(ns go.agent.alphabeta.abprune
  "abprune (with depth prune)"
  (:use [go.player]
        [go.utils]))


(declare ab)


(defn maxi-rule
  [v alpha beta done ab-v]
  #_(prn "maxi" v alpha beta done ab-v)
  (if (>= ab-v beta)
    [ab-v alpha beta true]
    (if (> ab-v v)
      [ab-v ab-v beta false]
      [v alpha beta false])))


(defn mini-rule
  [v alpha beta done ab-v]
  #_(prn "mini" v alpha beta done ab-v)
  (if (<= ab-v alpha)
    [ab-v alpha beta true]
    (if (< ab-v v)
      [ab-v alpha ab-v false]
      [v alpha beta false])))


(defn apply-rule
  [rule fn:get-moves fn:eval depth]
  (fn [[v alpha beta done] possible-move]
    (if done
      [v alpha beta done]
      (rule v alpha beta done
            (ab alpha beta possible-move fn:get-moves fn:eval (dec depth))))))


(def minimize (partial apply-rule mini-rule))

(def maximize (partial apply-rule maxi-rule))


(defn ab
  "
  Given a situation, returns the score
  for the best move for the current player
  in the situation.

  X : alpha (start value -999) : The Maximizer
  O : beta (start value 999): The Minimizer

  @param fn:get-moves board player: returns a list of
          all the possible moves for the given board and player
  "
  ([alpha beta {player :player :as situation} fn:get-moves fn:eval depth]

   (if (= depth 0)
     (fn:eval situation)
     (let [possible-moves (fn:get-moves situation)]
       (if (empty? possible-moves)
         (fn:eval situation)
         (first
           (if (= player :white)
             (reduce (minimize fn:get-moves fn:eval depth)
                     [999 alpha beta false] possible-moves)
             (reduce (maximize fn:get-moves fn:eval depth)
                     [-999 alpha beta false] possible-moves)))))))

  ([situation fn:get-moves fn:eval depth]
   (ab -999 999 situation fn:get-moves fn:eval depth)))