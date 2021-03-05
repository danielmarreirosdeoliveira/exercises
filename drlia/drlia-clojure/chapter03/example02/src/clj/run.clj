(ns run
  (:import (gridworld GridWorldModel))
  (:use [helper
         :only
         [as-action
          X
          feed-forward
          choose
          calc-Q
          Y
          rand-proba
          dec-epsilon
          select-randomly]])
  (:require [nd :as nd]
            [grid-world :as gw]
            [clojure.java.io :as io])
  (:gen-class))

(def model-file-path "nn.trained")

(defn finish
  ([board]
   (if (gw/finished? board)
     (reduced board)
     board))
  ([board replay]
   (if (gw/finished? board)
     (reduced [board replay])
     [board replay])))

(defn episode-reducer [nn epsilon]
  (fn [[board replay] _i]
    (let [q-vals               (feed-forward nn
                                             (nd/row-array-from-1d-vector (X board)))
          action-index         (if (< (rand-proba) epsilon)
                                 (rand-int 4)
                                 (first (choose q-vals)))
          board-1              (gw/move board (as-action action-index))
          reward               (gw/reward board-1)
          board-1-q-vals       (feed-forward nn
                                             (nd/row-array-from-1d-vector (X board-1)))
          Q                    (calc-Q (second (choose board-1-q-vals))
                                       reward)]
      (finish board-1
              (conj replay
                    [(X board) (Y q-vals action-index Q)])))))

(defn train [nn episodes max-steps-train mode]
  (reduce
   (fn [epsilon i]
     (prn (str "Episode: " i))
     (let [[X Y]  (->>
                    (reduce
                     (episode-reducer nn epsilon)
                     [(gw/create-board mode) '()]
                     (range max-steps-train))
                    (second)
                    (reverse)
                    (select-randomly)
                    (apply mapv vector))
           [X Y]   [(nd/row-vectors-to-nd X) (nd/row-vectors-to-nd Y)]]
       (GridWorldModel/fit nn X Y))
     (dec-epsilon epsilon episodes))
   1.0
   (range episodes)))

(defn run
  [nn max-steps mode]
  (let [[player goal :as board] (gw/create-board mode)]
    (prn (str "Player: " player ", Goal: " goal))
    (reduce
     (fn [board _i]
       (let [q-vals               (feed-forward nn (nd/row-array-from-1d-vector (X board)))
             action-index         (first (choose q-vals))
             [player :as board-1] (gw/move board (as-action action-index))
             reward               (gw/reward board-1)]
         (prn player)
         (finish board-1)))
     board
     (range max-steps))))

(defn train-new-model [train-episodes max-steps-train mode]
  (let [nn             (GridWorldModel/create)]
    (GridWorldModel/init nn)
    (train nn train-episodes max-steps-train mode)
    (GridWorldModel/save nn model-file-path)
    nn))


;; ----------------------------------------------------------------- ;;

(def max-steps-train 80)
(def max-steps 10)
(def train-episodes 10000)

(defn -main
  [& args]
  (run
   (if
     (.exists (io/file model-file-path))
     (do (prn "Load existing model")
       (GridWorldModel/restore model-file-path))
     (do (prn "Train new model")
       (train-new-model train-episodes max-steps-train :random)))
   max-steps
   :random))
