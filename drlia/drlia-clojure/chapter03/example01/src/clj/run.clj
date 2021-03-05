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
          dec-epsilon]])
  (:require [grid-world :as gw]
            [clojure.java.io :as io])
  (:gen-class))

(def model-file-path "nn.trained")

(def max-steps 100)

(defn finish [board]
  (if (gw/finished? board)
    (reduced board)
    board))

(defn episode-reducer [nn epsilon]
  (fn [board i]
    (let [q-vals           (feed-forward nn (X board))
          action-index     (if (< (rand-proba) epsilon)
                             (rand-int 4)
                             (first (choose q-vals)))
          board-1          (gw/move board (as-action action-index))
          reward           (gw/reward board-1)
          board-1-q-vals   (feed-forward nn (X board-1))
          Q                (calc-Q (second (choose board-1-q-vals))
                                   reward)]
      (.fit nn
            (X board)
            (Y q-vals action-index Q))
      (finish board-1))))

(defn train [model episodes]
  (reduce
   (fn [epsilon i]
     (prn (str "Episode: " i))
     (reduce
      (episode-reducer model epsilon)
      (gw/create-static-board)
      (range max-steps))
     (dec-epsilon epsilon))
   1.0
   (range episodes)))

(defn run
  [nn mode]
  (reduce
   (fn [board _step]
     (let [q-vals               (feed-forward nn (X board))
           action-index         (first (choose q-vals))
           [player :as board-1] (gw/move board (as-action action-index))
           reward               (gw/reward board-1)]
       (prn player)
       (finish board-1)))
   (gw/create-board mode)
   (range max-steps)))

(defn train-new-model [train-episodes]
  (let [nn             (GridWorldModel/create)]
    (.init nn)
    (train nn train-episodes)
    (GridWorldModel/save nn model-file-path)
    nn))

(defn -main
  [& args]
  (run
   (if
     (.exists (io/file model-file-path))
     (do
       (prn "Load existing model")
       (GridWorldModel/restore model-file-path))
     (do (prn "Train new model")
       (train-new-model 1000)))
   :player))
