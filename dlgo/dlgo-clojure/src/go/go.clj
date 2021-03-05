(ns go.go
  (:use [go.player]
        [go.move]
        [go.draw]
        [go.game]
        [go.board.util])
  (:require
    [go.agent.human]
    [go.agent.random]
    [go.agent.alphabeta])
  (:gen-class))

(def size 5)


(def start-board
  (board-from-string
   "..O..
    XOOX.
    .....
    O....
    O....
    " size))


(def start-player :white)


(def start-situation {:player start-player :board start-board :history '()})


(def player-agent-config
  {:white (go.agent.human/agent-move)
   :black (go.agent.alphabeta/agent-move 4)})


(defn -main []
  (game player-agent-config start-situation))

(-main)