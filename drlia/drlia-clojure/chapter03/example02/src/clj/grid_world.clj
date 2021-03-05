(ns grid-world)

(defn create-static-board
  "all positions will be the same every time"
  []
  [;; Player
    [3 0]
    ;; Goal
    [0 0]
    ;; Pit
    [1 0]
    ;; Wall
    [1 1]])

(defn create-player-board
  "the player position will be different every time"
  []
  (let [player [(rand-int 4) (rand-int 4)]
        goal   [0 0]
        pit    [1 0]
        wall   [1 1]]
    (if (and (not= player pit) (not= player wall) (not= player goal))
      [player goal pit wall]
      (create-player-board))))

(defn create-random-board
  "all positions are random every time"
  []
  (let [size   4
        player [(rand-int size) (rand-int size)]
        goal   [(rand-int size) (rand-int size)]
        pit    [(rand-int size) (rand-int size)]
        wall   [(rand-int size) (rand-int size)]
        board  [player goal pit wall]]
    (if (apply distinct? board)
      board
      (create-random-board))))

(defn create-board
  [mode]
  (cond
    (= mode :player)
    (create-player-board)
    (= mode :random)
    (create-random-board)
    :else
    (create-static-board)))

(defn- make-empty [size]
  (into [] (repeat 4 (into [] (repeat 4 0)))))

(defn- set-item [layer [x y]]
  (assoc-in layer [y x] 1))

(defn display [[player goal pit wall]]
  [[(set-item (make-empty 4) player)]
   [(set-item (make-empty 4) goal)]
   [(set-item (make-empty 4) pit)]
   [(set-item (make-empty 4) wall)]])

(defn reward [[player goal pit]]
  (cond
    (= player goal)
    10.0
    (= player pit)
    -10.0
    :else
    -1.0))

(defn finished? [[player goal pit]]
  (or (= player goal) (= player pit)))

(defn move
  [[[player-x player-y :as player]
    _goal
    _pit
    wall
    :as
    board]
   action]
  (let [new-player
        (cond
          (= action "u")
          [player-x ((if (> player-y 0) dec identity) player-y)]
          (= action "d")
          [player-x ((if (< player-y 3) inc identity) player-y)]
          (= action "l")
          [((if (> player-x 0) dec identity) player-x) player-y]
          (= action "r")
          [((if (< player-x 3) inc identity) player-x) player-y])]
    (if (= new-player wall)
      board
      (assoc board 0 new-player))))
