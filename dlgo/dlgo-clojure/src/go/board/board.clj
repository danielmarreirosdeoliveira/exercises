(ns go.board.board
  (:use [clojure.set]
        [go.utils]
        [go.board.gostrings]
        [go.player]))


(defn- make-fresh-gostring [gostrings size stone player]
  {#{stone} {:color player :liberties (calc-liberties gostrings stone size)}})


(defn- add-libs [[gostrings liberties] size]
  (add-liberties gostrings liberties size))


(defn- get-connected-gostrings [gostrings size stone player]
  (let [neighbors                         (neighbors stone size)
        adjacent-same-color-gostrings     (gostrings-for-stones gostrings neighbors :color player)
        adjacent-opposite-color-gostrings (gostrings-for-stones gostrings neighbors :color (other player))]
    [adjacent-same-color-gostrings
     adjacent-opposite-color-gostrings]))


(defn- make-gostring-for-player [adjacent-same-color-gostrings gostrings size stone player]
  (merge-gostrings (merge adjacent-same-color-gostrings
                          (make-fresh-gostring gostrings size stone player))))

(defn- calc-set-stone [[adjacent-same-color-gostrings
                        adjacent-opposite-color-gostrings]
                       gostrings
                       size
                       stone
                       player]
  (let [new-gostring-for-player             (make-gostring-for-player adjacent-same-color-gostrings
                                                                      gostrings size stone player)
        all-old-strings-removed             (apply dissoc gostrings
                                                   (flatten
                                                    (list (keys adjacent-same-color-gostrings)
                                                          (keys adjacent-opposite-color-gostrings))))

        [free-gostrings captured-stones]    (remove-liberties adjacent-opposite-color-gostrings stone)
        gostrings-after-stone-placed        (merge all-old-strings-removed
                                                   new-gostring-for-player
                                                   free-gostrings)]
    [gostrings-after-stone-placed captured-stones]))


(defn is-position-an-eye?
  [{player :player {gostrings :gostrings size :size} :board}
    position]
  (let [colors-of-adjacent-gostrings (map :color
                                          (map second (gostrings-for-stones gostrings (neighbors position size))))
        liberties                    (calc-liberties gostrings position size)]
    (and (empty? liberties)
         (every? #(= % player) colors-of-adjacent-gostrings))))


(defn set-stone
  "Returns the new board"
  [{player :player {gostrings :gostrings size :size :as board} :board} stone]
  (assoc board :gostrings
         (-> gostrings
             (get-connected-gostrings size stone player)
             (calc-set-stone gostrings size stone player)
             (add-libs size))))


(defn is-stone-placed?
  [{{gostrings :gostrings} :board} stone]
  (not (empty? (filter #(= % stone) (set (apply concat (map first gostrings)))))))


(defn is-self-capture?
  [gostrings stone]
  (empty? (:liberties (second (gostring-for-stone gostrings stone)))))