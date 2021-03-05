(ns go.move
  (:use [go.player]
        [go.board.board]
        [go.utils]))


(defn- eval-situation
  [{board :board player :player :as situation} stone]
  (let [result-board (set-stone situation stone)]
    (if (is-self-capture? (:gostrings result-board) stone)
      [:self-capture situation]
      [:ok
       (-> situation
           (assoc :board result-board)
           (assoc :player (other player))
           (update :history #(conj % board)))])))


(defn- violates-ko [{history :history board :board}]
  (not (empty? (filter #(= % board) history))))


(defn move
  "
  @returns:
  [status situation]
  where status is one of
  :self-capture - if move would result in self capture
  :eye          - if player tries to place stone into an eye
  :ko           - for violation of ko rule
  :ok           - on success
  :occupied     - if there is already a stone on that field
  "
  [situation stone]
  (cond
    (is-stone-placed? situation stone)
    [:occupied situation]
    (is-position-an-eye? situation stone)
    [:eye situation]
    :else
    (let [[status new-situation :as evaled] (eval-situation situation stone)]
      (if (and (= :ok status)
               (violates-ko new-situation))
        [:ko situation]
        evaled))))


(defn possible-moves
  "
  @returns: list of situations
  "
  [{{size :size}           :board
    :as                    situation}]

  (->> (permutations size)
       (map (fn [stone] (move situation stone)))
       (filter (fn [[status situation]] (= :ok status)))
       (map second)))

