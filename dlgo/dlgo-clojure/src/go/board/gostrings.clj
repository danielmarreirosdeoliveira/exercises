(ns go.board.gostrings
  "Gostrings
  and stones

  Gostrings are, if not specified otherwise, a hashmap.
  Example
   {#{[1 1]} {:liberties #{}}
    #{[2 2]} {:liberties #{}}}
  The keys of the map are the stones of which the string
  consists.

  In fact we store also the color
  {#{[1 1]} {:color :white :liberties #{}}
  but from the perspective of the functions in
  this file this we on purpose behave agnostically in
  regard to props apart from :liberties.
  "
  (:use [clojure.set]
        [go.utils]))


(defn neighbors
  "Returns the neighbor of the given stone as list"
  [[x y :as stone] size]
  (->>
   (list [(- x 1) y]
         [(+ x 1) y]
         [x (- y 1)]
         [x (+ y 1)])
   (filter
    (fn [[x y]]
      (and (> x 0) (> y 0) (<= x size) (<= y size))))))


(defn neighbors-of-stones
  "Returns the neighbors of all given stones as list"
  [stones size]
  (difference
   (set (apply concat (map #(neighbors % size) stones)))
   stones))


(defn- gostrings-for-stones-
  "Gets the gostrings the given stones are part of.
  Returns gostrings as list of vectors."
  [gostrings stones]
  (->> gostrings
       (map
        (fn [[_stones_ props]]
          (let [diff (intersection _stones_ (set stones))]
            (if (empty? diff) nil [_stones_ props]))))
       (remove nil?)))


(defn gostrings-for-stones
  "Tests which aronwa are part of existing gostrings.
  Optionally one can contrain properties which have to match
  Returns gostrings.
  "
  ([gostrings stones]
   (gostrings-for-stones- gostrings stones))
  ([gostrings stones prop-key prop-val]
   (->> (gostrings-for-stones- gostrings stones)
        (filter #(= prop-val (prop-key (second %))))
        (into {}))))


(defn gostring-for-stone
  "Gets the gostring the given stone is part of, if any.
  Returns gostring as vectors, or nil if not found."
  [gostrings stone]
  (let [found-gostrings (gostrings-for-stones-
                         gostrings (list stone))]
    (if (empty? found-gostrings) nil (first found-gostrings))))


(defn remove-liberties [gostrings stone]
  "Going through gostrings, removes the liberties
  where the stone is placed.
  Returns a pair with the gostrings with remaining liberties as left
  and the freed up positions as right element."
  (let [result
        (->>
         gostrings
         (map
          (fn [[stones props]]
            [stones (update props :liberties #(difference % #{stone}))])))
        [free-strings captured-strings]
        (pair-by-predicate (fn [[_ props]] (not (empty? (:liberties props)))) result)]
    [(into {} free-strings) (apply union (map first captured-strings))]))


(defn- add-liberties-reducer [liberties size]
  (fn [gostrings [stones props]]
    (update-in gostrings [stones :liberties]
               #(union
                 (intersection liberties (set (neighbors-of-stones stones size)))
                 %))))

(defn add-liberties [gostrings liberties size]
  (into
   {}
   (reduce
    (add-liberties-reducer liberties size)
    gostrings
    (gostrings-for-stones gostrings (neighbors-of-stones liberties size)))))


(defn- merge-two-gostrings [[left-stones left-props] [rights-stones right-props]]
  (let [merged-stones     (union left-stones rights-stones)
        merged-liberties  (difference
                           (union
                            (:liberties left-props)
                            (:liberties right-props))
                           merged-stones)
        merged-props      (merge left-props right-props)]
    [merged-stones (assoc merged-props :liberties merged-liberties)]))


(defn merge-gostrings
  "Merges gostrings
  into a single gostring.
  Returns gostrings as hash-map."
  [gostrings]
  (apply hash-map (reduce merge-two-gostrings gostrings)))


(defn calc-liberties [gostring stone size]
  (set
   (filter
    #(empty?
      (filter (fn [[stones]] (not (empty? (intersection stones #{%}))))
              gostring))
    (neighbors stone size))))
