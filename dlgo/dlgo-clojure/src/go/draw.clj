(ns go.draw
  (:use [go.board.gostrings]))


(defn clear-screen
  []
  (print (str (char 27) "[2J"))
  (print (str (char 27) "[;H")))


(defn kill-line []
  (print (str (char 27) "[K")))


(defn cursor-up []
  (print (str (char 27) "[1A")))


(defn cursor-left-side []
  (print "\r"))


(defn- make-grid [size]
  (into []
        (replicate size
                   (into [] (replicate size " . ")))))


(defn- calc-gostrings [gostrings size]
  (let [grid (make-grid size)]
    (reduce
     (fn [grid i]
       (let [y (mod i size)
             x (quot i size)]
         (if-let [[_ props] (gostring-for-stone gostrings [(inc x) (inc y)])]
           (assoc-in grid [y x]
                     (if (= :black (:color props)) " X " " O "))
           grid)))
     grid
     (range (* size size)))))


(defn draw-gostrings [gostrings size]
  (assert (<= size 9) "Drawing of boards bigger than 9 not supported")
  (println " " (apply str (interpose "  " (clojure.string/join (range 1 (inc size))))))
  (doall
   (for [r (map-indexed (fn [idx itm] (apply str (inc idx) itm))
                        (calc-gostrings gostrings size))]
     (println r)))
  nil)

