(ns go.agent.human
  (:use [go.utils]
        [go.draw]
        [go.move]))


(def timeout 1100)


(defn- hit-eye []
  (println ":eye")
  (Thread/sleep timeout)
  (cursor-left-side)
  (kill-line)
  (cursor-up)
  (kill-line)
  (cursor-up)
  (kill-line)
  (cursor-left-side))

;; todo review, why do we pass fn:move, instead of using move directly?
(defn agent-move
  []
  (fn inner
    ([situation skip-prompt]
     (if (not skip-prompt)
       (print "Human, enter coords like '3 2' for (3,2) = (x,y) or 'p' for pass or 'r' for resign\n"))
     (flush)
     (let [answer      (read-line)]
       (cond
         (= (first answer) \p) {:status :pass}
         (= (first answer) \r) {:status :resign}
         :else                 (let [[x y]       (clojure.string/split answer #" ")
                                     result      (move situation [(Integer/parseInt x) (Integer/parseInt y)])]
                                 (if (= :eye (first result))
                                   (do
                                     (hit-eye)
                                     (inner situation true))
                                   result)))))
    ([situation]
     (inner situation false))))