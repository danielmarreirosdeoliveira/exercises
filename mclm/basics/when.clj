

(defmacro my-when
  "Evaluates test."
  {:added "now"}
  [test & body]
  (list 'if test (cons 'do body)))

(my-when (= 1 1) (prn "yes"))

(eval '(if (= 1 1) (prn "yes")))
