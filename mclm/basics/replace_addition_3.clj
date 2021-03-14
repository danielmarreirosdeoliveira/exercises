(let [expression '(+ 1 2 3 4 5)
      adjusted (cons '* (rest expression))]
  (prn adjusted)
  (prn (eval adjusted)))

