

(define (f1 n)
  (if (< n 3)
      n
      (+ (f1 (- n 1))
	 (* 2 (f1 (- n 2)))
	 (* 3 (f1 (- n 3))))))


(define (f2 n)
  (define (f2_ i p1 p2 p3)
    (if (> i n)
      p1
      (f2_ (+ i 1)
	   (+ (if (< i 3)
		  i
		  p1)
	      (* 2 p2)
	      (* 3 p3)
	      )
	   p1
	   p2)))
  (f2_ 0 0 0 0))


(define (f3 n)
  (define (f3_ i p1 p2 p3)
    (if (< i 3)
	p1
	(f3_ (- i 1)
	     (+ p1 (* 2 p2) (* 3 p3))
	     p1
	     p2)))
  (if (< n 3) n (f3_ n 2 1 0)))





