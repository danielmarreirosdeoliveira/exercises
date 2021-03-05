


(define (cbrt_a x)
  
  (define (good_enough? guess)
    (< (abs (- (* guess guess guess) x))
       (/ guess 1000)))

  (define (qavrg a b) (/ (+ a b) 3))

  (define (improve guess)
    (qavrg
     (* 2 guess)
     (/ x (* guess guess))))

  (define (cbrt-iter guess)
    (if (good_enough? guess)
	guess
	(cbrt-iter (improve guess))))
    
  (cbrt-iter 1.0))



(define (cbrt_b x)
  
  (define (good_enough? guess previous_guess)
    (< (abs (- guess previous_guess)) (/ guess 1000)))

  (define (qavrg a b) (/ (+ a b) 3))

  (define (improve guess)
    (qavrg
     (* 2 guess)
     (/ x (* guess guess))))

  (define (cbrt-iter guess previous_guess)
    (if (good_enough? guess previous_guess)
	guess
	(cbrt-iter (improve guess) guess)))
    
  (cbrt-iter 1.0 0.0))

