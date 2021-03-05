


(define (sqrt_a x)
  
  (define (good_enough? guess)
    (< (abs (- (* guess guess) x))
       (/ guess 100000000)))

  (define (avrg a b) (/ (+ a b) 2))

  (define (improve guess)
    (avrg guess (/ x guess)))

  (define (sqrt-iter guess)
    (if (good_enough? guess)
	guess
	(sqrt-iter (improve guess))))
    
  (sqrt-iter 1.0))



(define (sqrt_b x)

  (define (good_enough? guess previous_guess)
    (< (abs (- guess previous_guess)) (/ guess 100000000)))

  (define (avrg a b) (/ (+ a b) 2))
    
  (define (improve guess)
    (avrg guess (/ x guess)))

  (define (sqrt-iter guess previous_guess)
    (if (good_enough? guess previous_guess)
	guess
	(sqrt-iter (improve guess) guess)))
    
  (sqrt-iter 1.0 0.0))

