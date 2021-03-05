					; Pascals Triangle


(define (ptri h p)
  (cond ((< h 1) 0)
        ((> p h) 0)
	((< p 1) 0)
	((= p 1) 1)
	((= p h) 1)
	(else (+ (ptri (- h 1) (- p 1))
     		 (ptri (- h 1) p)
		 ))))
  
