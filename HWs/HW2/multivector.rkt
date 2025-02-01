(define (make-multi-vector sizes . fill)
  (if(null? fill)
     (list->vector(append (list sizes) (vector->list(make-vector
                                                    (apply * sizes)))))
     (list->vector(append (list sizes) (vector->list(make-vector
                                        (apply * sizes) (car fill)))
     ))
  )
  )

(define (multi-vector? m)
  (and (vector? m) (list? (car (vector->list m)))
       (= (apply * (car (vector->list m))) (- (vector-length m) 1)))
  )

(define (multi-vector-ref m indicices)
  (define sizes (car (vector->list m)))
  (define (convert xs ys itog)
    (if(null? xs)
       itog
       (convert (cdr xs) (cdr ys) (+ itog (*(car ys) (/ (apply * xs)
                                                        (car xs)))))
       )
    )
  (vector-ref m (+ (convert sizes indicices 0) 1))
  )

(define (multi-vector-set! m indicices new_el)
  (define sizes (car (vector->list m)))
  (define (convert xs ys itog)
    (if(null? xs)
       itog
       (convert (cdr xs) (cdr ys) (+ itog (*(car ys) (/ (apply * xs)
                                                        (car xs)))))
       )
    )
  (vector-set! m (+ (convert sizes indicices 0) 1) new_el)
  )
