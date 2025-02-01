(load "LR-3.rkt")
;;Задание 1
;; Конструктор потока
(define (make-stream items . eos)
  (if (null? eos)
      (make-stream items #f)
      (list items (car eos))))
;; Запрос текущего символа
(define (peek stream)
  (if (null? (car stream))
      (cadr stream)
      (caar stream)))
;; Продвижение вперёд
(define (next stream)
  (let ((n (peek stream)))
    (if (not (null? (car stream)))
        (set-car! stream (cdr (car stream))))
    n))
;Предикат 1
;<float>::=<sign><int>.<int>
;<sign>::=+|-|<empty>
;<int>::=DIGIT<tail>
;<tail>::DIGIT<tail>|<empty>
;<empty>::=
(define (valid-dec? str)
  (define stream
    (make-stream (string->list str) (integer->char 0)))
  (call-with-current-continuation
   (lambda (error)
     (and (float? stream error)
          (equal? (peek stream) (integer->char 0)))))
  )
(define (float? stream error)
  (cond
    ((sign? (peek stream)) (next stream)  
						                     (and
                                (int? stream error)
                                (equal? (peek stream) #\.)
                                (begin
                                    (next stream)
                                    (int? stream error)
                                    )
                                ))
    ((char-numeric? (peek stream)) (and
                                    (int? stream error)
                                    (equal? (peek stream) #\.)
                                    (begin
                                      (next stream)
                                      (int? stream error)
                                      )))
    (else (error #f))
    )
  )
(define (sign? symb)
  (or (equal? symb #\+) (equal? symb #\-))
  )
(define (int? stream error)
  (cond
    ((char-numeric? (peek stream)) (next stream) 
				(tail? stream error))
    (else (error #f))
    )
  )
(define (tail? stream error)
  (cond
    ((char-numeric? (peek stream)) (next stream) 
				(tail? stream error))
    (else '())
    )
  )
;;Предикат 2
;<list>::=<spaces><float><list-tail>
;         |<float><list-tail>
;         |<empty>
;<list-tail>::=<spaces><float><list-tail>
;             |<sep><int>.<int><list-tail>
;             |<empty>
;<sep>::=+|-
;<empty>::=
;<spaces>::=SPACE<tail-sp>
;<tail-sp>::=SPACE<tail-sp>|<empty>
(define (valid-many-decs? str)
  (define stream
    (make-stream (string->list str) (integer->char 0)))
  (call-with-current-continuation
   (lambda (error)
     (and (list-of-float? stream error)
          (equal? (peek stream) (integer->char 0)))))
  )
(define (list-of-float? stream error)
  (cond
    ((char-whitespace? (peek stream)) (skip-spaces stream) 
      (list-of-float? stream error))
    ((float? stream error) (list-tail? stream error))
    (else (error #f))
    )
  )
(define (list-tail? stream error)
  (cond
    ((char-whitespace? (peek stream)) (skip-spaces stream) 
      (or
        (equal? (peek stream) (integer->char 0))
        (and
            (float? stream error)
            (list-tail? stream error))))
    ((sep? (peek stream)) (next stream) 
      (and
        (int? stream error)
        (equal? (peek stream) #\.)
        (begin
	        (next stream)
            (int? stream error)
            (list-tail? stream error)
            )))
    (else '())
    )
  )
(define (sep? sym)
  (or (equal? sym #\+) (equal? sym #\-))
  )
(define (skip-spaces stream)
  (if(char-whitespace? (peek stream))
     (begin
       (next stream)
       (skip-spaces stream)
       )
     )
  )

(define the-tests
  (list (test (valid-dec? "7234.4") #t)
        (test (valid-dec? "+07.70") #t)
        (test (valid-dec? "-4.111") #t)
        (test (valid-dec? "+.7777") #f)
        (test (valid-dec? "5555.") #f)
        (test (valid-dec? "10") #f)
        (test (valid-many-decs? "\t1.8 -2.4\n\n5.0\t") #t)
        (test (valid-many-decs? "\t1.8 -2.4\n\n5.-") #f)
        (test (valid-many-decs? "-568.3+77.1") #t)
        (test (valid-many-decs? " 21") #f)
        ))
(run-tests the-tests)

;;Сканер 1
;<float>::=<sign><int>.<drob>
;<sign>::=+|-|<empty>
;<int>::=DIGIT<tail>
;<drob>::=DIGIT<tail>
;<tail>::DIGIT<tail>|<empty>
;<empty>::=
(define (scan-dec str)
  (define stream
    (make-stream (string->list str) (integer->char 0)))
  (call-with-current-continuation
   (lambda (error)
     (let((num (scan-float stream error)))
       (and 
        (equal? (peek stream) (integer->char 0))
        num
        )
       )
     ))
  )
(define (scan-float stream error)
  (cond
    ((equal? (peek stream) #\-) (next stream) 
      (exact->inexact (* -1 (+ (scan-int stream error)
		                       (scan-point stream error) 
                         (scan-drob stream error)))))
    ((equal? (peek stream) #\+) (next stream) 
      (exact->inexact (+ (scan-int stream error)
                      (scan-point stream error)
                         (scan-drob stream error)
                         )))
    ((char-numeric? (peek stream)) 
      (exact->inexact (+ (scan-int stream error)
                         (scan-point stream error)
                         (scan-drob stream error)
                         )))
    (else (error #f))
    )
  )
(define (scan-drob stream error)
  (cond
    ((char-numeric? (peek stream)) 
       (scan-drob-tail stream error 
         (* (char->int (next stream)) (expt 10 -1)) -2))
    (else (error #f))
    )
  )
(define (scan-drob-tail stream error num s)
  (cond
    ((char-numeric? (peek stream))
     (scan-drob-tail stream error 
       (+ num (* (expt 10 s) 
               (char->int (next stream)))) (- s 1)))
    (else num)
    )
  )
(define (scan-point stream error)
  (cond
    ((equal? (peek stream) #\.) (next stream) 0)
    (else (error #f))
    )
  )
(define (scan-int stream error)
  (cond
    ((char-numeric? (peek stream)) 
      (scan-tail stream error (char->int (next stream))))
    (else (error #f))
    )
  )
(define (scan-tail stream error num)
  (cond
    ((char-numeric? (peek stream)) 
      (scan-tail stream error (+ (* num 10) 
        (char->int (next stream)))))
    (else num)
    )
  )
(define (char->int symb)
  (- (char->integer symb) (char->integer #\0))
  )
;;Сканер 2
;<list>::=<spaces><float><list-tail>
;         |<float><list-tail>
;         |<empty>
;<list-tail>::=<spaces><float><list-tail>
;             |<sep><int>.<int><list-tail>
;             |<empty>
;<sep>::=+|-
;<empty>::=
;<spaces>::=SPACE<tail-sp>
;<tail-sp>::=SPACE<tail-sp>|<empty>
(define (scan-many-decs str)
  (define stream
    (make-stream (string->list str) (integer->char 0)))
  (call-with-current-continuation
   (lambda (error)
     (let((l (scan-list-float stream error)))
       (and
        (equal? (peek stream) (integer->char 0))
        l
        )
       )))
  )
(define (scan-list-float stream error)
  (if(char-whitespace? (peek stream))
     (skip-space stream))
  (cons
   (scan-float stream error)
   (cond
     ((char-whitespace? (peek stream)) 
     (skip-space stream)
        (cond
            ((equal? (peek stream) (integer->char 0)) '())
        (else (scan-list-float stream error)))
            )
     ((or (char-numeric? (peek stream))
          (equal? (peek stream) #\+)
          (equal? (peek stream) #\-))
      (scan-list-float stream error))
     (else '())
     )
   )
  )
(define (skip-space stream)
  (if(char-whitespace? (peek stream))
     (begin
       (next stream)
       (skip-space stream)
       )
     )
  )

(define the-tests
  (list (test (scan-dec "7234.4") 7234.4)
        (test (scan-dec "+07.70") 7.7)
        (test (scan-dec "-4.111") -4.111)
        (test (scan-dec "+.7777") #f)
        (test (scan-dec "5555.") #f)
        (test (scan-dec "10") #f)
        (test (scan-many-decs "\t1.8 -2.4\n\n5.0\t") 
	        (1.8 -2.4 5.0))
        (test (scan-many-decs "\t1.8 -2.4\n\n5.-") #f)
        (test (scan-many-decs "-568.3+77.1") (-568.3 77.1))
        ))
(run-tests the-tests)

;;Задание 2
;Предикат
(define (valid? vec)
  (define stream
    (make-stream (vector->list vec) (integer->char 0)))
  (call-with-current-continuation
   (lambda (error)
     (and
      (program? stream error)
      (equal? (peek stream) (integer->char 0))
      )
     ))
  )
(define (program? stream error)
  (cond         
    ((or
      (equal? (peek stream) 'define)
      (equal? (peek stream) 'if)
      (number? (peek stream))
      (symbol? (peek stream))
      )
     (and (articles? stream error)
          (body? stream error)))
    (else (error #f))
    )
  )
(define (articles? stream error)
  (cond
    ((equal? (peek stream) 'define) 
	    (and
            (article? stream error)
            (articles? stream error)
            ))
    (else #t)
    )
  )
(define (article? stream error)
  (cond
    ((equal? (peek stream) 'define) 
	    (next stream)
        (and (symbol? (peek stream))
            (next stream)
            (body? stream error)
            (equal? (peek stream) 'end)))
    (else (error #f))
    )
  )
(define (body? stream error)
  (cond
    ((equal? (peek stream) 'if) 
	    (next stream)
        (and
        (body? stream error)
        (else-part? stream error)
        (equal? (peek stream) 'endif)
        (next stream)
        (or
            (equal? (peek stream) (integer->char 0))
            (body? stream error))
        ))
    ((number? (peek stream)) (next stream) 
	    (body? stream error))
    ((or (equal? (peek stream) 'else)
         (equal? (peek stream) 'endif)
         (equal? (peek stream) 'end))#t)
    ((symbol? (peek stream)) (next stream) 
	    (body? stream error))
    (else #t)
    )
  )
(define (else-part? stream error)
  (cond
    ((equal? (peek stream) 'else) (next stream) 
			(body? stream error))
    (else #t)
    )
  )
;;Парсер
(define (parse vec)
  (define stream
    (make-stream (vector->list vec) (integer->char 0)))
  (call-with-current-continuation
   (lambda(error)
     (let((tokens (scan-program stream error)))
       (and
        (equal? (peek stream) (integer->char 0))
        tokens
        )
       )))
  )
(define (scan-program stream error)
  (cond         
    ((or
      (equal? (peek stream) 'define)
      (equal? (peek stream) 'if)
      (number? (peek stream))
      (symbol? (peek stream))
      )
     (append
      (list(scan-articles stream error))
      (list(scan-body stream error))))
    (else (error #f))
    )
  )
(define (scan-articles stream error)
  (cond
    ((equal? (peek stream) 'define) 
	    (append
            (list(scan-article stream error))
            (scan-articles stream error)
            ))
    (else '())
    )
  )
(define (scan-article stream error)
  (cond
    ((equal? (peek stream) 'define)
     (next stream)
     (append
      (cond
        ((symbol? (peek stream)) (list(next stream)))
        (else (error #f)))
      (list(scan-body stream error))
      (cond
        ((equal? (peek stream) 'end) (next stream) '())
        (else (error #f))
        )))
    (else (error #f))
    )
  )
(define (scan-body stream error)
  (cond
    ((equal? (peek stream) 'if)
     (append
      (list(append (list(next stream))
                   (list(scan-body stream error))
                   (scan-else-part stream error)))
      (cond
        ((equal? (peek stream) 'endif) (next stream) '())
        (else (error #f)))                                
      (scan-body stream error))
     )
    ((number? (peek stream)) (cons (next stream) 
                                (scan-body stream error)))
    ((or (equal? (peek stream) 'else)
         (equal? (peek stream) 'endif)
         (equal? (peek stream) 'end)) '())
    ((symbol? (peek stream)) (cons (next stream) 
                                 (scan-body stream error)))
    (else '())
    )
  )
(define (scan-else-part stream error)
  (cond
    ((equal? (peek stream) 'else) (next stream) 
                (list (scan-body stream error)))
    (else '())
    )
  )
  
;;Тестирование:
(define the-tests
  (list (test (valid? #(1 2 +)) #t)
        (test (valid? #(define 1 2 end)) #f)
        (test (valid? #(define x if end endif)) #f)
        (test (parse #(1 2 +)) (() (1 2 +)))
        (test (parse #(x dup 0 swap if drop -1 endif))
              (() (x dup 0 swap (if (drop -1)))))
        (test (parse #(x dup 0 swap if drop -1 
	        else swap 1 + endif))
              (() (x dup 0 swap (if (drop -1) (swap 1 +)))))
        (test (parse #( define -- 1 - end
                         define =0? dup 0 = end
                         define =1? dup 1 = end
                         define factorial
                         =0? if drop 1 exit endif
                         =1? if drop 1 exit endif
                         dup --
                         factorial
                         *
                         end
                         0 factorial
                         1 factorial
                         2 factorial
                         3 factorial
                         4 factorial ))
              (((-- (1 -))
                (=0? (dup 0 =))
                (=1? (dup 1 =))
                (factorial
                 (=0? (if (drop 1 exit)) =1? (if (drop 1 exit)) 
                 dup -- factorial *)))
               (0 factorial 1 factorial 2 factorial 3 factorial 
               4 factorial))
              )
        (test (parse #( define -- 1 - end
                         define =0? dup 0 = end
                         define =1? dup 1 = end
                         define factorial
                         =0? if
                         drop 1
                         else =1? if
                         drop 1
                         else
                         dup --
                         factorial
                         *
                         endif
                         endif
                         end
                         0 factorial
                         1 factorial
                         2 factorial
                         3 factorial
                         4 factorial ))
              (((-- (1 -))
                (=0? (dup 0 =))
                (=1? (dup 1 =))
                (factorial
                 (=0? (if (drop 1)(=1? (if (drop 1) 
                 (dup -- factorial *)))))))
               (0 factorial 1 factorial 2 factorial 
               3 factorial 4 factorial)))
        (test (parse #(define word w1 w2 w3)) #f)
        ))
(run-tests the-tests)
