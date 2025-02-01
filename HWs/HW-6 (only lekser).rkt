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

; 1. Лексический анализ
;; <expr>::= "(" <expr>
;;         | ")" <expr>
;;         | "+" <expr>
;;         | "-" <expr>
;;         | "\" <expr>
;;         | "*" <expr>
;;         | "^" <expr>
;;         | <var> <expr>
;;         | <float> <expr>
;;         | <int> <expr>
;;         | <spaces> <expr>
;;         | <empty>
;; <spaces>::=SPACE|SPACE<spaces>
;; <var>::=CHAR|CHAR<var>
;; <float>::=<int>.<int>|<int>e-?<int>
;;          |<int>.<int>e-?<int>
;; <int>::=NUM<int-tail>
;; <int-tail>::=NUM<tail>|<empty>
(define (tokenize str)
  (define stream
    (make-stream (string->list str) (integer->char 0)))
  (call-with-current-continuation
   (lambda (error)
     (let((tokens (scan-expr stream error)))
       (and
        (equal? (peek stream) (integer->char 0))
        tokens)
       )))
  )
(define (scan-expr stream error)
  (cond
    ((char-whitespace? (peek stream)) (skip-space stream)
                                      (scan-expr stream error))
    ((or
      (equal? (peek stream) #\()
      (equal? (peek stream) #\)))
     (append
      (list (string(next stream)))
      (scan-expr stream error)
      )
     )
    ((or 
      (equal? (peek stream) #\+)
      (equal? (peek stream) #\-)
      (equal? (peek stream) #\/)
      (equal? (peek stream) #\*)
      (equal? (peek stream) #\^))
     (append
      (list (string->symbol(string(next stream))))
      (scan-expr stream error)
      ))
    ((char-numeric? (peek stream))
     (append
      (list (scan-number stream error))
      (scan-expr stream error)
      )
     )
    ((char-alphabetic? (peek stream))
     (append
      (list (var stream error))
      (scan-expr stream error)
      ))
    (else '())
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
(define (scan-number stream error)
  (cond
    ((char-numeric? (peek stream)) 
     (string->number (string-append
                      (scan-int stream error)
                      (cond
                        ((equal? (peek stream) #\.)
                         (string-append
                          (string (next stream))
                          (scan-int stream error)
                          (cond
                            ((equal? (peek stream) #\e)
                             (string-append
                              (string (next stream))
                              (cond
                                ((equal? (peek stream) #\-) (string (next stream)))
                                (else "")
                                )
                              (scan-int stream error)
                              ))
                            (else "")
                            )))
                        ((equal? (peek stream) #\e)
                         (string-append
                          (string (next stream))
                          (cond
                            ((equal? (peek stream) #\-) (string (next stream)))
                            (else "")
                            )
                          (scan-int stream error)
                          )
                         )
                        (else "")
                        )
                      (cond
                        ((or
                         (equal? (peek stream) #\+)
                         (equal? (peek stream) #\-)
                         (equal? (peek stream) #\\)
                         (equal? (peek stream) #\*)
                         (equal? (peek stream) #\^)
                         (equal? (peek stream) #\()
                         (equal? (peek stream) #\))
                         (char-whitespace? (peek stream))
                         (equal? (peek stream) (integer->char 0))
                         )
                         "")
                        (else (error #f))
                        )
                      )
                     ))
    (else (error #f))
    )
  )

(define (scan-int stream error)
  (cond
    ((char-numeric? (peek stream))
     (string-append (string (next stream))
                    (scan-tail stream error)))
    (else (error #f))
    )
  )
(define (scan-tail stream error)
  (cond
    ((char-numeric? (peek stream)) 
     (string-append (string (next stream))
                    (scan-tail stream error)))
    (else "")
    )
  )
(define (var stream error)
  (cond
    ((char-alphabetic? (peek stream))
     (string->symbol(string-append
                     (string(next stream))
                     (var-tail stream error))))
    (else (error #f))
    )
  )
(define (var-tail stream error)
  (cond
    ((char-alphabetic? (peek stream))
     (string-append
      (string (next stream))
      (var-tail stream error))
     )
    (else "")
    )
  )
