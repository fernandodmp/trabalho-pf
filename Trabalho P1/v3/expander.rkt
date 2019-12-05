#lang br/quicklang

(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))
(provide (rename-out [bf-module-begin #%module-begin]))

(define (fold-funcs apl bf-funcs)
  (for/fold ([current-apl apl])
            ([bf-func (in-list bf-funcs)])
    (if (list? bf-func)
        (apply (first bf-func) current-apl #:N (second bf-func))
        (apply bf-func current-apl))
    ))

(define-macro (bf-program OP-OR-LOOP-ARG ...)
  #'(begin
      (define first-apl (list (make-vector 30000 0) 0))
      (void (fold-funcs first-apl (list OP-OR-LOOP-ARG ...)))))
(provide bf-program)

(define-macro (bf-loop "begin" OP-OR-LOOP-ARG ... "end")
  #'(lambda (arr ptr)
      (for/fold ([current-apl (list arr ptr)])
                ([i (in-naturals)]
                 #:break (zero? (apply current-byte
                                       current-apl)))
        (fold-funcs current-apl (list OP-OR-LOOP-ARG ...)))))
(provide bf-loop)

(define-macro-cases bf-expr
  [(bf-expr "fwd") #'gt]
  [(bf-expr "rwd") #'lt]
  [(bf-expr "inc") #'plus]
  [(bf-expr "dec") #'minus]
  [(bf-expr "fwd" N) #'(list gt N)]
  [(bf-expr "rwd" N) #'(list lt N)]
  [(bf-expr "inc" N) #'(list plus N)]
  [(bf-expr "dec" N) #'(list minus N)]
  [(bf-expr "write") #'period]
  [(bf-expr "read") #'comma])
(provide bf-expr)

(define (current-byte arr ptr) (vector-ref arr ptr))

(define (set-current-byte arr ptr val)
  (define new-arr (vector-copy arr))
  (vector-set! new-arr ptr val)
  new-arr)

(define (gt arr ptr #:N [N 1])
  (list arr (+ ptr N)))

(define (lt arr ptr #:N [N 1])
  (list arr (- ptr N)))

(define (plus arr ptr #:N [N 1])
  (list
   (set-current-byte arr ptr (+ (current-byte arr ptr) N))
   ptr))

(define (minus arr ptr #:N [N 1])
  (list
   (set-current-byte arr ptr (- (current-byte arr ptr) N))
   ptr))

(define (period arr ptr)
  (write-byte (current-byte arr ptr))
  (list arr ptr))

(define (comma arr ptr)
  (list (set-current-byte arr ptr (read-byte)) ptr))
