#lang br/quicklang

(define-macro (asm-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))
(provide (rename-out [asm-module-begin #%module-begin]))

(define (run apl asm-funcs)
  (let*
      ([instruction (hash-ref (second apl) "$PC")]
       [func (if (< instruction (vector-length asm-funcs)) (vector-ref asm-funcs instruction) '())]
       [arr (first apl)]
       [rt (second apl)])
    (if (list? func)
        (cond
          [(= (length func) 2) (void (run ((first func) arr rt (second func)) asm-funcs))]
          [(= (length func) 3) (void (run ((first func) arr rt (second func) (third func)) asm-funcs))]
          [(= (length func) 5) (void (run ((first func) arr rt (second func) (third func) (fourth func) (fifth func)) asm-funcs))]
          [else void])
        (run (func arr rt) asm-funcs))))

(define-macro (asm-program OP-OR-LOOP-ARG ...)
  #'(begin
      (define first-apl (list (make-vector 65536 0) (hash "$PC" '0
                               "$SP" '0
                               "$ACC" '0
                               "$BR" '0
                               "$CR" '0
                               "$DR" '0 )))
      (void (run first-apl (vector OP-OR-LOOP-ARG ...)))))
(provide asm-program)

(define-macro (expr TRUE_EXPR)
  #'TRUE_EXPR)
(provide expr)

(define-macro-cases io-expr
  [(io-expr "read") #'read]
  [(io-expr "write") #'print]
  )
(provide io-expr)

(define-macro-cases mem-expr
  [(mem-expr "load") #'load]
  [(mem-expr "store") #'store]
  )
(provide mem-expr)

(define-macro (op-expr TRUE_EXPR)
  #'TRUE_EXPR)
(provide op-expr)

(define-macro-cases single-op
  [(single-op "inc" REGISTER) #'(list inc REGISTER)]
  [(single-op "dec" REGISTER) #'(list dec REGISTER)])
(provide single-op)

(define-macro-cases double-op
  [(double-op "move" REGISTER OPERAND) #'(list move REGISTER OPERAND)]
  [(double-op "add" REGISTER OPERAND) #'(list add REGISTER OPERAND)]
  [(double-op "sub" REGISTER OPERAND) #'(list sub REGISTER OPERAND)])
(provide double-op)

(define-macro (jmp-expr TRUE_EXPR) #'TRUE_EXPR)
(provide jmp-expr)

(define-macro-cases jmp
  [(jmp "jump" N) #'(list jump N)])
(provide jmp)

(define-macro-cases jmpc
  [(jmpc "je" N OPERAND-1 OPERAND-2) #'(list jumpc N = OPERAND-1 OPERAND-2)]
  [(jmpc "jne" N  OPERAND-1 OPERAND-2) #'(list jumpc N (list not =) OPERAND-1 OPERAND-2)]
  [(jmpc "jg" N  OPERAND-1 OPERAND-2) #'(list jumpc N > OPERAND-1 OPERAND-2)]
  [(jmpc "jge" N  OPERAND-1 OPERAND-2) #'(list jumpc N >= OPERAND-1 OPERAND-2)]
  [(jmpc "jl" N  OPERAND-1 OPERAND-2) #'(list jumpc N <= OPERAND-1 OPERAND-2)]
  [(jmpc "jle" N  OPERAND-1 OPERAND-2) #'(list jumpc N < OPERAND-1 OPERAND-2)]
  )
(provide jmpc)

(define (current-byte arr rt)
  (vector-ref arr (hash-ref rt "$SP")))

(define (set-current-byte arr rt val)
  (define new-arr (vector-copy arr))
  (vector-set! new-arr (hash-ref rt "$SP") (hash-ref rt "$ACC"))
  new-arr)

(define (increase-pc rt)
  (define new-rt (hash-copy rt))
  (hash-set! new-rt "$PC" (+ (hash-ref rt "$PC") 1))
  new-rt)

(define (inc arr rt register)
  (define new-rt (hash-copy rt))
  (hash-set! new-rt register (+ (hash-ref rt register) 1))
  (list arr (increase-pc new-rt)))

(define (dec arr rt register)
  (define new-rt (hash-copy rt))
  (hash-set! new-rt register (- (hash-ref rt register) 1))
  (list arr (increase-pc new-rt)))

(define (add arr rt register operand)
  (define new-rt (hash-copy rt))
  (define current-value (hash-ref new-rt register))
  (define result
    (if (number? operand)
        (+ current-value operand)
        (+ current-value (hash-ref rt operand))))
  (hash-set! new-rt register result)
  (list arr (increase-pc new-rt)))

(define (sub arr rt register operand)
  (define new-rt (hash-copy rt))
  (define current-value (hash-ref rt register))
  (define result
    (if (number? operand)
        (- current-value operand)
        (- current-value (hash-ref rt operand))))
  (hash-set! new-rt register result)
  (list arr (increase-pc new-rt)))

(define (move arr rt register operand)
  (define new-rt (hash-copy rt))
  (define result
    (if (number? operand)
        operand
        (hash-ref rt operand)))
  (hash-set! new-rt register result)
  (list arr (increase-pc new-rt)))

(define (load arr rt)
  (define new-rt (hash-copy rt))
  (hash-set! new-rt "$ACC" (current-byte arr rt))
  (list arr (increase-pc new-rt)))

(define (store arr rt)
  (list (set-current-byte arr rt (hash-ref rt "$ACC")) (increase-pc rt)))

(define (print arr rt)
  (write-byte (hash-ref rt "$ACC"))
  (list arr (increase-pc rt)))

(define (read arr rt)
  (define new-rt (hash-copy rt))
  (hash-set! new-rt "$ACC" (read-byte))
  (list arr (increase-pc new-rt)))

(define (jump arr rt N)
  (define new-rt (hash-copy rt))
  (hash-set! new-rt "$PC" N)
  (list arr new-rt))

(define (jumpc arr rt N c op-1 op-2)
  (define new-rt (hash-copy rt))
  (let
      ([op-1-new (if (number? op-1) op-1 (hash-ref rt op-1))]
       [op-2-new (if (number? op-2) op-2 (hash-ref rt op-2))])
    (if (list? c)
        (if (not (eq? op-1-new op-2-new))
            (begin (hash-set! new-rt "$PC" N)
            (list arr new-rt))
            (list arr (increase-pc rt)))
        (if (c op-1-new op-2-new)
            (begin (hash-set! new-rt "$PC" N)
            (list arr new-rt))
            (list arr (increase-pc rt))))
    ))

;:TODO
;logica de recursão
;teste e correção de errors
























