#lang br/quicklang
(require "parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module bf-mod "expander.rkt"
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)


(require brag/support)
(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define (make-tokenizer port)
  (define (next-token)
    (define bf-lexer
      (lexer-srcloc
       [(:or "\n" whitespace) (token lexeme #:skip? #t)]
       [(:or "fwd" "rwd" "inc" "dec" "read" "write") (token 'OPERAND lexeme)]
       [(:or "begin" "end") (token lexeme lexeme)]
       [digits (token 'INTEGER (string->number lexeme))]
       ))
    (bf-lexer port))  
  next-token)