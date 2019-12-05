#lang br/quicklang
(require "parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module asm-mod "expander.rkt"
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)


(require brag/support)
(define-lex-abbrev digits (:+ (char-set "0123456789")))
(define-lex-abbrev letters (:+ (char-range #\A #\Z)))

(define (make-tokenizer port)
  (define (next-token)
    (define asm-lexer
      (lexer-srcloc
       ["\n" (token 'NEWLINE #:skip? #t)]
       [(:or whitespace ":") (token lexeme #:skip? #t)]
       [(:or "read" "write") (token 'IO-OP lexeme)]
       [(:or "load" "store") (token 'MEM-OP lexeme)]
       [(:or "inc" "dec") (token 'SINGLE-OP lexeme)]
       [(:or "add" "sub" "move") (token 'DOUBLE-OP lexeme)]
       [(:or "je" "jne" "jg" "jge" "jl" "jle") (token 'JUMPC-OP lexeme)]
       ["jump" (token 'JUMP-OP lexeme)]
       [(:or "$PC" "$SP" "$ACC" "$BR" "$CR" "$DR") (token 'REGISTER lexeme)]
       [(:seq alphabetic (:* (:or alphabetic numeric "$")))
        (token 'ID (string->symbol lexeme))]
       [digits (token 'INTEGER (string->number lexeme))]
       ))
    (asm-lexer port))  
  next-token)