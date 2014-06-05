; generate tokens from the lexemes from the file parser
(define (tokenize lexemes)
  (let ((tokens '()))
    (define (match-token lexeme)
      (cond ((

; create an individual token
(define (make-token token lexeme)
  (list token lexeme))

; token definitions
(define (register? lexeme) (pregexp-match "^r([0-9]$|[1-9][0-9]$)" lexeme))
(define (branch? lexeme) (string=? lexeme "branch"))
(define (openop? lexeme) (string=? lexeme "("))
(define (closeop? lexeme) (string=? lexeme ")"))
(define (openlabel? lexeme) (string=? lexeme "{"))
(define (closelabel? lexeme) (string=? lexeme "{"))
(define (label? lexeme) (string=? (string-head lexeme 1) "@"))
(define (conststr? lexeme) (string=? (string-head lexeme 1) "\""))
(define (constchar? lexeme) (string=? (string-head lexeme 1) "'"))
(define (openop? lexeme) (string=? lexeme "("))
(define (closeop? lexeme) (string=? lexeme ")"))
(define (constnumber? lexeme) (string=? (
