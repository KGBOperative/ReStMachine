;; defines the stack along with associated functions for push/pop

(define make-stack
  (let ((stack '()))
    (define (push x) (cons x stack))
    (define (pop) 
      (let ((top (car stack)))
        (if top
            (set! stack (cdr stack))
            top
            (error "cannot pop empty stack"))))
    (define init (set! stack '()))
    (define (action msg)
      (cond ((eq? msg 'push) push)
            ((eq? msg 'pop) (pop))
            ((eq? msg 'init) (init))))
    action))
(define (pop stack) (stack 'pop))
(define (push stack x) (stack 'push x))
(define (init stack) (stack 'init))

