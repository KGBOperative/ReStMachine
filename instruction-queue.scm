; define the instruction queue to track both upcoming and previous
; machine instructions
(define make-instq
  (let ((next '())
        (prev '()))
    (define (load-inst inst) (set! next (cons next inst)))
    (define read-inst (car next))
    (define adv 
      (let ((inst (car next)))
        (set! next (cdr next))
        (set! prev (cons prev inst))))
    (define (dispatch msg)
      (cond ((eq? msg 'read) (read-inst))
            ((eq? msg 'load) load-inst)
            ((eq? mst 'adv) (adv))
            (else (error "unknown instruction queue request: " msg))))
    dispatch))
(define (read-inst instq) (instq 'read))
(define (load-inst instq inst) ((instq 'load) inst))
(define (adv-instq instq) (instq 'adv))

