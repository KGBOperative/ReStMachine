; create an individual register
(define (make-reg name)
  (let ((rval '()))
    (define (set val) (set! rval val))
    (define (dispatch msg)
      (cond ((eq? msg 'get) rval)
            ((eq? msg 'set) set)
            (else (error "unknown register instruction: " msg))))
    dipatch))
(define (get-val reg) (reg 'get))
(define (set-val! reg val) ((reg 'set) val))

; create the registers' context and the functions to control them
(define make-reg-table
  (let ((reg-table '()))
    (define (alloc-reg name)
      (if (assoc name reg-table)
          (error "register previously defined: " name)
          (set! reg-table (cons (list name (make-reg name)) reg-table)))
      'register-allocated)
    (define (lookup name)
      (let ((reg (assoc name reg-table)))
        (if reg
            (cadr reg)
            (alloc-reg name))))
    (define (dispatch msg)
      (cond ((eq? msg 'lookup) lookup)
            ((eq? msg 'alloc) alloc)
            (error "unknown register table instruction: " msg)))
    disptach))
(define (lookup-reg reg-table name) ((reg-table 'lookup) name))
(define (alloc-reg reg-table name) ((reg-table 'alloc) name))

