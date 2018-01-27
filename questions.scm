(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items) nil
   (cons
      (proc (car items)) ;apply procedure on first element of scheme list
      (map proc (cdr items)) ;recursive call to map on rest of scheme list
      ;returns merged list
    )
  )
)

(define (cons-all first rests)
  (define
    (add-first rest)
    (cons first rest)
    ;helper that attaches first to beginning of scheme list rests
  )
  (map add-first rests) ;applies helper to each element of rests
)

(define (zip pairs)
  ;makes list of separated cars and cadrs of list pairs
  (if (null? pairs)
      nil
      (list (map car pairs) (map cadr pairs))
  )
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17

  (define
    (order lst num)
      (if (null? lst) nil
        (cons
          (cons num
            (cons (car lst) nil)
            ;adds num before first element of lst, creates list of that
          )
          (order (cdr lst) (+ 1 num)) ;recursive call on rest of lst
        )
      )
  )
  (order s 0) ;start with index 0
)

  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
      ((null? denoms) nil)
      ((= 0 total) nil)
      ((= (car denoms) total)
          (cons
            (list (car denoms))
            (list-change total (cdr denoms)) ;recursive call on rest of denoms
          )
          ;creates merged list with first element of denoms and breakdown of rest of denoms
      )
      ((< (car denoms) total)
          (append
            (cons-all
              (car denoms)
              (list-change (- total (car denoms)) denoms) ;subtract total with car denoms to pass into recursive call
            )
            (list-change total (cdr denoms))
          )
          ;when car denoms is smaller than total, attach car denoms with breakdown of total - car denoms and that of cdr denoms
      )
      (else (list-change total (cdr denoms)))
      ;case for when car denoms is bigger than total - ignore car denoms and make recursive call on rest of denoms
  )
)
  ; END PROBLEM 18



;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  ; atoms and quotes are simply returned as themselves
  (cond ((atom? expr)

         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19

         )
        ((quoted? expr)

         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19

         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))

           ; BEGIN PROBLEM 19
          (append
            (list form params) ;make list with word lambda and params
            (let-to-lambda body) ;transform body from let to lambda form recursively
          )
          ;merge lists
           ; END PROBLEM 19

           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))

           ; BEGIN PROBLEM 19
           (cons
              (append
                  (list 'lambda (car (zip (map let-to-lambda values)))) ;list of word lambda with lambda form values
                  (let-to-lambda body) ;lambda form body
              )
              (cadr (zip (map let-to-lambda values))) ;each element of values turned into lambda form
              ;merge the two lists
            )
           ; END PROBLEM 19

           )
          )
        (else

         ; BEGIN PROBLEM 19
         (map let-to-lambda expr) ;in case of scheme list as input, turn each element of list into lambda form 
         ; END PROBLEM 19

         )))
