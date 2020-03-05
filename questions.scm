(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (zip-new pairs)
  (if
    (null? pairs) nil)
      (cons (map (lambda (pair) (car pair)) pairs) (cons (map (lambda (pair) (cadr pair)) pairs) nil))
      )



(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
    '()
    (cons (list (car lst1) (car lst2))
      (zip (cdr lst1) (cdr lst2))) 
  ) 
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (helperindex len counter)
    (if (= counter 0)
      nil
      (cons (- len counter) (helperindex len (- counter 1)))
    )
  )
  (define newlist (helperindex (length s) (length s)))
  (zip newlist s)
)

  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (define (cons-all first rests)
          (map (lambda (rest) (cons first rest)) rests)
          )
  (cond
    ((null? denoms) nil)
    ((= total (car denoms)) (cons (list (car denoms)) (list-change total (cdr denoms))))
    ((< total (car denoms)) (list-change total (cdr denoms)))
    (else
      (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))

      ))  

  
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
           (cons form (cons params (let-to-lambda body)))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
              (cons (list 'lambda (car (zip-new values)) (let-to-lambda (car body))) (let-to-lambda (cadr (zip-new values))))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
