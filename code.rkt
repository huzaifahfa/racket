#lang racket
; Name: Huzaifah Bin Fakhrul Anuar
; PSU
; question 3
(define (merge firstlst secondlst)
  (cond ((null? firstlst) secondlst)
        ((null? secondlst) firstlst)
        ((>= (car firstlst) (car secondlst))
         (cons (car secondlst) (merge firstlst (cdr secondlst))))
        (else
         (cons (car firstlst) (merge (cdr firstlst) secondlst)))))


; question 4
(define (findMax lst)
  (cond ((null? lst) 0)
        (else
         (let ((rest (findMax (cdr lst))))
           (if (list? (car lst))
               (max (findMax (car lst)) rest)
               (max (car lst) rest))))))


; question 5
(define (depthOfList val)
  (if (list? val)
         (+ 1 (maximumDepth val))
         0))

; helper function for question 5
(define (maximumDepth lst)
  (if (null? lst)
      0
      (max(depthOfList (car lst)) (maximumDepth (cdr lst)))))


; question 6
(define (trunc a b x)
  (map (lambda (xi) (truncCompare xi a b)) x))

; helper function for question 6
(define (truncCompare xi a b)
  (cond ((< xi a) a)
        ((> xi b) b)
        (xi)))
