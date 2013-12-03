#lang racket

(define (sum-to-n n)
  (cond
   ((zero? n) 0)
   (else
    (+ n (sum-to-n (- n 1))))))

;;; Calculate n choose r, for 0 <= n, 0 <= r <= n
(define (combination n r)
  (cond
   ((zero? r) 1)
   ((= n r) 1)
   (else
    (+ (combination (- n 1) (- r 1))
       (combination (- n 1) r)))))

;;; Return the sum of all numbers in a list
(define (sum lst)
  (cond
   ((null? lst) 0)
   (else
    (+ (car lst) (sum (cdr lst))))))

;;; Return whether element a is contained in list lst
(define (member? a lst)
  (cond
   ((null? lst) #f)
   ((equal? a (car lst)) #t)
   (else
    (member? a (cdr lst)))))

;;; Return a list with the *first* instance of element a removed if present
(define (rember lst a)
  (cond
   ((null? lst) '())
   ((equal? a (car lst)) (cdr lst))
   (else
    (cons (car lst) (rember (cdr lst) a)))))

;;; Return whether element a is contained in any level of S-expression lst
(define (member?* a lst) 
(cond
  (null?lst)#f)
(equal? a(car lst) #t)
(list? (car lst) (member?* a (car lst)))
(else
  (member?* a (cdr lst))
)

;;; Return the intersection of sets set1 and set2
;;; set1 and set2 are guaranteed not to contain duplicate elements
(define (intersect set1 set2) 
(define (intersect-helper set1 set2 answer)
(cond
((if(and (null? set1) (null? set2)))answer) 
((member?* car(set1) set2) (cons(answer car(set1))))
((member?* car(set2) set1) (cons(answer car(set2))))
(intersect-helper cdr(set1) cdr(set2) answer)
)
)
(intersect-helper set1 set2 '())
)

;;; Return whether the list contains two equal adjacent elements
(define (two-in-a-row? lst) 
(cond
((equal? car(lst) car(cdr(lst)))#t)
((null?lst)#f)
(else(two-in-a-row?cdr(lst)))
)

;;; Return the nth element of a list
(define (nth lst n) 
(cond

((equal? n 0) car(lst)))

((null?lst) "Index out of range"))

(else(nth cdr(lst) n-1))
)

;;; Return a list containing the unique elements of lst
(define (dedup lst) 
(cond
  ((member?* car(lst) cdr(lst)) (dedup cdr(lst)))

(else(cons(car(lst) (dedup lst))))
)

;;; Return a list containing the elements of lst in reverse order
(define (reverse lst) 
(define (reverse-helper lst rev)
(cond
((null?lst)rev)
(else
(reverse-helper(cdr lst) (cons(car lst)rev)))))
(reverse-helper lst '()))



(provide
 sum-to-n
 combination
 sum
 rember
 member?
 member?*
 intersect
 two-in-a-row?
 nth
 dedup
 reverse)

