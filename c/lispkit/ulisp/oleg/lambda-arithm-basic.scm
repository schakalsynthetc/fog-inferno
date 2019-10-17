;		      Basic Lambda Arithmetics

; $Id: lambda-arithm-basic.scm,v 2.0 2001/03/30 20:31:32 oleg Exp oleg $

; Basic combinators

(X Define %Y (L f ((L x (f (x x))) (L x (f (x x))))))
(X Define %I (L x x))			; identity

;-------------------------
; Booleans
(X Define %true (L x (L y x)))
(X Define %false (L x (L y y)))

(X Define %and (L p (L q (p q %false))))
(X Define %or (L p (L q (p %true q))))
(X Define %not (L p (p %false %true)))
(X Define %xor (L p (L q (p (q %false %true) q))))
(X Define* %equ (L p (L q (%not (%xor p q))))) 
; another derivation: (X Define %equ (L p (L q (p q (q %false %true)))))

(X expand-shortcuts %equ)
(%equ %true %false)
(%equ %false %false) 

(%and %false q)
(%and q %false)
(%and %true %true)

; In other words, reduction of (%and p q) gives %true if and only if
; both p and q are true. Otherwise the reduction ends up with false,
; just as it is supposed to.
; It is easy to verify that definitions for the other boolean functions
; are consistent with their usual meaning.


; By the way, it should be noted that
(%true a b) ;==> a
(%false a b) ;==> b
; i.e. our functions %true and %false act like the if-selector. The following
; is a genuine if selector, which takes a predicate and branches accordingly.

; Genuine if selector
(X Define %if (L p (L x (L y (p x y)))))
(%if %true a b) ; ==> a
(%if %false a b) ; ==> b

;-------------------------
; Ordered Pairs

(X Define %cons (L x (L y (L p (p x y)))))
(X Define %car (L p (p %true)))
(X Define %cdr (L p (p %false)))

; The definitions above satisfy the axioms for pairs:
(%car (%cons a b)) ; ==> a
(%cdr (%cons a b)) ; ==> b

;-------------------------
; Basic arithmetics

(X Define %c0 (L f (L x x)))		; Church numeral 0
; note %c0 is %false -- just like in C
;(X equal? %c0 %false) ==> #t
(X Define %succ (L c (L f (L x (f (c f x))))))		; Successor


(X Define* %c1 (%succ %c0))
(X Define* %c2 (%succ %c1))
(X Define* %c3 (%succ %c2))
(X Define* %c4 (%succ %c3))


(X expand-shortcuts %c4)
(X equal? (%succ %c0) %c1)	; ==> #f
(X equal?* (%succ %c0) %c1)	; ==> #t

(X Define %add (L c (L d (L f (L x (c f (d f x)))))))	; Add two numerals
(X Define %mul (L c (L d (L f (c (d f))))))		; multiplication

(%add %c1 %c2) 

(X env-print)

(%mul %c1 e)	; Note, e is free in both expressions! Thus
(%mul %c0 e)	; 0 * e ==> 0 for _all_ e, an Algebraic result!

(%mul %c3 %c4)

; Zero checking
; A numeral %cn  is represented by a function (operator) that applies
; any function to its argument exactly n times.
; Given a function (%and %false), zero applications of it to an
; argument %true returns %true, whereas applying the function one or
; several times to the %true argument will give %false. The idea does work:

(X Define* %zero? (L c (c (%and %false) %true)))
(%zero? %c0) ; ==> %true
(%zero? %c1) ; ==> %false
(%zero? (%mul %c0 a)) ; ==> %true regardless of a -- an algebraic result
(%zero? (%succ a)) ;==> %false

; raising m to the power of n means multiplying 1 by m exactly n times.
; In other words, it means applying mult m operator to %c1 n times.

(X Define %expt (L m (L n (n (%mul m) %c1))))
(%expt a %c0) ; ==> %c1
(%expt a %c1) ; ==> a, that is, for any a

;; Random notes
;; c1 true x -> true x
;;	(X equal?* (%c1 %true x) (%true x)) 
;; true \y.x -> \v\y.x
;;	(%true (L y x))
;; cn (\y.x) u -> x  whereas c0 (\y.x) u -> u
;;	(%c3 (L y x) u) ==> x   (%c0 (L y x) u) ==> u
;; In particular, 
;; cn (\y.succ) u -> succ
;;	(%c3 (L y %succ) u) ==> %succ

