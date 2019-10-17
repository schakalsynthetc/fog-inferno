
;For example,
(?lc-calc
 ((lambda (x) (x x)) (lambda (y) (y z)))
 (??!lambda (result) (display '(??! result)))
)

;==expands-to==>
;(display '(z z)) 

;As a good test of hygiene, we will try expanding
;   (lambda (a) ((lambda (x) (lambda (a) (x a))) a))
;The result is
;    (lambda
;        (a~2397)
;        (lambda (a~5~2398) (a~2397 a~5~2398)))

;Note that the inner-most application involves differently colored 
;(i.e., distinct) identifiers. To be sure of that, we take advantage of
;the fact that abstractions in our ?lc-calculus look exactly like
;regular Scheme lambda-forms. Therefore, we can use the Scheme evaluator
;to "run" them.

(?lc-calc
 (lambda (a) ((lambda (x) (lambda (a) (x a))) a))
 (??!lambda (result) (write (((??! result) list) 1)))
)

;==expands-to==>
;(write (((lambda
;           (a~2397)
;           (lambda (a~5~2398) (a~2397 a~5~2398)))
;         list)
;        1))
;which, upon evaluation, prints (1). 


;Finally, we try some arithmetics. Let's try to compute a*(a+b) for a=2
;and b=1:

(?lc-calc 
 (	; term to expand, an application of...
  (lambda (a) (lambda (b)  ; lambda-term for a*(a+b)
     (lambda (f) (a (lambda (x) ((b f) ((a f) x)))))))

  (lambda (f) (lambda (x) (f (f x))))	; Numeral 2, for a
  (lambda (f) (lambda (x) (f x)))	; Numeral 1, for b
  )
 (??!lambda (result) (display '(??! result)))
)

after some 2 minutes of compilation and 40000+ reduction steps expands into

(display
  '(lambda
     (f~7~7107)
     (lambda
       (x~21865~24432)
       (f~7~7107
         (f~7~7107
           (f~7~7107
             (f~7~7107 (f~7~7107 (f~7~7107 x~21865~24432)))))))))

which is the Church numeral for 6. Perhaps this is one of the most
expensive ways to compute 2*3. I'm constantly working on enhancing
the complexity.
