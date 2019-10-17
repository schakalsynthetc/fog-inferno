;From posting-system@google.com Sun Dec 16 16:14:09 2001
;Date: Sun, 16 Dec 2001 13:14:02 -0800
;From: oleg@pobox.com (oleg@pobox.com)
;Newsgroups: comp.lang.functional,comp.lang.scheme
;Subject: Re-writing abstractions, or Lambda: the ultimate pattern macro
;Message-ID: <7eb8ac3e.0112161314.5b86b826@posting.google.com>


(define-syntax ??!apply
  (syntax-rules (??!lambda)
    ((_ (??!lambda (bound-var . other-bound-vars) body)
	oval . other-ovals)
     (letrec-syntax
	 ((subs
	   (syntax-rules (??! bound-var ??!lambda)
	     ((_ val k (??! bound-var))
	      (appl k val))
	     ; check if bound-var is shadowed in int-body
	     ((_ val k (??!lambda bvars int-body))
	      (subs-in-lambda val bvars (k bvars) int-body))
	     ((_ val k (x))	; optimize single-elem list substitution
	      (subs val (recon-pair val k ()) x))
	     ((_ val k (x . y))
	      (subs val (subsed-cdr val k x) y))
	     ((_ val k x)	; x is an id other than bound-var, or number&c
	      (appl k x))))
	  (subsed-cdr		; we've done the subs in the cdr of a pair
	   (syntax-rules ()     ; now do the subs in the car
	     ((_ val k x new-y)
	      (subs val (recon-pair val k new-y) x))))
	  (recon-pair		; reconstruct the pair out of substituted comp
	   (syntax-rules ()
	     ((_ val k new-y new-x)
	      (appl k (new-x . new-y)))))
	  (subs-in-lambda ; substitute inside the lambda form
	   (syntax-rules (bound-var)
	     ((_ val () kp  int-body)
	      (subs val (recon-l kp ()) int-body))
	      ; bound-var is shadowed in the int-body: no subs
             ((_ val (bound-var . obvars) (k bvars) int-body)
	      (appl k (??!lambda bvars int-body)))
             ((_ val (obvar . obvars) kp int-body)
	      (subs-in-lambda val obvars kp int-body))))
	  (recon-l	; reconstruct lambda from the substituted body
	   (syntax-rules ()
	     ((_ (k bvars) () result)
	      (appl k (??!lambda bvars result)))))
	  (appl		; apply the continuation
	   (syntax-rules ()	; add the result to the end of k
	     ((_ (a b c d) result)
	      (a b c d result))
	     ((_ (a b c) result)
	      (a b c result))))
	  (finish
	   (syntax-rules ()
	     ((_ () () exp)
	      exp)
	     ((_ rem-bvars rem-ovals exps)
	      (??!apply (??!lambda rem-bvars exps) . rem-ovals))))
	  )
       ; In the following, finish is the continuation...
       (subs oval (finish other-bound-vars other-ovals) body)))))


;First we introduce a few utility re-writing rules:

; cons in CPS
; ?cons A LST K
; passes (A . LST) to K

(define-syntax ?cons
  (syntax-rules ()
    ((_ x y k)
     (??!apply k (x . y)))))

; append in CPS
; ?append LIST1 LIST2 K
; passes (append LIST1 LIST2) to K

(define-syntax ?append 
  (syntax-rules ()
    ((_ () x k)
     (??!apply k x))
    ((_  x () k)
     (??!apply k x))
    ((_ (x ...) (y ...) k)
     (??!apply k (x ... y ...)))))

; map in CPS
; ?map F (X Y ...) K
; passes ((f X) (f Y) ...) to K
; Here F is a CPS (syntax) function F X KX that passes (f x) to KX

; The algorithm:
; map f () => ()
; map f (x . tail) => (f x . map f tail)

(define-syntax ?map
  (syntax-rules ()
    ((_ f () k)
     (??!apply k ()))
    ((_ f (x . rest) k)
     (f x
      (??!lambda (new-x)
	 (?map f rest
	   (??!lambda (new-rest)
	     (?cons (??! new-x) (??! new-rest) k))))))))

;Note that ?map is an example of a higher-order combinator for
;re-writing rules.

;Now we can implement a hygienic (i.e., avoiding capture of free
;variables) substitutor.

; ?lc-beta VAR TERM VALUE K
; passes to K the result of a hygienic substitution of VALUE
; for VAR in TERM
; The algorithm is a basic induction on the shape of the TERM
; We have to be careful only in two cases, where the TERM is
;  (lambda (var) body) -- in this case var is rebound and the result is
;			  the term itself
;  (lambda (var1) body) -- a direct substitution into the body
;      may capture var1 if it appears free in VALUE.
;      Therefore, we must first alpha-convert the TERM to replace var1
;      with a unique variable. To be more precise, we replace var1
;      with a var1 of a different color. See [3] for generating
;      colored ids.
; Note that alpha-conversion of body uses ?lc-beta itself!
; The algorithm terminates since body is smaller than the original lambda-term

(define-syntax ?lc-beta
  (syntax-rules ()
    ((_ var oterm oval ok)
     (letrec-syntax
	 ((subs
	   (syntax-rules (lambda var)
	     ((_ var val k)
	      (??!apply k val))
	     ((_ (lambda (var) body) val k) ; var is shadowed; don't rename
	      (??!apply k (lambda (var) body)))
	     ((_ (lambda (var1) body) val k) ; alpha-convert var1 first
	      (let-syntax		; we use lc-beta itself for alpha-conv
		  ((uv
		    (syntax-rules ()
		      ((_ var1- body- val- k-)
		       (?lc-beta var1- body- var1
			  (??!lambda (alpha-converted-body)
			       (subs (??! alpha-converted-body) val-
				  (??!lambda (new-body)
				     (??!apply k-
					(lambda (var1) (??! new-body))))))))
		      )))
		(uv var1 body val k)))
	     ((_ (x . rest) val k)	; beta-reduce an application
	      (subs x val		; reduce the head
		(??!lambda (new-x)
		  (subs rest val	; then reduce the tail
		    (??!lambda (new-rest)	; and re-combine them
	                 (?cons (??! new-x) (??! new-rest) k))))))
	     ((_ var1 val k)		; var1 is distinct from var
	      (??!apply k var1))
	     )))
       (subs oterm oval ok)))))


;We should note the modularity of the macro. Curiously, the
;beta-substitutor uses itself to perform an alpha-conversion.


;Finally, the following re-writing rule is the normal-order lambda-evaluator.
;It tries the leftmost outermost reduction first.
;We rely on the left-associativity of applications:
;If (a b) is not a redex and a is not a pair, we try reducing b
;If (a b) is not a redex and a is a pair (x y), we unfold this
;as (x y b) and repeat.

(define-syntax ?lc-calc
  (syntax-rules (lambda)
		; The top-level is a lambda-term: reduce inside
    ((_ (lambda (var) body) k)
     (?lc-calc body (??!lambda (new-body)
		(??!apply k (lambda (var) (??! new-body))))))
		; The top-most leftmost is a redex
		; Reduce it and retry from the top
    ((_ ((lambda (var) body) exp . rest) k)
     (?lc-beta var body exp
	       (??!lambda (result)
			  (?lc-calc ((??! result) . rest) k))))
		; The topmost is a nested application
		; Unfold using the associativity rule
    ((_ ((x) . rest) k) (?lc-calc (x . rest) k))
    ((_ ((x y . in-rest) . rest) k)
     (?append (x y . in-rest) rest 
       (??!lambda (result)
	  (?lc-calc (??! result) k))))
		; In topmost (x y z ...), x is not an appl and not an abst
		; It is equivalent to ((x y) z ...)
		; Try to reduce y, z, etc. separately
    ((_ (x y . rest) k)
     (?map ?lc-calc (y . rest)
	   (??!lambda (new-args) 
			  (?cons x (??! new-args) k))))
		; The topmost is (x) -- remove the extra parens
    ((_ (term) k) (?lc-calc term k))
    ((_ var k) (??!apply k var))
    ))

;And this is it.
