; 			lambda-calculator in Scheme
;
; This calculator implements a normal-order evaluator for untyped
; lambda-calculus with "shortcuts". Shortcuts are distinguished constants
; that represent terms. The shortcut names are distinct from the names
; of variables in the calculator. A shortcut -- an association between
; a shortcut symbol and a term -- must be declared before a term that contains
; the shortcut could be evaluated. The declaration of a shortcut does not
; cause the shortcut term to be evaluated. Therefore a term that a
; shortcut represents may contain other shortcuts -- or even not yet
; defined shortcuts.
;
; Syntax:
;	<var> ::= <id> -- follows the syntax of Scheme identifiers, _except_
;		it may not start with a %-sign and it may not be one of the
;		reserved words: "L", "X"
;
;	<shortcut> ::= "%"<id>	-- a shortcut
;	<program> ::= {<command> | <term>}*
;	<term> ::= <var>
;		| <shortcut>
;		| (<term> <term>+)	-- application
;		| (<term>)		-- the same as <term>
;		| (L <var> (<term>+))	-- abstraction
;
;	<command> ::= (X Define-Shortcut  <shortcut> <term> )
;		    | (X Define-Shortcut*  <shortcut> <term> )
;		    | (X trace-on) | (X trace-off)
;		    | (X equal?  <term1> <term2> )
;		    | (X equal?*  <term1> <term2> )
;		    | (X expand-shortcuts  <term> )
;		    | (X include  "filename" )
;		    | (X  <other-defined-command> <command-args>* )
;		      (see X-Commands assoc list below)
;
; Commands whose name ends in "*" are strict -- that is, they perform
; evaluation of their arguments. While 'Define-Shortcut' binds a
; shortcut to a <term>, 'Define-Shortcut*' binds the shortcut to the
; normal form of the <term>. The strictness specification is just an
; optimization: if a shortcut was introduced by 'Define-Shortcut*',
; defining it through 'Define-Shortcut' will not affect the result of
; evaluation of any term containing that shortcut. The converse is not
; true: For example, if a <term> does not have a normal form or refers
; to yet to be defined shortcuts, only 'Define-Shortcut' can be used
; to bind the term to a shortcut.
;
; The main function in this module -- REPL -- implements a
; read-eval-print loop which reads a <term> or a <command> one by one
; and prints out the result of their evaluation (with all intermediate
; steps, if requested)


; ULisp note: REPL has been renamed lambda-calculator


;
; Note abstraction looks like an application of a pre-defined
; abstraction-constructor L to a variable and a term-body. This suggests an
; alternative formulation of Lambda-calculus: rather then postulate two
; types of complex terms -- abstraction and application -- we can get by with
; application only; we also need a distinguished primitive term L whose
; application to a var and a term yields an "applyable value".
;
; In the current version, we prohibit terms represented by shortcuts
; to have any free variables. This greatly simplifies evaluation of
; terms containing shortcuts. If we are to allow free variables in
; shortcut terms in the future, we have two choices. One is to avoid
; capturing of these variables during substitution: this is equivalent
; to lexical scoping of shortcuts. Or we can substitute the shortcut
; body in the containing term as it is, thus allowing a potential
; capture of shortcut's free variables.  This is equivalent to dynamic
; scoping rules as in e-Lisp. It's amazing how simple term-rewriting
; calculus represents all the major notions of programming languages.
;
; This module also defines a few auxiliary (yet useful) functions:
;	expand-shortcuts -- takes a term and expands all of its
;		shortcuts
;	term-equal?  -- compares two terms modulo alpha-renaming (that is,
;		performing renaming of bound variables if necessary)
; 
;
; Motivation for the present lambda-calculator
;
; A Scheme system is not a lambda-calculator: Scheme is not an
; applicative-order lambda-calculator, let alone a normal-order
; one. Although call-by-value may look rather similar to
; applicative-order reductions (both attempt inner-most reductions
; first), there is an important difference.  Let's consider a term:
;	((L z z) (x y))
;
; A normal-order evaluator will first take the left-most redex, the
; application of the lambda-abstraction.  The result is (x y) -- which
; is a term in the normal form as no further reductions are possible.
;
; If we follow the applicative order, we should try to reduce the
; inner-most subterm (x y) first. No reduction applies. Therefore, we
; leave (x y) as it is and attempt the second redex: the application
; of the lambda-abstraction. The result is a term (x y), which is in
; the normal form.
;
; A call-by-value evaluator such as Scheme will attempt the inner-most
; subterm (x y) first as well. This subterm is an application, which
; is not a value. Furthermore, it cannot be reduced. Whereas an
; applicative-order evaluator will leave such a term alone and will
; look for other reductions to perform, a call-by-value evaluator
; signals an error: a failure to reduce a term to a value.
;
; A low-level macro facility of Scheme however does behave like a
; lambda-calculator. Like a normal-order evaluator, the macro-expander
; attempts the left-most reduction (i.e., expansion) first. It
; re-scans the result searching again for the first applicable
; reduction. Furthermore, when the macro-expander comes across a form
; such as (x y) where x is not a macro, the macro-expander leaves the
; form as it is and keeps looking for forms to expand. A
; macro-expander does not signal an error when it comes across an
; application that cannot be reduced. Unlike a lambda-calculator
; however, the low-level macro-facility of Scheme does not permit
; definitions and applications of unnamed abstractions (i.e.,
; anonymous macros).

; $Id: lambda-calc.scm,v 1.1 2001/03/30 20:27:14 oleg Exp oleg $


; The following macro runs built-in test cases -- or does not run,
; depending on which of the two lines below you commented out
;(define-macro run-test (lambda body
;			 `(begin (newline) (display "-->Test") (newline)
;				 ,@body)))
(define-macro run-test (lambda body '(begin #f)))


(define-macro (shortcut? x)
  `(and (symbol? ,x) (char=? (string-ref (symbol->string ,x) 0) #\%)))

		; make sure lst is a list of two elements
(define-macro (assert-two-elem-list lst)
  `(assert (and (pair? ,lst) (pair? (cdr ,lst)) (null? (cddr ,lst)))))


;-------------------------------------------------------------------------
; Evaluation environment
; 	in which terms are evaluated. The environment provides bindings
;	for shortcuts. The environment also specifies other applicable
;	flags and options, e.g., a debug/trace flag
;
; Environment is a _record_: a vector
;	slot 0:	signature
;	slot 1: debug/trace flag:
;		'trace -- print our reductions as they are performed
;		#f  -- do not trace term evaluation
;	slot 2: evaluation threshold; evaluation of a term is aborted after
;		that many redexes are performed
;	slot 3: bindings for shortcuts, an assoc list of tuples:
;		(<shortcut> <term> fv-<term>)
; 	where <shortcut> is the shortcut symbol (which always starts with '%'),
; 	<term> is the corresponding term, and FV is a list of free variables in
; 	this term. Note that the the latter list is actually a promise.
;	Thus we don't scan the term for free variables until we really need
;	that list (until the shortcut is being evaluated)
;	In the current version, the shortcut-term-fv must evaluate to '() --
;	a shortcut term is not permitted unbound variables.


(define (env-signature) env-signature)

	; Make a new empty environment
(define (make-env)
  (vector env-signature #f 50 '()))

	; make sure env is indeed an evaluation environment
(define-macro (assert-env env)
  `(assert (and (vector? ,env) (eq? (vector-ref ,env 0) env-signature))))

	; Getters and setters
(define (env-trace-flag env)
  (assert-env env) (vector-ref env 1))

(define (env-trace-flag-set! env flag)
  (assert-env env) (vector-set! env 1 flag))

(define (env-eval-threshold env)
  (assert-env env) (vector-ref env 2))

(define (env-eval-threshold-set! env threshold)
  (assert-env env) (vector-set! env 2 threshold))


	; Given a shortcut symbol, lookup the corresponding term
	; and the list of its fv.
	; Return '(term fv-list) (where the fv-list may be a promise)
	; If the shortcut is not found, return #f
(define (env-sc-lookup env shortcut)
  (assert-env env)
  (let ((sc-assoc (assq shortcut (vector-ref env 3))))
    (and sc-assoc (cdr sc-assoc))))

	; Define a new shortcut: an association of a symbol shortcut
	; with a term. Once a shortcut is defined, it may not be re-defined. 
(define (env-sc-define! env shortcut term)
  (assert (shortcut? shortcut))
  (if (env-sc-lookup env shortcut)
    (error "Shortcut " shortcut " has been already defined "))
  (vector-set! env 3
    (cons
      (list shortcut term (delay (free-vars env term)))
      (vector-ref env 3))))

	; Dump the evaluation env
(define (env-print env)
  (assert-env env)
  (cout nl "Evaluation environment:"
    nl "Trace flag: " (env-trace-flag env)
    nl "Evaluation threshold: " (env-eval-threshold env)
    nl "Defined shortcuts:"
    (lambda ()
      (for-each
        (lambda (sc-assoc)
          (cout nl #\tab (car sc-assoc) ": " (cadr sc-assoc)))
        (vector-ref env 3)))
    nl "--->" nl))


;-------------------------------------------------------------------------
; 			Term decomposition
; The macro that decomposes a term and calls the corresponding
; handler. 
; The macro has the following syntax and usage:
;	(case-term-structure env term
;	   (var <handle-var>) ; where var is bound to the <var> in question
;	   (%sh <handle-shortcut-term>) ; where sh is bound to the shortcut
;					; symbol, and fv-sh is bound to
;					; the fv of this shortcut
;	   ((<term> <arg>) <handle-appl>)
;	   ((L <var> <body>) <handle-abstraction>))
; We assume left-associativity, that is, (a b c) <==> ((a b) c)
;
; The macro expands to a procedure invocation
;  (decompose-term env term
;	`( (var . ,(lambda (var) <handle-end-of-term-list>))
;	   (shortcut . ,(lambda (shortcut-term fv-list) <handle-shortcut-term>))
;	   (abst . (lambda (var lambda-body) <handle-abstraction>))
;	   (appl . (lambda (term arg) <handle-application>))))
;

(define-macro (case-term-structure env term . alternatives)
  (define-macro (assert . x)
    (if (null? (cdr x))
      `(or ,@x (error "failed assertion" ',@x))
      `(or (and ,@x) (error "failed assertion" '(,@x)))))
  (define-macro (shortcut? x)
  `(and (symbol? ,x) (char=? (string-ref (symbol->string ,x) 0) #\%)))

  (define (assert-two-symbol-list lst)
    (assert (pair? lst) (pair? (cdr lst)) (null? (cddr lst)))
    (assert (symbol? (car lst)))
    (assert (symbol? (cadr lst))))
   
  (define (make-handler alternative)
    (assert (pair? alternative) (pair? (cdr alternative))
        (null? (cddr alternative)))
    (cond
      ((shortcut? (car alternative))
        (let* ((sh-string (symbol->string (car alternative)))
            (sh-proper-str (substring sh-string 1 (string-length sh-string))))
          (cons 'shortcut (list 'unquote 
              `(lambda (,(string->symbol sh-proper-str)
                  ,(string->symbol (string-append
				    (symbol->string 'fv-) sh-proper-str)))
                ,(cadr alternative))))))
      ((symbol? (car alternative))
        (cons 'var (list 'unquote
            `(lambda (,(car alternative)) ,(cadr alternative)))))
      ((not (pair? (car alternative)))
        (error "Bad term-case option: " alternative))
      ((eq? 'L (caar alternative))
        (assert-two-symbol-list (cdar alternative))
        (cons 'abst (list 'unquote
            `(lambda ,(cdar alternative) ,(cadr alternative)))))
      (else
        (assert-two-symbol-list (car alternative))
        (cons 'appl (list 'unquote
            `(lambda ,(car alternative) ,(cadr alternative)))))
      ))

  (list 'decompose-term env term
    (list 'quasiquote (map make-handler alternatives))))


; Besides a trivial analysis of term's structure, we also take care
; of the following transformations:
; (<term>) ==> <term>
; (<t1> <t2> <t3>) ==> ((<t1> <t2>) <t3>)

(define (decompose-term env term handlers)
  (define (get-handler symb)
    (cond ((assq symb handlers) => cdr)
      (else (error "missing handler for " symb " in " handlers))))
  (cond
    ((shortcut? term)
      (apply (get-handler 'shortcut)
        (cond
          ((env-sc-lookup env term) =>
            (lambda (sc-term-fvlist)	; which is '(term term-fv-list)
              (let ((fv (force (cadr sc-term-fvlist))))
                (if (not (null?  fv))
                  (error "Shortcut's " term " term " (car sc-term-fvlist)
                    " has free variables!"))
                (list (car sc-term-fvlist) fv))))
          (else
            (error "Undefined shortcut " term)))))
    ((symbol? term) ((get-handler 'var) term))
    ((not (pair? term)) (error "Invalid term: " term))
    ((eq? 'L (car term))
      (assert-two-elem-list (cdr term))
      (apply (get-handler 'abst) (cdr term)))
    ((null? (cdr term))		; term is (<t1>)
      (decompose-term env (car term) handlers))
    ((null? (cddr term))	; term is (<t1> <t2>)
      (apply (get-handler 'appl) term))
    (else			; term is (<t1> <t2> ... <tn>)
      (let ((term-rev (reverse term))) ; ==> (((<t1> <t2> ...) <tn>)
        (apply (get-handler 'appl)
          (list (reverse (cdr term-rev)) (car term-rev)))))))


; Term composition
; These functions return #f if one of the terms' components is #f

(define (make-term-abst bound-var term)
  (and term
    (begin (assert (symbol? bound-var))
      (list 'L bound-var term))))

(define (make-term-appl head arg)
  (and head arg (list head arg)))

;-------------------------------------------------------------------------
; 			Evaluation Machinery

; Return the list of free variables in a term
; The list may contain duplicates (which does not change the semantics of the
; list)
; Formal definition:
;	(free-vars <var>) 	=> (set-of <var>)
;	(free-vars (<term> <arg>)) => 
;			(union (free-vars <term>) (free-vars <arg>))
;	(free-vars (L <var> <term>)) => 
;			(set-diff (free-vars <term>) (set-of <var>))

(define (free-vars env term)
  (define (fv term bound)
    (case-term-structure env term
      (var (if (memq var bound) '() (list var)))
      (%shortcut fv-shortcut)
      ((L bound-var body) (fv body (cons bound-var bound)))
      ((thead targ) (append (fv thead bound) (fv targ bound)))))
  (fv term  '()))

(define-macro (show exp)
  `(cout ',exp " ==> " ,exp nl))

		; make sure that the 'FORM' gave upon evaluation the
		; EXPECTED-RESULT
(define-macro (expect form expected-result)
  `(begin
    (display "evaluating ")
    (write ',form)
    (let ((real-result ,form))
     (if (equal? real-result ,expected-result)
       (cout "... gave the expected result: "
         (lambda () (write real-result)) nl)
       (error "... yielded: " real-result
        " which differs from the expected result: " ,expected-result)
      ))))

; a few lines of validation code
(run-test
 (let ((env (make-env)))
   (expect (free-vars env 'xx) '(xx))
   (expect (free-vars env '(L x x)) '())
   (expect (free-vars env '(xx y z)) '(xx y z))
   (expect (free-vars env '(L x (x y))) '(y))
   (expect (free-vars env '((L x (x y)) (x y z))) '(y x y z))
   (expect (free-vars env '((L x (L x (x y))) (L x (L y (x y))))) '(y))
))


	; Return the TERM with all shortcuts expanded with the corresponding
	; terms.
	; We rely on the fact that no shortcut is allowed free variables.
	; Thus we can substitute shortcut terms without worrying about
	; capturing of free variables

(define (expand-shortcuts env term)
  (case-term-structure env term
    (var var)
    (%shortcut (expand-shortcuts env shortcut))
    ((L bound-var body) (make-term-abst bound-var (expand-shortcuts env body)))
    ((thead targ)
      (make-term-appl 
       (expand-shortcuts env thead) (expand-shortcuts env targ)))))


; a few lines of validation code
(run-test
 (let ((env (make-env)))
   (env-sc-define! env '%c0 '(L f (L x x)))
   (env-sc-define! env '%c1 '(L f (L x (f x))))
   (env-sc-define! env '%succ '(L c (L f (L x (f (c f x))))))
   (env-sc-define! env '%c1-1 '(%succ %c0))
   (env-print env)
   (expect (expand-shortcuts env '%c1) '(L f (L x (f x))))
   (expect (expand-shortcuts env '%c1-1)
	   '((L c (L f (L x (f ((c f) x))))) (L f (L x x))))
   (expect (expand-shortcuts env '(%succ %c0))
	   '((L c (L f (L x (f ((c f) x))))) (L f (L x x))))
   (expect (free-vars env '((%succ %c0) f x)) '(f x))
   (expect (free-vars env '((%succ %c0) g y)) '(g y))
;  (env-sc-define! env '%succ1 '(L c (L f (f (c f x)))))
;  (show (expand-shortcuts env '(x %succ1)))
))


	; Check out to see if two terms are equal modulo renaming
	; of bound variables and expansion of shortcuts.
	; Note that term-deconstruction automatically guarantees
	; left-associativity, i.e.,
	; (a b c) <===> ((a b) c)
	; as well as (term) <===> term

(define (term-equal? env term1 term2)

		; In the following, subst-list is the assoc list
		; that defines renaming of bound variables in term
		; (which were necessary to match the terms so far)
		; Note both terms should have their shortcuts expanded
  (define (term-compare-equal? term1 subst-list1 term2 subst-list2)
;    (cout "comparing " term1 " with " term2 " under " subst-list1 nl)
    (case-term-structure env term1
      (var1
        (case-term-structure env term2
          (var2				; both terms are variables
            (let ((bv1 (assq var1 subst-list1))
                  (bv2 (assq var2 subst-list2)))
                (if bv1			; var1 is bound in term1
                  (and bv2 (eq? (cdr bv1) (cdr bv2)))
                  (and			; var1 is free in term1
                    (not bv2) (eq? var1 var2)))))
          ((L bound-var2 body2) #f)
          ((thead2 targ2) #f)))
      ((L bound-var1 body1)			; term1 is an abstraction
        (case-term-structure env term2
          (var #f)
          ((L bound-var2 body2)			; rename both bound vars
            (let ((renamed-bound-var (gensym)))	; to a unique common name
              (term-compare-equal?
                body1 (cons (cons bound-var1 renamed-bound-var) subst-list1)
                body2 (cons (cons bound-var2 renamed-bound-var) subst-list2))))
          ((thead2 targ2) #f)))
      ((thead1 targ1)				; term1 is an application
        (case-term-structure env term2
          (var #f)
          ((L bound-var2 body2) #f)
          ((thead2 targ2)
            (and
              (term-compare-equal? thead1 subst-list1 thead2 subst-list2)
              (term-compare-equal? targ1 subst-list1 targ2 subst-list2)))))))

  (term-compare-equal? 
    (expand-shortcuts env term1) '() (expand-shortcuts env term2) '()))

; a few lines of validation code
(run-test
 (let ((env (make-env)))
   (env-sc-define! env '%c0 '(L f (L x x)))
   (env-sc-define! env '%c1 '(L f (L x (f x))))
   (env-sc-define! env '%succ '(L c (L f (L x (f (c f x))))))
   (env-print env)
   (expect (term-equal? env 'x 'x) #t)
   (expect (term-equal? env 'x 'y) #f)
   (expect (term-equal? env '(x) 'x) #t)
   (expect (term-equal? env 'x '((x))) #t)
   (expect (term-equal? env '(x) '((x))) #t)
   (expect (term-equal? env '(a b) '((a b))) #t)
   (expect (term-equal? env '(a b (c)) '((a b) c)) #t)
   (expect (term-equal? env '(((a (b c)) (d)) (e f)) '(a (b c) d (e f))) #t)
   (expect (term-equal? env '(a (b c) d (e f)) '(a b c d (e f))) #f)

   (expect (term-equal? env '(L x x) '(L x y)) #f)
   (expect (term-equal? env '(L x x) '(L y y)) #t)
   (expect (term-equal? env '(L x (x x)) '(L y (y y))) #t)
   (expect (term-equal? env '(L x (x x)) '(L y (y x))) #f)
   (expect (term-equal? env '(L y (y x)) '(L x (x x))) #f)
   (expect (term-equal? env '(L y (y x)) '(L z (z x))) #t)
   (expect (term-equal? env '(L x (L y (x y))) '(L f (L f (f f)))) #f)
   (expect (term-equal? env '(L x (L x (x x))) '(L f (L f (f f)))) #t)
   (expect (term-equal? env '(L x (L y (y y))) '(L f (L f (f f)))) #t)
   (expect (term-equal? env '%c1 '%c1) #t)
   (expect (term-equal? env '%c1 '%c0) #f)
   (expect (term-equal? env '%c1 '(L f (L x (f x)))) #t)
   (expect (term-equal? env '%c1 '(L g (L x (g x)))) #t)
   (expect (term-equal? env '%c1 '(L g (L y (g y)))) #t)
   (expect (term-equal? env '(L g (L y (g y))) '%c1) #t)
   (expect (term-equal? env '(L g (L y (g x))) '%c1) #f)
   (expect (term-equal? env '%c1 '(L g (L y (g x)))) #f)
))


		; beta-substitute arg for subst-var in the body,
		; trying to avoid capture of free vars in the arg
		; Those bound variables that can capture free vars in arg
		; are renamed (the other bound vars are left as they are)
(define (beta-subst env subst-var body arg)

  (define (do-subst term rename-list fv-arg)
    (case-term-structure env term
      (var
        (cond
          ((assq var rename-list) => cdr)
          ((eq? var subst-var) arg)
          (else var)))
      (%shortcut term)		; never substitute inside the shortcut
      ((L bound-var body)
        (cond
	 ((memq bound-var fv-arg)	; capture of fv(arg) is imminent
	  (let ((renamed-bv (gensym)))
	    (make-term-abst renamed-bv
	       (do-subst body (cons (cons bound-var renamed-bv) rename-list)
			 fv-arg))))
	 (else
	  (make-term-abst bound-var 
	     (do-subst body (cons (cons bound-var bound-var) rename-list)
		       fv-arg)))))
      ((thead targ) (make-term-appl
          (do-subst thead rename-list fv-arg)
          (do-subst targ rename-list fv-arg)))))

  (assert (symbol? subst-var))

  (if (env-trace-flag env)
    (cout nl "Beta-subst: " body "[" arg "/" subst-var "]" nl))

  (do-subst body '() (free-vars env arg)))
         
; a few lines of validation code
(run-test
 (let ((env (make-env)))
   (env-sc-define! env '%c0 '(L f (L x x)))
   (env-sc-define! env '%c1 '(L f (L x (f x))))
   (env-sc-define! env '%succ '(L c (L f (L x (f (c f x))))))
   (env-trace-flag-set! env #t)
   (env-print env)
   (expect (term-equal? env
	    (beta-subst env 'x '((L y x) (L x x) x) 'y)
	    '((L z y) (L u1 u1) y))
	   #t)
   (expect (term-equal? env
            (beta-subst env 'x '(x y (L y (x y (L y (x y))))) '(x y z))
	    '((x y z) y (L v ((x y z) v (L u ((x y z) u))))))
	   #t)
   (expect (term-equal? env
            (beta-subst env 'x '(x y (L x (x y (L x (x y))))) '(x y z))
	    '((x y z) y (L u1 (u1 y (L u2 (u2 y))))))
	   #t)
   (expect (term-equal? env
	    (beta-subst env 'x '(x y (L x (x x (L x (x x))))) '(y z))
	    '((y z) y (L u1 (u1 u1 (L u2 (u2 u2))))))
	   #t)
   (expect (term-equal? env
	    (beta-subst env 'x '(x y (L x (x y)) (L y (x y))) '(x y z))
	    '((x y z) y (L u1 (u1 y)) (L u2 ((x y z) u2))))
	   #t)
))


	; Carry out the evaluation of a term. We always look at the
	; leftmost possible reduction
(define (term-eval env term)
		; Try to eta-reduce body. Return the reduced
		; term if successful; otherwise, return #f
  (define (try-eta-reduction env bound-var body)
    (case-term-structure env body
      (var #f)
      (%shortcut #f)	; shortcut can't have free vars at present
      ((L bound-var body) #f)
      ((thead targ) 
        (case-term-structure env targ
          (arg-var (and (eq? arg-var bound-var)
              (not (memq bound-var (free-vars env thead)))
              (begin
                (if (env-trace-flag env)
                  (cout nl "Eta-reduction in: " (make-term-abst bound-var body)
                    nl))
                thead)))
          (%shortcut #f)	; shortcut can't have free vars at present
          ((L bound-var body) #f)
          ((thead targ) #f)))))

		; Given a term, try to reduce it. That is, we look for
		; the first (i.e., leftmost redex) and apply it.
		; Return the term after reduction, or
		; return #f if no redex is found.
		; Note we prefer a beta reduction over an eta-reduction
		; if both apply (as beta-reduction is more general)
  (define (try-reduction env term)
    (case-term-structure env term
      (var #f)
;      (%shortcut (try-reduction env shortcut))
      (%shortcut #f)
      ((L bound-var body)
        (or (try-eta-reduction env bound-var body)
          (make-term-abst bound-var (try-reduction env body))))
      ((thead targ)
        (case-term-structure env thead
          (var (make-term-appl var (try-reduction env targ)))
          (%shortcut (try-reduction env (make-term-appl shortcut targ)))
          ((L bound-var body)
            (beta-subst env bound-var body targ))
          ((headhead headarg) 
            (or (make-term-appl (try-reduction env thead) targ)
              (make-term-appl thead (try-reduction env targ))))))))


			; Note the second-scan property of this algorithm...
  (let loop ((count 0) (term term))
    (cond
      ((> count (env-eval-threshold env))
        (cout nl "Redex threshold of " (env-eval-threshold env)
          " reached, evaluation of the term" nl
          term nl "aborted" nl)
        term)
      ((try-reduction env term) =>
        (lambda (new-term)
          (if (env-trace-flag env)
            (cout "Reduction " term " ==> " new-term nl))
          (loop (++ count) new-term)))
      (else term))))

         

   
        
; a few lines of validation code
(run-test
 (let ((env (make-env)))
   (env-sc-define! env '%c0 '(L f (L x x)))
   (env-sc-define! env '%c1 '(L f (L x (f x))))
   (env-sc-define! env '%succ '(L c (L f (L x (f (c f x))))))
   (env-sc-define! env '%Y '(L f ((L x (f (x x))) (L x (f (x x))))))
   (env-trace-flag-set! env #t)
   (env-eval-threshold-set! env 10)
   (env-print env)
   (expect (term-equal? env
	     (term-eval env '((L x (a b x)) (L a (a b))))
	     '(a b (L u (u b))))
	     #t)
   (expect (term-equal? env
             (term-eval env '(((L f (L x (f x))) g) z))
	     '(g z))
	     #t)
   (expect (term-equal? env
             (term-eval env '(%succ %c0))
	     '(L f f))
	     #t)
   (expect (term-equal? env
             (term-eval env '((L x (x x)) (L x (x x))))
	     '((L u (u u)) (L v (v v))))
	     #t)
   (expect (term-equal? env
             (term-eval env '((L x y) ((L x (x x)) (L x (x x)))))
	     'y)
	     #t)
   (expect (term-equal? env
             (term-eval env '((L x (L y (f x y y))) (g y)))
	     '(L z (f (g y) z z)))
	     #t)
   (expect (term-equal? env
	     (term-eval env '(%succ %c1))
	     '(L g (L x (g (g x)))))
	     #t)
   (expect (term-equal? env
	     (term-eval env '(%Y e))
	     '(e (e (e (e (e (e (e (e (e (e 
			      ((L x (e (x x))) (L x (e (x x)))))))))))))))
	     #t)
   
))




;-------------------------------------------------------------------------
; 		The front end -- a Read, Eval, Print Loop
; 

	; An Assoc list of command-tags and the corresponding procedures
	; A command handler takes the env as the first argument, followed
	; by the other arguments of the command
(define X-Commands
  `((Define . ,env-sc-define!)
    (Define* . ,(lambda (env shortcut term)
        (env-sc-define! env shortcut (term-eval env term))))
    (equal? . ,term-equal?)
    (equal?* . ,(lambda (env term1 term2)
        (term-equal? env (term-eval env term1) (term-eval env term2))))
    (eval . ,term-eval)
    (expand-shortcuts . ,expand-shortcuts)
    (free-vars . ,free-vars)
    (env-print . ,env-print)
    (set-eval-threshold . ,env-eval-threshold-set!)
    (trace-on . ,(lambda (env) (env-trace-flag-set! env #t)))
    (trace-off . ,(lambda (env) (env-trace-flag-set! env #f)))
    (include . ,(lambda (env filename)
		  (with-input-from-file (concat (current-directory) filename) ;; ULisp change here
		    (lambda () (lambda-calculator env)))))
))


(define (lambda-calculator . env-opt)
  (let ((env (if (null? env-opt) (make-env) (car env-opt))))

		; Evaluate 'stmt' and return the result of its evaluation
    (define (exec-stmt stmt)
      (cond
        ((not (pair? stmt)) (term-eval env stmt))
        ((eq? 'X (car stmt))
          (assert (pair? (cdr stmt)))
          (cond
            ((assq (cadr stmt) X-Commands) =>
              (lambda (command)
                (apply (cdr command) (cons env (cddr stmt)))))
            (else
              (cerr "invalid command: " (cadr stmt) nl)
              '())))
        (else (term-eval env stmt))))

;ulisp;
(newline)
(display "LCalc> ")
;ulisp;

    (let loop ((stmt (read)))
      (cond
        ((eof-object? stmt)
          (cout nl "Done" nl))
        (else
          (display (exec-stmt stmt))
          (newline)
;ulisp;
(display "LCalc> ")
;ulisp;
          (loop (read)))))))
(run-test
 (newline)
 (display "All tests passed")
 (newline)
)

(cout nl "Enter a command or a term to evaluate." nl
      "For example: (X include \"lambda-arithm-basic.scm\") followed by (%expt %c2 %c3)" nl)
(lambda-calculator)

