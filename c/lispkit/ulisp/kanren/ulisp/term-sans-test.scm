;		Terms, variables, substitutions, unification
;
; The appropriate prelude (e.g., chez-specific.scm) is assumed.

; Some terminology related to variables and substitutions
;
; A substitution subst is a finite map { xi -> ti ... }
; where xi is a logic variable.
; ti is a term ::= variable | Scheme-atom | (cons term term)
; We will sometimes call one `component' xi -> ti of a substitution
; a commitment, or a binding, of a variable xi to a term ti.
;
; A variable x is free in the substitution subst if x \not\in Dom(subst)
;
; Given a term t and a substitution subst, a weak reduction
;   t -->w  t'
; is defined as
;   x -->w subst[x]  if x is a var and x \in Dom(subst)
;   t -->w t otherwise
;
; A strong reduction
;   t -->s t' 
; is defined as
;   x -->s subst[x]  if x is a var and x \in Dom(subst)
;   (cons t1 t2) -->s (cons t1' t2')
;          where t1 -->s t1'  t2 -->s t2'
;   t -->s t otherwise
;
; The notion of reduction can be extended to substitutions themselves:
;   { xi -> ti ...} -->w { xi -> ti' } where ti -> ti'
; ditto for -->s.
; Let -->w* be a reflexive transitive closure of -->w, and 
; let -->w! be a fixpoint of -->w. Ditto for -->s* and -->s!
; For acyclic substitutions, the fixpoints exist.
;
; The confluence of the reduction is guaranteed by the particular form
; of the substitution produced by the unifier (the unifier always
; deals with the weak normal forms of submitted terms).
;
; The similarity of the weak normalization with call-by-value and
; the strong normalization with the applicative-order reduction should
; be apparent.
;
; Variable x is called ultimately free if
; x -->w! x' and x' is free in the subtutution in question.
;
; Two ultimately free variables x and y belong to the same equivalence class
; if x -->w! u and y -->w! u
; The (free) variable u is the natural class representative.
; For the purpose of presentation, one may wish for a better representative.
; Given a set of equivalent variables xi -->w! u,
; a pretty representative is a member z of that set such that the
; string name of 'z' is lexicographically smaller than the string names 
; of the other variables in that set.
;
; If a variable x is ultimately free in subst and x ->w! u, 
; then there is a binding
; v1 -> v2 where both v1 and v2 are variables and v2 ->w! u. Furthermore,
; the set of all such v1 union {u} is the whole equivalence class of x.
; That property is guaranteed by the unifier. That property lets us
; build an inverse index to find the equivalence class of x.
;
; $Id: term.scm,v 4.50 2005/02/12 00:05:27 oleg Exp $


;----------------------------------------
; A few preliminaries
; LET*-AND: a simplified and streamlined AND-LET*.
; The latter is defined in SRFI-2 <http://srfi.schemers.org/srfi-2/>

(define-syntax let*-and
  (syntax-rules ()
    ((_ false-exp () body0 body1 ...) (begin body0 body1 ...))
    ((_ false-exp ((var0 exp0) (var1 exp1) ...) body0 body1 ...)
     (let ((var0 exp0))
       (if var0
         (let*-and false-exp ((var1 exp1) ...) body0 body1 ...)
         false-exp)))))

; Regression testing framework
; test-check TITLE TESTED-EXPRESSION EXPECTED-RESULT 
; where TITLE is something printable (e.g., a symbol or a string)
; EXPECTED-RESULT and TESTED-EXPRESSION are both expressions.
; The expressions are evaluated and their results are cmpared
; by equal?
; If the results compare, we just print the TITLE.
; Otherwise, we print the TITLE, the TESTED-EXPRESSION, and
; the both results.
(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (cout "Testing " title nl)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (errorf 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

(define symbol-append
  (lambda symbs
    (string->symbol
      (apply string-append
        (map symbol->string symbs)))))

;----------------------------------------


(cond-expand
  (chez
    (define-record logical-variable (id) ()))
  (else
    ; use SRFI-9 records
    (define-record-type *logical-variable*
      (make-logical-variable name) logical-variable?
      (name logical-variable-id))
    ))
(define logical-variable make-logical-variable)
(define var? logical-variable?)

; Introduction of a logical variable
(define-syntax let-lv
  (syntax-rules ()
    ((_ (id ...) body)
      (let ((id (logical-variable 'id)) ...) body))))

; The anonymous variable
(define _ (let-lv (_) _))

; Another way to introduce logical variables: via distinguished pairs
; (define logical-var-tag (list '*logical-var-tag*)) ; unique for eq?
; (define native-pair? pair?)
; (define logical-variable
;   (lambda (id)
;     (cons logical-var-tag id)))
; (define var?
;   (lambda (x)
;     (and (native-pair? x) (eq? (car x) logical-var-tag))))
; (define logical-variable-id
;   (lambda (x)
;     (if (var? x) (cdr x) 
;       (errorf 'logical-variable-id "Invalid Logic Variable: ~s" x))))
; (define pair?
;   (lambda (x)
;     (and (native-pair? x) (not (eq? (car x) logical-var-tag)))))


; Eigen-variables -- unique symbols that represent universally-quantified
; variables in a term
; For identification, we prefix the name of the eigen-variable with
; the exclamation mark. The mark makes sure the symbol stands out when
; printed.

(define eigen-variable
  (lambda (id)
    (symbol-append '! id '_ (gensym))))

(define eigen-var?
  (lambda (x)
    (and (symbol? x)
      (let ((str (symbol->string x)))
	(> (string-length str) 2)
	(char=? (string-ref str 0) #\!)))))


; (eigen (id ...) body) -- evaluate body in the environment
; extended with the bindings of id ... to the corresponding
; eigen-variables
(define-syntax eigen
  (syntax-rules ()
    ((_ (id ...) body)
      (let ((id (eigen-variable 'id)) ...) body))))

;;; ------------------------------------------------------

(define commitment cons)
(define commitment->term cdr)
(define commitment->var car)

(define empty-subst '())
(define empty-subst? null?)

(define extend-subst
  (lambda (v t subst)
    (cons (commitment v t) subst)))

; get the free vars of a term (a list without duplicates)
(define vars-of
  (lambda (term)
    (let loop ((term term) (fv '()))
      (cond
        ((var? term) (if (memq term fv) fv (cons term fv)))
        ((pair? term) (loop (cdr term) (loop (car term) fv)))
        (else fv)))))

; Check to see if a var occurs in a term
(define occurs?
  (lambda (var term)
    (cond
      ((var? term) (eq? term var))
      ((pair? term) (or (occurs? var (car term)) (occurs? var (cdr term))))
      (else #f))))

; A ground term contains no variables
(define ground?
  (lambda (t)
    (cond
      ((var? t) #f)
      ((pair? t) (and (ground? (car t)) (ground? (cdr t))))
      (else #t))))

; Given a term v and a subst s, return v', the weak normal form of v:
; v -->w! v' with respect to s
(define subst-in-weak
  (lambda (v s)
    (cond
      ((var? v) 
       (cond
         ((assq v s) =>
          (lambda (b) (subst-in-weak (commitment->term b) s)))
         (else v)))
      (else v))))

; Given a term v and a subst s, return v', the strong normal form of v:
; v -->s! v' with respect to s
(define subst-in
  (lambda (t subst)
    (cond
      ((var? t)
       (let ((c (assq t subst)))
         (if c (subst-in (commitment->term c) subst) t)))
      ((pair? t)
       (cons
         (subst-in (car t) subst)
         (subst-in (cdr t) subst)))
      (else t))))


; ; Given a term v and a subst s, return v', the strong normal form of v:
; ; v -->s! v' with respect to s
; (define subst-vars-recursively
;   (lambda (t subst)
;     (cond
;       ((var? t)
;        (cond
;          ((assq t subst) =>
;           (lambda (c)
;             (subst-vars-recursively
;               (commitment->term c) (remq c subst))))
;          (else t)))
;       ((pair? t)
;        (cons
;          (subst-vars-recursively (car t) subst)
;          (subst-vars-recursively (cdr t) subst)))
;       (else t))))

; (define normalize-subst
;   (lambda (subst)
;     (map (lambda (c)
;            (commitment (commitment->var c)
;              (subst-vars-recursively (commitment->term c) subst)))
;       subst)))


; Sooner or later, we will need to print out a term or do something
; else with it. We have to decide what to do with free variables that
; may be in that term.
; The long experience with Kanren and miniKanren and long discussions
; convinced us that it's best to `display' free variables as
; _.n where n is a number. BTW, we can't just display
; logical-variable-id, because distinct logical variables may have the same
; logical-variable-id.

; reify:: term -> reified-term
; where reified-term is identical to term if it is ground.
; Otherwise, we replace all free variables in term with _.n symbols
; The 'reverse' in (reverse (vars-of t))
; just makes the output look as it used to look before. Consider it
(define reify
  (lambda (term)
    (let ((fv (reverse (vars-of term))))
      (if (null? fv) term		; the term is ground
	(let ((renaming			; build the renaming substitution
		(let loop ((counter 0) (fv fv))
		  (if (null? fv) empty-subst
		    (extend-subst 
		      (car fv)
		      (string->symbol
			(string-append "_." (number->string counter)))
		      (loop (+ 1 counter) (cdr fv)))))))
	  (subst-in term renaming))))))


; we will also need to print the substitution, either in whole or in part
; reify-subst:: list-of-vars subst -> reified-subst
; where list-of-vars is a list of variables to reify, or the empty
; list. In the latter case, all variables from subst are reified.
; reified-subst has a form ((var-name reified-term) ...)
; where var-name, for historical reasons, has the form id.0
; where `id' is logical-variable-id.

(define reify-subst
  (lambda (vars subst)
    (let* ((vars (if (null? vars) (map commitment->var subst) vars))
	   (terms (reify (subst-in vars subst))))
      (map (lambda (x y)
	     (list (string->symbol 
		     (string-append (symbol->string (logical-variable-id x))
		       ".0"))
	       y))
	     vars terms))))

      
  

; (define compose-subst/own-survivors
;   (lambda (base refining survivors)
;     (let refine ((b* base))
;       (if (null? b*) survivors
;           (cons-if-real-commitment
;             (commitment->var (car b*))
;             (subst-in (commitment->term (car b*)) refining)
;             (refine (cdr b*)))))))
;
; (define compose-subst
;   (lambda (base refining)
;     (cond
;       ((null? base) refining)
;       ((null? refining) base)
;       (else
;         (compose-subst/own-survivors base refining
;           (let survive ((r* refining))
;             (cond
;               ((null? r*) '())
;               ((assq (commitment->var (car r*)) base) (survive (cdr r*)))
;               (else (cons (car r*) (survive (cdr r*)))))))))))

; Replace a logical variable with the corresponding eigen-variable
; Note: to be really right, universalize should be a scoping predicate,
; something like exists:
; (universalize (term) goal)
; to prove 'goal' in the env where term is universalized.
; In that case, the introduced eigen-variables do not escape.
; Also, perhaps universalize should take a subst and first
; do (subst-in term subst) and then universalize the remaining
; logical variables -- which by that time would surely be free.
(define universalize
  (lambda (term)
    (let ((fv (vars-of term)))
      (let ((subst
              (map
                (lambda (v)
                  (commitment v (eigen-variable (logical-variable-id v))))
                fv)))
        (subst-in term subst)))))


; copy-term TERM -> TERM
; return a TERM that is identical to the input term modulo the replacement
; of variables in TERM with fresh logical variables. 
; If a logical variable occurs several times in TERM, the result
; will have the same number of occurrences of the replacement fresh
; variable.
; This is a sort-of dual to universalize, to be used on the other side
; of the implication. It replaces the existential quantification
; (implicit in free logical variables of a term) with the universal
; quantification.
(define copy-term
  (lambda (t)
    (let* ((fv (vars-of t))
	   (subst
	     (map (lambda (old-var)
		    (commitment old-var
		      (logical-variable (logical-variable-id old-var))))
	       fv)))
      (subst-in t subst))))


; Similar to universalize: makes nicer symbols for variables that look
; nicer when printed. The 'reverse' in (reverse (vars-of t))
; just makes the output look as it used to look before. Consider it
; a historical accident.
; (define concretize
;   (lambda (t)
;     (subst-in t
;       (let loop ((fv (reverse (vars-of t))) (env '()))
; 	(cond
; 	  ((null? fv) empty-subst)
; 	  (else (let ((id (logical-variable-id (car fv))))
; 		  (let ((num (let*-and 0 ((pr (assq id env))) (+ (cdr pr) 1))))
; 		    (cons (commitment (car fv) (artificial-id id num))
; 		      (loop (cdr fv) (cons (cons id num) env)))))))))))
;  (define artificial-id
;   (lambda (t-id num)
;     (string->symbol
;       (string-append
;         (symbol->string t-id) "." (number->string num)))))





;-------------------------------------------------------
;;;; This is Oleg's unifier

; Either t or u may be:
; _
; free-var
; bound-var
; pair
; other-value
; So, we have in general 25 possibilities to consider.
; actually, a pair or components of a pair can be variable-free
; or not. In the latter case, we have got to traverse them.
; Also, if a term to unify has come from subst, it has special properties,
; which we can exploit. See below.
;
; "Measurements of the dynamic behavior of unification on four real
; programs show that one or both of the arguments are variables about
; 85% of the time [63]. A subroutine call is made only if both arguments
; are nonvariables." (Peter Van Roy, The Wonder Years ...)
;
; Just like in the union-find unification algorithm, we produce
; substitutions in the "triangular form" (see Baader, Snyder, Unification
; Theory). Circularity is detected only at the end (when we do subst-in).

(define unify
  (lambda (t u subst)
    (cond
      ((eq? t u) subst)			; quick tests first
      ((eq? t _) subst)
      ((eq? u _) subst)
      ((var? t)
       (let*-and (unify-free/any t u subst) ((ct (assq t subst)))
	 (if (var? u)			; ct is a bound var, u is a var
	   (let*-and (unify-free/bound u ct subst) ((cu (assq u subst)))
	     (unify-bound/bound ct cu subst))
	   (unify-bound/nonvar ct u subst))))
      ((var? u)				; t is not a variable...
       (let*-and
         (cond
           ((pair? t) (unify-free/list u t subst))
           ; t is not a var and is not a pair: it's atomic
           (else (extend-subst u t subst)))
         ((cu (assq u subst)))
         (unify-bound/nonvar cu t subst)))
      ((and (pair? t) (pair? u))
       (let*-and #f ((subst (unify (car t) (car u) subst)))
         (unify (cdr t) (cdr u) subst)))
      (else (and (equal? t u) subst)))))

; ct is a commitment to a bound variable, u is a atomic or a composite
; value -- but not a variable
(define unify-bound/nonvar
  (lambda (ct u subst)
    (let ((t (commitment->term ct)))
      (cond				; search for the end of ct -> chain
	((eq? t u) subst)
	((var? t)
	  (let*-and 
	    (cond
	      ((pair? u) (unify-free/list t u subst))
              ; u is not a var and is not a pair: it's atomic
	      (else (extend-subst t u subst)))
	    ((ct (assq t subst)))
	    (unify-bound/nonvar ct u subst)))
	; t is some simple or composite value. So is u.
      ((and (pair? t) (pair? u))
	(let*-and #f ((subst (unify-internal/any (car t) (car u) subst)))
	  (unify-internal/any (cdr t) (cdr u) subst)))
      (else (and (equal? t u) subst))))))


; Just like unify. However, the first term, t, comes from
; an internalized term. We know it can't be _ and can't contain _

(define unify-internal/any
  (lambda (t u subst)
    (cond
      ((eq? t u) subst)			; quick tests first
      ((eq? u _) subst)
      ((var? t)
       (let*-and (unify-free/any t u subst) ((ct (assq t subst)))
	 (if (var? u)			; ct is a bound var, u is a var
	   (let*-and (unify-free/bound u ct subst) ((cu (assq u subst)))
	     (unify-bound/bound ct cu subst))
	   (unify-bound/nonvar ct u subst))))
      ((var? u)				; t is not a variable...
       (let*-and			; It's a part of an internal term
	 (extend-subst u t subst)	; no further checks needed
         ((cu (assq u subst)))
         (unify-internals (commitment->term cu) t subst)))
      ((and (pair? t) (pair? u))
       (let*-and #f ((subst (unify-internal/any (car t) (car u) subst)))
         (unify-internal/any (cdr t) (cdr u) subst)))
      (else (and (equal? t u) subst)))))


; Unify two already bound variables represented by their commitments
; ct and cu.
; We single out this case because in the future we may wish
; to unify the classes of these variables, by making a redundant
; binding of (commitment->var ct) to (commitment->term cu) or
; the other way around.
; Aside from the above, this function can take advantage of the following
; facts about (commitment->term cx) (where cx is an existing commitment):
;   - it is never _
;   - it never contains _
; Most importantly, if, for example, (commitment->term ct) is a free variable,
; we enter its binding to (commitment->term cu) with fewer checks.
; in particular, we never need to call unify-free/list nor
; unify-free/any as we do need to rebuild any terms.

(define unify-internals
  (lambda (t u subst)
      (cond
	((eq? t u) subst)               ; quick tests first
	((var? t)
         (let*-and (cond                ; t is a free variable
                     ((var? u)
                      (let*-and (extend-subst t u subst) ((cu (assq u subst)))
                        (unify-free/bound t cu subst)))
                     (else              ; t is free, u is not a var: done
                       (extend-subst t u subst)))
           ((ct (assq t subst)))
           (cond			; t is a bound variable
             ((var? u) 
              (let*-and (unify-free/bound u ct subst) ((cu (assq u subst)))
                (unify-bound/bound ct cu subst)))
             (else                      ; unify bound and a value
               (unify-internals (commitment->term ct) u subst)))))
	((var? u)                       ; t is not a variable...
         (let*-and (extend-subst u t subst) ((cu (assq u subst)))
           (unify-internals (commitment->term cu) t subst)))
        ((and (pair? t) (pair? u))
         (let*-and #f ((subst (unify-internals (car t) (car u) subst)))
           (unify-internals (cdr t) (cdr u) subst)))
        (else (and (equal? t u) subst)))))

(define unify-bound/bound
  (lambda (ct cu subst)
    (unify-internals (commitment->term ct) (commitment->term cu) subst)))


; t-var is a free variable, u can be anything
; This is analogous to get_variable instruction of Warren Abstract Machine
; (WAM).
; This function is not recursive and always succeeds, 
; because unify-free/bound and unify-free/list always succeed.
(define unify-free/any
  (lambda (t-var u subst)
    (cond
      ((eq? u _) subst)
      ((var? u)
       (let*-and (extend-subst t-var u subst) ((cu (assq u subst)))
         (unify-free/bound t-var cu subst)))
      ((pair? u) (unify-free/list t-var u subst))
      (else ; u is not a var and is not a pair: it's atomic
	(extend-subst t-var u subst)))))

; On entrance: t-var is free.
; we are trying to unify it with a bound variable (commitment->var cu)
; Chase the binding chain, see below for comments
; This also works somewhat like union-find...
; This function always succeeds. The resulting substitution is either
; identical to the input one, or differs only in the binding to t-var.
;
; Unlike the previous version of the unifier,
; The following code does not introduce the temp variables *a and *d
; It makes substitutions more complex. Therefore, pruning them
; will take a while, and will break up the sharing. Therefore, we
; don't do any pruning.

(define unify-free/bound
  (lambda (t-var cu s)
    (let loop ((cm cu))
      (let ((u-term (commitment->term cm)))
	(cond
	  ((eq? u-term t-var) s)
	  ((var? u-term)
	    (cond
	      ((assq u-term s) => loop)
	      (else (extend-subst t-var u-term s)))) ; u-term is free here
	  (else (extend-subst t-var u-term s)))))))

; ((and (pattern-var? tree2) (assq tree2 env)) => ; tree2 is a bound var
;        ; binding a free variable to a bound. Search for a substantial binding
;        ; or a loop. If we find a loop tree1->tree2->...->tree1
;        ; then we do not enter the binding to tree1, because tree1 is not
;        ; actually constrained.
;       (lambda (tree2-binding)
; 	(let loop ((binding tree2-binding))
; 	  (cond
; 	    ((eq? tree1 (cdr binding)) env)  ; loop: no binding needed
; 	    ((and (pattern-var? (cdr binding)) (assq (cdr binding) env))
; 	      => loop)
; 	    (else (cons (cons tree1 (cdr binding)) env))))))

; t-var is a free variable, u-value is a proper or improper
; list, which may be either fully or partially grounded (or not at all).
; We scan the u-value for _, and if, found, replace them with fresh
; variables. We then bind t-var to the term.
; This function is not recursive and always succeeds.
;
; We assume that more often than not u-value does not contain _.
; Therefore, to avoid the wasteful rebuilding of u-value, we 
; first scan it for the occurrence of _. If the scan returns negative,
; we can use u-value as it is.

      ; Rebuild lst replacing all anonymous variables with some
      ; fresh logical variables
      ; If lst contains no anonymous variables, return #f
      ; Note that lst itself may be #f -- and yet no contradiction arises.
(define ufl-rebuild-without-anons
  (lambda (lst)
    (cond
      ((eq? lst _) (logical-variable '*anon))
      ((not (pair? lst)) #f)
      ((null? (cdr lst))
	(let ((new-car (ufl-rebuild-without-anons (car lst))))
	  (and new-car (cons new-car '()))))
      (else
	(let ((new-car (ufl-rebuild-without-anons (car lst)))
              (new-cdr (ufl-rebuild-without-anons (cdr lst))))
	  (if new-car
	    (cons new-car (or new-cdr (cdr lst)))
	    (and new-cdr (cons (car lst) new-cdr))))))))

(define unify-free/list
  (lambda (t-var u-value subst)
    (extend-subst t-var
      (or (ufl-rebuild-without-anons u-value) u-value)
      subst)))

