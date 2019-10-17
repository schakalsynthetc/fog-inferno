;;; $Name: rel-2_10 $
;;; $Revision: 1.23 $
;;; $Date: 2006/07/08 13:07:48 $
;;;
;;; (load "/Lisp/cm/src/cm.scm")


;; misc

(require 'srfi) ; cond-expand ... does not work !

(defmacro cond-expand clauses
  (letrec ((errout
	    (lambda (form exp)
	      (slib:error 'cond-expand 'invalid form ': exp)))
	   (feature?
	    (lambda (exp)
	      (cond ((symbol? exp)
		     (or (provided? exp) (eq? exp (software-type))))
		    ((and (pair? exp) (list? exp))
		     (case (car exp)
		       ((not) (not (feature? (cadr exp))))
		       ((or) (if (null? (cdr exp)) #f
				 (or (feature? (cadr exp))
				     (feature? (cons 'or (cddr exp))))))
		       ((and) (if (null? (cdr exp)) #t
				  (and (feature? (cadr exp))
				       (feature? (cons 'and (cddr exp))))))
		       (else (errout 'expression exp)))))))
	   (expand
	    (lambda (clauses)
	      (cond ((null? clauses) (slib:error 'Unfulfilled 'cond-expand))
		    ((not (pair? (car clauses))) (errout 'clause (car clauses)))
		    ((or (eq? 'else (caar clauses)) (feature? (caar clauses)))
		     `(begin ,@(cdar clauses)))
		    (else (expand (cdr clauses)))))))
    (expand clauses)))

(require 'random)
(require 'dynamic-wind)
(require 'logical)
(require 'format)

(define (read-macro-set! char func)
  #f)

(define (os-name) (send (send SmalltalkImage current) platformName))

(require 'pretty-print)

(define pprint pretty-print)

(define (list-set! lis pos val)
  (set-car! (list-tail lis pos) val)
  val)

;; specialize vector-set! so that it can work on lists
;; (else loop withh fail somewhere in scales.scm... ??)
(define-generic vector-set!)
(define-method vector-set! ((lis <pair>) index val)
               (list-set! lis index val))


;;; Keywords.

(define (keyword->symbol kw)
  (send (send kw allButFirst) asSymbol))

(define (string->keyword str)
  (send (string-append ":" str) asSymbol))

(define symbol->keyword string->keyword)

(define (keyword->string kw)
  (send kw allButFirst))

(define (keyword? ob)
  (and (symbol? ob) (eq? (elt ob 0) #\:)))


;; hash tables

;(require 'hash-table)

;(define (make-equal?-hash-table size)
;  (make-hash-table size))

;(define hash-ref (hash-inquirer equal?))

;(define hash-set! (hash-associator equal?))

;(define hash-remove! (hash-remover equal?))

; ??:
;(define (hash-fold fn x table)
;  (hash-table-fold table fn x))    

(define (make-equal?-hash-table size)
  (send Dictionary new))

(define (hash-ref table key)
  (send table at: key))

(define (hash-set! table key value)
  (send table at:put: key value))

(define (hash-remove! table key)
  (send table removeKey: key))

(define (make-equal?-hash-table size)
  (send Dictionary new))

(define (hash-ref table key)
  (send table at: key))

(define (hash-set! table key value)
  (send table at:put: key value))

(define (hash-remove! table key)
  (send table removeKey: key))

(define (hash-fold fn init table)
  (dolist (association (vector->list (send table associations)) init)
    (set! init (fn (send association key) (send association value) init))))
  

;;; level1 (the original level1.scm redefines too many ULisp primitives)

(define (in-package name) #f)

(define-macro (eval-when decl . body) ; noop in scheme
  (if (null? (cdr body))
    (car body)
    `(begin ,@ body)))

(define-macro (incf sym . val)
  `(begin
    (set! ,sym (+ ,sym ,(if (null? val) 1 (car val))))
    ,sym))

(define-macro (decf sym . val)
  `(begin
    (set! ,sym (- ,sym ,(if (null? val) 1 (car val))))
    ,sym))

(define-macro (rotatef sym1 sym2)
  (let ((v (gensym)))
    `(let ((,v ,sym1))
      (set! ,sym1 ,sym2)
      (set! ,sym2 ,v))))

(define-macro (cdr-pop x)
    ;; an equivalent for (pop (cdr ..)) 
  (let ((h (gensym))
	(t (gensym))
	(v (gensym)))
    `(let* ((,h ,x)
	    (,t (cdr ,h))
	    (,v (car ,t)))
      (set-cdr! ,h (cdr ,t))
      ,v)))

(define true #t)
(define false #f)

(define rest cdr)

(define (list-position x lis . arg)
  (let ((test (if (null? arg) (function eq?) (car arg))))
    (do ((tail lis (cdr tail))
         (indx 0 (+ indx 1))
         (flag #f))
        ((or (null? tail) flag) flag)
      (if ( test x (car tail)) (set! flag indx)))))

(define (copy-tree lis)
  (if (pair? lis)
    (cons (copy-tree (car lis))
          (copy-tree (cdr lis)))
    lis))


;;;
;;; property list getting and setting
;;;

(define (list-prop lis prop . def)
  (if (null? lis)
      (if (null? def) #f (car def))
      (if (eq? (car lis) prop)
          (cadr lis)
          (apply list-prop (cddr lis) prop def))))

(define (list-prop-set! lis prop val)
  (if (eq? (car lis) prop)
      (set-car! (cdr lis) val)
      (if (null? (cddr lis))
          (set-cdr! (cdr lis) (list prop val))
          (list-prop-set! (cddr lis) prop val))))


;;;
;;; define-list-struct defines a list struct implemented by a
;;; constructor and getters/setters for each slot:
;;; (define-list-struct foo a (b 1)) 
;;; => (make-foo &key a b)
;;;    (foo-a s) (foo-b s) (foo-a-set! s v) (foo-b-set! s v)
;;;

(define-macro (define-list-struct name . slotspecs)
  (expand-list-struct name slotspecs))

(define (expand-list-struct name slotspecs)
  (letrec ((tailform
            (lambda (pos)
              (case pos
                ((0) 'arg)
                ((1) '(cdr arg))
                ((2) '(cddr arg))
                ((3) '(cdddr arg))
                ((4) '(cddddr arg))
                (else `(list-tail arg ,pos)))))
           (setslotform 
            (lambda (pos) `(set-car! ,(tailform pos) val)))
           (getslotform 
            (lambda (pos) `(car ,(tailform pos))))
           (fillslotform
            (lambda (slot)
              (let ((value #f))
                (when (pair? slot)
                  (set! value (cadr slot))
                  (set! slot (car slot)))
                `(list-prop args ',(symbol->keyword slot) ,value))))
           (make-name
            (lambda (str1 . strs)
              (string->symbol (apply string-append str1 strs)))))

  (let  ((name (symbol->string name))
         (slots (map (lambda (s) (if (symbol? s) s (car s)))
                    slotspecs)))
    `(begin
      ;; create constructor
       (define (, (make-name "make-" name) . args)
        , (cons 'list (map fillslotform slotspecs)))
      ;; create accessor functions
       ,@ (do ((i 0 (+ i 1))
	       (l '())
	       (s slots (cdr s)))
	      ((null? s) (reverse l))
	    (push `(define (,(make-name name "-" 
					 (symbol->string (car s)))
			     arg)
		     ,(getslotform i))
		   l))
      ;; create setter functions
      ,@ (do ((i 0 (+ i 1))
	       (l '())
	       (s slots (cdr s)))
	      ((null? s) (reverse l))
	    (push `(define (,(make-name name "-"
					 (symbol->string (car s))
					 "-set!") 
			     arg val)
		     ,(setslotform i))
		   l))))))

;;;
;;; hash tables
;;;

(define (hash-clear! tabl)
  (hash-fold (lambda (k v r)
               (hash-remove! tabl k)
               r)
             #t
             tabl))


;;;
;;; Numbers and bit twiddling. requires ash, logand, logior, lognot
;;;

(define pi 3.141592653589793)

;;; common lisp floor and round. 

(define (clfloor n . arg)
  (if (null? arg)
    (let ((v (floor n)))
      (values (inexact->exact v) (- n v)))
    (let* ((d (car arg))
	   (v (/ n d))
	   (i (inexact->exact (floor v)))
	   (r (- n (* i d))))
      (values i r))))

(define (clround n . arg)
  (if (null? arg)
    (let ((v (round n)))
      (values (inexact->exact v) (- n v)))
    (let* ((d (car arg))
	   (v (/ n d))
	   (i (inexact->exact (round v)))
	   (r (- n (* i d))))
      (values i r))))

(define mod modulo)

(define rem remainder)

(define %log2 (log 2))

(define (log2 n) (/ (log n) %log2))

(define (logn num base) (/ (log num) (log base)))

(define (signum n)
  (cond ((= n 0) 0)
        ((< n 0) -1)
        (else +1)))

;;;
;;; byte spec
;;;

(define (byte siz pos)
  ;; cache size, position and mask.
  (vector siz pos (ash (- (expt 2 siz) 1) pos)))

(define (byte-size bytespec)
  (vector-ref bytespec 0))

(define (byte-position bytespec)
  (vector-ref bytespec 1))

(define (byte-mask bytespec)
  (vector-ref bytespec 2))

(define (ldb bytespec integer)
  (ash (logand integer (byte-mask bytespec))
       (- (byte-position bytespec))))

(define (dpb integer bytespec into)
  (let ((val (logand integer (ash (byte-mask bytespec) 
                                  (- (byte-position bytespec))))))
    (logior (logand into (lognot (byte-mask bytespec)))
	    (ash val (byte-position bytespec)))))

;;;
;;; cltl2 lambda parameters
;;;
;;;       
;;; with-args (list . decl) . body)
;;; binds variables to values from a list according to cltl2's lambda
;;; parameter declaration syntax. any &optional, &key &rest and &aux
;;; parameters without default values are initiaized to #f. Example: 
;;; cltl2:
;;; (defun foo (a b &optional (c 3) &key d (e a) &aux (f -99))
;;;   (list a b c d e f))
;;; scheme:
;;; (define (foo . args) 
;;;   (with-args (args a b &optional (c 3) &key d (e a) &aux (f a)) 
;;;     (list a b c d e f)))
;;;
;;; (foo 0 1 2 :d 3 :e 4)
;;;

(define-macro (with-args spec . body) 
  ;; spec is (list . lambda-decl)
  (let ((args (gensym ))
        (reqs '())
        (opts '())
        (rest '())
        (keys '())
        (auxs '())
        (aok? #f)			; allow-other-keys
        (vars #f)
        (setk #f)
        (keyc #f)
        (seta #f))

    ;; parse-lambda-list defined in utils.scm...
    (multiple-value-setq (reqs opts rest keys aok? auxs )
                         (parse-lambda-list (cdr spec)))
    ;; each &key entry is represented by a four element list:
    ;; (var default passed? keyword) where var is the variable,
    ;; default is its default value, passed? is a flag set to
    ;; #t if key is passed and keyword is the keyword.
    (do ((tail keys (cdr tail))
         (head (list))
         (b #f)
         (v #f)
         (k #f)
         (l #f))
        ((null? tail)
         (set! keys (reverse! head)))
      (set! b (car tail))
      ;; binding b is ( {var | (key var)} val [var2])
      (cond ((pair? (car b))
             (set! k (caar b))
             (set! v (cadar b)))
            (else
             (set! v (car b))
             (set! k (symbol->keyword v))))
      (set! l (length b)) ;; 2 or 3
      (cond ((= l 2)
             (push (list v (cadr b) (gensym ) k) head))
            ((= l 3)
             (push (list v (cadr b) (caddr b) k) head))
            (else (err "Malformed &key binding: ~s" b))))
    ;; create required arg bindings
    (set! reqs (map (lambda (r)		; r is required par
                      (unless (symbol? r)
                        (err "Required arg not symbol: ~s" r))
		      (let ((v (gensym )))
			`(,r (if (null? ,args)
			       (err "Missing value for required arg ~s"
				    ',r)
			       (let ((,v (car ,args)))
				 (set! ,args (cdr ,args))
				 ,v)))))
		reqs))
    ;; create optional args bindings. optimize the common case of a
    ;; single optional arg
    (set! opts (if (and (null? rest) (null? keys)
			(= (length opts) 1))
		 ;; skip the let and cdring if single optional arg
		 `((, (car (car opts))
                      (if (null? ,args) ,(cadr (car opts)) (car ,args))))
		 (map (lambda (b)
			;; b is (<var> <val>)
			(let ((v (gensym )))
			  `(,(car b)
                             (if (null? ,args)
                               ,(cadr b)
                               (let ((,v (car ,args)))
                                 (set! ,args (cdr ,args))
                                 ,v)))))
		      opts)))
    ;; vars is list of all parameter var bindings.
    (set! vars
          (append! reqs
                   opts
                   ;; rest arg is already a binding. hmmm is this true?
                   rest
                   ;; bind all keyword vars, key-exist flags and aux vars to false
                   (map (lambda (b) (list (car b) #f)) keys)
                   (map (lambda (b) (list (caddr b) #f)) keys)
                   (map (lambda (b) (list (car b) #f)) auxs)))
    ;; setk is a list of setting forms for setting default keyword
    ;; values after keyword processing.
    (set! setk
          (apply append
                 (map (lambda (b) 
                        ;; b is (<var> <val> <v?>) only set <var> to
                        ;; <val> if <v?> is #f, ie user didnt pass the
                        ;; arg
                        (if (eq? (cadr b) #f)
                          (list)
                          `((if (not ,(caddr b))
                              (set! ,(car b) ,(cadr b))))))
                      keys)))
    ;; keyc is a list of case clauses for each keyword:
    ;; ((<keyword>) (set! <var> (cadr <args>) (set! <flag> #t)))
    (set! keyc (map (lambda (b)
                      ;; b is (<var> <val> <flag> <keyw>)
                      `((,(cadddr b) )
                        (set! ,(car b) (cadr ,args))
                        (set! ,(caddr b) #t)))
                    keys))
    ;; seta is a list of aux var setting forms. spliced in just before
    ;; body of with-args
    (set! seta
          (apply append (map (lambda (b)
			      (if (eq? (cadr b) #f)
				(list)
				`((set! ,(car b) ,(cadr b)))))
			auxs)))
    ;; let* so lambda param bindings can reference earlier ones
    `(let* ((,args ,(car spec))
	    ;; splice in all var bindings
	    ,@ vars)
       ;; splice in keyword processing loop.
       ,@ 
       (if (pair? keys)
         (let ((head (gensym)))
           ;; this do loop parses keyword args, each keword has its
           ;; own case clause. signals error for incorrect keys.
           `((do ((,head ,args))
		 ((null? ,args) 
                  ;; loop termination clause sets default values for
                  ;; all keys that were not passed in args and whose
                  ;; default value is not #f.
                  ,@ setk
                  )
               ;; do actions. first is make sure a value exists for
               ;; each keyword
               (if (null? (cdr ,args))
                 (err "Args not keyword format: ~s." ,head))
               ;; current keyword must match a case clause else its bogus
               (case (car ,args)
                 ,@ keyc
                    ;; splice in error trap unless &allow-other-keys.
                    ;; error message includes list of valid keywords.
                    ,@
                    (if (not aok?)
                      `((else
                         (err 
                          "Illegal keyword '~s' in: ~s.~%Valid keywords: ~s"
                          (car ,args) ,head ',(map cadddr keys))))
                      (list)))
               (set! ,args (cddr ,args)))))
         (list))
       ;; spice in &aux params if default value not #f.
       ,@seta
       ;; splice in body of with-args.
       ,@body)))


(define (strip-chars str . args)
  (let ((chars (if (null? args) '(#\space #\tab #\return)
                   (car args))))
    (string-trim-both str (lambda (c) (member c chars)))))

(define (string-read str . args)
  ;; args is: start eoftok
  (let ((len (string-length str))
        (beg (if (null? args) 0 (car args)))
        (eof (if (or (null? args) (null? (cdr args)))
               ':eof
               (car (cdr args)))))
    (call-with-input-string 
     str
     (lambda (sp) ; string port
       ;; advance to starting pos
       (do ((p 0 (+ p 1)))
           ((not (< p beg)) #f)
         (read-char sp))
       (if (not (< beg len))
         (values eof 0)
         (let ((val (read sp)))
           (values (if (eof-object? val) eof val)
                   (port-position sp))))))))

;;;
;;; unix filename twiddling. filenames are just strings.
;;; the level0 files must set the directory character.
;;;

(define (namestring p) p)

(define (filename p) p)

(define (filename-directory file)
  (send FileDirectory dirPathFor: file))

(define (filename-name file)
  (send FileDirectory localNameFor:
        (send FileDirectory baseNameFor: file)))

(define (filename-type file)
  (send FileDirectory extensionFor: file))

(define (merge-filenames p1 p2)
  (let ((pd (filename-directory p1))
	(pn (filename-name p1))
	(pt (filename-type p1)))
    (if (not pd)
      (set! pd (filename-directory p2)))
    (if (not pn)
      (set! pn (filename-name p2)))
    (if (not pt)
      (set! pt (filename-type p2)))
    (apply string-append (or pd "") (or pn "") 
	   (if pt (list "." pt) '()))))

(define (open-file name direction . type)
  (if (eq? direction :output)
    (open-output-file name)
    (open-input-file name)))

(define (close-file fp dir) 
  (if (eq? dir :output)
    (close-output-port fp)
    (close-input-port fp)))

(define (file-form fil)
  (read fil))

(define (file-line fil)
  (read-line fil))

(define (file-eof? x) (eof-object? x))


;;;
;;; Scheme expansion for defobject
;;;

(define (expand-defobject name gvar supers decl pars methods streams)
  ;; slots must be parsed into goops format.
  (let ((slts (map (lambda (s) (parse-slot-spec name s)) decl))
        (opts (if (or pars (not (null? streams)))
                `(:metaclass <parameterized-class>
;                             :event-streams (quote ,streams)
;                             :parameters (quote ,pars)
                             )
                '())))
    `(begin 
      (define-class* ,gvar ,(map class-name->class-var supers)
	, slts :name ',name ,@ opts )
      
      ,@(if (null? pars) (list)
            `((set! (class-parameters ,gvar) (quote ,pars))))
      ,@(if (null? streams) (list)
            `((set! (class-event-streams ,gvar) (quote ,streams))))
      
      (define-method* (make-load-form (obj ,gvar))
        (cons* 'make ', gvar (slot-init-forms obj :eval #t)))
      ,@methods
      (values))))

(define (parse-slot-spec cname spec)
  (let ((acc (lambda (b)
	       (string->symbol (format #f "~a-~a" cname b))))
	(key (lambda (b) (symbol->keyword b)))
	(val #f)
	(name (if (pair? spec) (car spec) spec))
	(spec (if (pair? spec) (list-copy (cdr spec)) (list))))
    
    ;; convert :initarg to :init-keyword. in cltl the slot
    ;; can have any number of initargs and initargs can
    ;; be symbols or keywords. guile only supports a single
    ;; keyword initarg.
    (set! val (memq ':initarg spec))
    (if val
      (do ((tail val (memq ':initarg tail))
           (sofar '() sofar)
           (key #f))
          ((not tail)  #f)
        (if (keyword? (cadr tail))
          (set! key (cadr tail))
          (set! key (and (cadr tail) ;; not #f
                         (symbol->keyword (cadr tail)))))
        ;; remove duplicate initarg or ':initarg #f'
        (if (or (memq key sofar)
                (not key))
          (begin
           ; remove warning
           ;(when (cadr tail)
           ;  (warning "Ignoring duplicate initarg for ~a." 
           ;           name))
           ;; remove from spec. 
           ;(format #t "~%spec=~s tail=~s" spec tail)
           (if (eq? spec tail)
             (begin (set! spec (cddr spec))
                    (set! tail spec))
             (do ((edit spec (cdr edit)))
                 ((eq? (cdr edit) tail)
                  (set-cdr! edit (cdddr edit))))))
          (begin (set-car! tail ':init-keyword)
                 (set-car! (cdr tail) key)
                 (push key sofar)))
        (set! tail (cddr tail)))
      (set! spec (cons ':init-keyword (cons (key name) spec))))
    ;; add accessor if not supplied
    (unless (memq ':accessor spec)
      (set! spec (cons ':accessor (cons (acc name) spec))))
    ;; convert :initform to :init-value
    (set! val (memq ':initform spec))
    (when val
      (set-car! val ':init-value))
    (cons name spec)))

;;;
;;; scheme expansion for write-event
;;;

(define (define-output-method objclassname objclassvar objvar
          fileclassname fileclassvar
          filevar timevar body)
  `(define-method* (write-event (,objvar ,objclassvar)
                               (,filevar ,fileclassvar)
                               ,timevar)
     ,@body))

;;;
;;; Scheme expansion for process macro
;;;

(define (process-stop expr)
  ;; stopprocess is lexical var holding continuation 
  ;; return false
  '(stopprocess #f))

(define (expand-process forms ops)
  (let ((parsed (parse-iteration 'process forms ops))
	(code '())
	(func #f)
	(tests '())
	(done #f))
    (set! tests (loop-end-tests parsed))
    (set! done (process-stop #f))
    (if (loop-finally parsed)
      (set! done `(begin ,@(loop-finally parsed) ,done)))
    (if (not (null? tests))
      (begin
       (if (null? (cdr tests))
	 (set! tests (car tests))
	 (set! tests (cons 'or tests)))
       (set! tests `((if ,tests ,done))))
      (unless (process-code-terminates? (loop-looping parsed)
					(process-stop #f))
	(format #t "Warning: A non-terminating process may have been defined.")
        ))
    (set! func `(lambda ()
		  (call-with-current-continuation
		   (lambda (stopprocess)
		     ,@ tests
		        ,@ (loop-looping parsed)
		           ,@ (loop-stepping parsed)
                              ;;(enqueue *process* *qnext* *qstart* )
		              #t
                              ))))
    (if (and (null? (loop-bindings parsed))
	     (null? (loop-initially parsed)))
      func
      ;; use let* sequential binding
      `(let* ,(loop-bindings parsed)
	 ,@(loop-initially parsed)
	 ,func))))

(define (expand-defprocess forms)
  (let ((args (second forms)))
    (if (not (list? args))
      (err "defprocess arguments not list: ~S" args))
    `(define (,(first forms) ,@args) 
       ,@(cddr forms))))

;;;
;;; scheme expansion for make-midi-message-set!
;;;

(define (make-midi-message-set! getter bytespec)
  (let ((setter (string->symbol
                 (string-append (symbol->string getter)
                                "-set!"))))
    `(define-macro (,setter message value)
       (if (symbol? message)
         (let ((val (gensym)))
           `(let ((,val ,value )) ;
              (set! ,message (dpb ,val ,',bytespec ,message))
              ,val))
         `(dpb ,value ,',bytespec ,message)))))

;;;
;;;
;;;

(define (cm . verbose)
  ;; a no-op for now, 
  (if (or (null? verbose)
          (not (eq? (car verbose) #f)))
    (cm-logo))
  (values))

;;;
;;; u8
;;;

(define (u8vector-write vec fd)
  (do ((i 0 (+ i 1))
       (e (u8vector-length vec)))
      ((= i e) vec)
    (write-byte (u8vector-ref vec i) fd)))

;;; (end of level1)

;;; Object system (copied from stklos.scm)

;(load "lkos") ; port of the STKLOS object system to ULisp/Squeak 



(define-macro (define-generic* . args)
  `(define-generic ,(car args)))

(define-macro (define-method* formals . body)
  `(define-method ,(car formals) ,(cdr formals) ,@body))

(define-macro (define-class* class supers slots . options)
  (let ((cname #f)
        (metac #f)
        (csets (list)))
    ;; substitute :init-form for :init-value
    (do ((tail slots (cdr tail))
         (head (list )))
        ((null? tail) 
         (set! slots (reverse! head)))
      (set! head 
            (cons (if (member ':init-value (cdr (car tail)))
                      (map (lambda (x) (if (eq? x ':init-value)
                                           ':init-form x))
                          (car tail))
                      (car tail))
                  head)))
    (do ((tail options (cddr tail)))
        ((null? tail) #f)
      ;; parse the various metaclass options we use (in guile these
      ;; appear in prop list format after slotdefs)
      (case (car tail)
        ((:name) (set! cname (cadr tail)))
        ((:metaclass) (set! metac (cadr tail)))
        ((:file-types )  ; cm metaclass slot
         (set! csets (list* `(slot-set! ,class 'handles
                                        ,(cadr tail)) csets)))
        ((:output-hook) ; cm metaclass slot
         (set! csets (list* `(slot-set! ,class 'output-hook
                                        ,(cadr tail)) csets)))
        ((:definer) ; cm metaclass slot
         (set! csets (list* `(slot-set! ,class 'definer
                                        ,(cadr tail)) csets)))
        ((:versions) ; cm metaclass slot
         (set! csets (list* `(slot-set! ,class 'versions
                                        ,(cadr tail)) csets)))
        ))
    `(begin
      (define-class ,class ,supers ,slots
                    ,@ (if metac (list :metaclass metac) (list)))
      (slot-set! ,class 'name ,cname)
      ;; set slots from metaclass
      ,@csets)))

(define-macro (find-class* name . args)
  `(find-class (string->symbol (string-append "<" ,name ">"))))

(define-macro (define-object-printer* args . body)
  `(define-method write-object , args ,@ body))

;; has class-subclasses

(define (slot-definition-initargs slot)
  (do ((opts (slot-definition-options slot) (cddr opts))
       (args (list)))
      ((null? opts)
       (reverse! args))
    (if (eq? (car opts) ':init-keyword)
      (set! args (cons (cadr opts) args)))))

(define class-direct-superclasses class-direct-supers)

(define slot-definition-initform slot-definition-init-form)

(define (slot-getter-form obj slot)
  `(slot-ref ,obj ',slot))

(define (slot-setter-form obj slot val)
  `(slot-set! ,obj ',slot ,val))

; (define-class <foo> () ((a :accessor foo-a :init-keyword :a :init-keyword :z)))
; (describe (make <foo> :z 22))
; (describe (make <foo> :z 22))   ; allows multi keywords but others do not set
; (slot-ref <foo> 'name)
; (slot-set! <foo> 'name 'freddy)  ; can change class name
; (find-class 'freddy)
; (describe <foo>)



;                                   Chicken Gauche  Guile  Stklos
;(make class . args)                        y       y       y
;(initialize class inits)                   y       y       y
;(class-of obj)                             y       y       y
;(is-a? obj class)                          y       y       y
;(slot-ref obj slot)                        y       y       y
;(slot-set obj slot val)                    y       y       y
;(class-name class)                         y       y       y
;(class-slots class)                        y       y       x
;(class-direct-subclasses class)            y       y       y
;(class-direct-superclasses class)          *       y       *
;(slot-definition-name slot)                y       y       y
;(slot-definition-initargs slot)            n       n       n
;(slot-definition-initform slot)            n       n       n

; SCHEMES MUST PROVIDE THESE OOP METHODS, starred methods are to
; avoid overriding an existing implementation and/or normalize syntax
; issues between schemes
;
;;(define-class* ...)
;;(define-method* ...)
;;(define-generic* ...)
;(find-class* name)                
;;(define-object-printer* object port)
;(class-subclasses class) 
;;(slot-getter-form obj slot)
;;(slot-setter-form obj slot val)


;; loading Common Music

(define *cm-directory* #f)

(let ((this-file #f)
      (load-path "src/")
      (file-list '("loop" ;"level1" 
                   "utils" "mop" "objects"
		   "data" "scales" "spectral" "patterns"
                   "io" "scheduler" "sco" "clm" "clm2"
                   "midi1" "midi2" "midi3"
		   "osc" "sc" "pm" "rt"
                   )))


  (let load-cm ((tail file-list))
       (if (null? tail) 
           #f
           (let ((file (string-append load-path (car tail))))
             (display (string-append "; loading " file))
             (newline)
             (load file)
             (load-cm (cdr tail)))))

  ;; load user init file if it exists
 ; (let* ((this (pwd))
 ;        (home (cd))
 ;        (init (string-append home "/.cminit.lisp")))
 ;   (if (file-exists? init)
 ;       (load init))
 ;   (cd this))

  )                                     ; end let

