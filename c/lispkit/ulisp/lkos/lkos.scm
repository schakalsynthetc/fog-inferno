;;;; the LispKit Object System, ported from:

;;;; object.stk	-- -- A variation of the Gregor Kickzales Tiny CLOS for STklos
;;;; 
;;;; Copyright © 1993-2006 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;;; 
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, 
;;;; USA.
;;;; 
;;;;           Author: Erick Gallesio [eg@unice.fr]
;;;;    Creation date: 20-Feb-1994 21:09
;;;; Last file update: 19-Dec-2006 10:11 (eg)



;;; add-ons / overrides for ULisp:

(define (void . args) (smalltalk-class 'LKOSVoid))

(define new-global void)
(define current-module void)

(define closure? procedure?)                 ;; ??
(define (%procedure-arity proc)               ;; à revoir... usage très spécifique
  (length (cadr (send proc lambdaForm))))

(define (symbol-value* symbol . ignored) 
        (if (boundp symbol) (symbol-value symbol) #f))

;; replaced with a primitive
;(defun key-get (plist symbol &optional default)
;        (or (plist-get plist symbol) default))

(require 'srfi-1) ; for remove!
(defun delete! (val lst &optional proc)
	(if (null proc) (setq proc equal?))
	(remove! (lambda (x) (apply proc (list val x))) lst))

;; from lib/runtime.stk

(define (map* fn . l) 		; A map which accepts dotted lists (arg lists  
  (cond 			; must be "isomorph"
   ((null? (car l)) '())
   ((pair? (car l)) (cons (apply fn      (map car l))
			  (apply map* fn (map cdr l))))
   (else            (apply fn l))))

(define (for-each* fn . l) 	; A for-each which accepts dotted lists (arg lists  
  (cond 			; must be "isomorph"
   ((null? (car l)) '())
   ((pair? (car l)) (apply fn (map car l)) (apply for-each* fn (map cdr l)))
   (else            (apply fn l))))


;=============================================================================
; from STKLOS object.c and object.h 
; (see also the corresponding code in Squeak class LKOSInstance)


(define lkos-slot-for-name 1)
(define lkos-slot-for-direct-supers 2)
(define lkos-slot-for-direct-slots 3)
(define lkos-slot-for-direct-subclasses 4)
(define lkos-slot-for-direct-methods 5)
(define lkos-slot-for-cpl 6)
(define lkos-slot-for-slots 7)
(define lkos-slot-for-nfields 8)
(define lkos-slot-for-getters-n-setters 9)
(define lkos-slot-for-redefined 10)

(define lkos-slot-for-methods 2)

(define lkos-slot-for-generic-function 1)
(define lkos-slot-for-specializers 2)
(define lkos-slot-for-procedure 3)



(define (%symbol-define name value module) 
  (eval `(define ,name ,value))
  value)


(define (%make class type more)

  (let ((z (send LKOSInstance makeInstanceClass:size:type:lisp: 
                 class 
                 10 ;; (length (send class getSlot: #.lkos-slot-for-slots)) 
                 type ;; (if (eq? type 'generic) 'generic 'instance))))
                 (this-lisp))))
    (if (eq? type 'generic)
      (send! z 
             (setSlot:to: #.lkos-slot-for-name more) 
             (setSlot:to: #.lkos-slot-for-methods ())))
    (if (or (eq? type 'method)
            (eq? type 'simple-method)
            (eq? type 'accessor-method))
      (send! z 
             (setSlot:to: #.lkos-slot-for-generic-function (car more)) 
             (setSlot:to: #.lkos-slot-for-specializers (cadr more)) 
             (setSlot:to: #.lkos-slot-for-procedure (caddr more)))) 
    (if (eq? type 'class)
      (send! z 
             (setSlot:to: #.lkos-slot-for-name (car more)) 
             (setSlot:to: #.lkos-slot-for-direct-supers (cadr more)) 
             (setSlot:to: #.lkos-slot-for-direct-slots (caddr more))))
    z))

;; (define (%allocate-instance class)
;;   (send LKOSInstance makeInstanceClass:size:type:lisp: 
;;                  class 
;;                  (send class getSlot: #.lkos-slot-for-nfields) 
;;                  (cond
;;                    ((eq? class <generic>) 'generic)
;;                    ((eq? class <simple-method>) 'simple-method)
;;                    ((eq? class <accessor-method>) 'accessor-method)
;;                    (else 'instance))
;;                  (this-lisp)))


(define *slots-of-class*
  '(name direct-supers direct-slots direct-subclasses direct-methods cpl slots nfields getters-n-setters redefined))

(define <class> 
  (send LKOSInstance makeInstanceClass:size:type:lisp: () 10 'instance (this-lisp)))

(let ((accessors (send LKOSInstance computeGettersNSetters: *slots-of-class*)))
  (send! <class> 
       (accessors: accessors)
       (iClass: <class>)
       (setSlot:to: #.lkos-slot-for-name '<class>) 
       (setSlot:to: #.lkos-slot-for-direct-supers ()) 
       (setSlot:to: #.lkos-slot-for-direct-slots *slots-of-class*) 
       (setSlot:to: #.lkos-slot-for-direct-subclasses ()) 
       (setSlot:to: #.lkos-slot-for-direct-methods ()) 
       (setSlot:to: #.lkos-slot-for-cpl ()) 
       (setSlot:to: #.lkos-slot-for-slots *slots-of-class*) 
       (setSlot:to: #.lkos-slot-for-nfields (length *slots-of-class*)) 
       (setSlot:to: #.lkos-slot-for-getters-n-setters accessors) 
       (setSlot:to: #.lkos-slot-for-redefined #f)))

(define <top> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: 
        <class> '<top> () () (this-lisp)))

(define <object> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: 
        <class> '<object> (list <top>) () (this-lisp)))

(send <object> setSlot:to: #.lkos-slot-for-direct-subclasses (list <class>))
(send <class> setSlot:to: #.lkos-slot-for-direct-supers (list <object>))
(send <class> setSlot:to: #.lkos-slot-for-cpl (list <class> <object> <top>))

(define <procedure-class> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<procedure-class> (list <class>) () (this-lisp)))

(define <entity-class> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<entity-class> (list <procedure-class>) () (this-lisp)))

(define <method> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<method> (list <object>) 
        '(generic-function specializers procedure) (this-lisp)))

(define <simple-method> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<simple-method> (list <method>) () (this-lisp)))

(define <accessor-method> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<accessor-method> (list <simple-method>) () (this-lisp))) 

(define <generic> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <entity-class> '<generic> (list <object>) 
        '(name methods) (this-lisp)))


(define <boolean> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<boolean> (list <top>) () (this-lisp))) 

(define <char> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<char> (list <top>) () (this-lisp))) 

(define <symbol> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<symbol> (list <top>) () (this-lisp))) 

(define <pair> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<pair> (list <top>) () (this-lisp))) 

(define <null> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<null> (list <top>) () (this-lisp))) 

(define <string> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<string> (list <top>) () (this-lisp))) 

(define <vector> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<vector> (list <top>) () (this-lisp))) 

(define <number> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<number> (list <top>) () (this-lisp))) 

(define <complex> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<complex> (list <number>) () (this-lisp)))

(define <real> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<real> (list <complex>) () (this-lisp))) 

(define <rational> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<rational> (list <real>) () (this-lisp))) 

(define <integer> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<integer> (list <rational>) () (this-lisp))) 

(define <procedure> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<procedure> (list <top>) () (this-lisp))) 

(define <unknown> 
  (send LKOSInstance basicMakeClass:name:dsupers:dslots:lisp: <class> '<unknown> (list <top>) () (this-lisp))) 

(define (instance? object) 
  (send object isLKOSInstance))

;; (define (test-change-class obj)
;;   (let* ((class (send obj iClass))
;;          (new-class (send class getSlot: #.lkos-slot-for-redefined)))
;;     (if new-class
;;       (change-object-class obj class new-class))))

;; (define (%get-slot-value classe obj slot-name)
;;   (%get-slot-value-from-accessors (send obj accessors) slot-name obj))
    
;; (define (%get-slot-value-from-accessors accessors slot-name obj)
;;   (if (null accessors) 
;;     (error "slot missing")
;;     (if (eq? (caar accessors) slot-name)
;;       (let ((a (cddar accessors)))
;;         (if (integer? a)
;;           (send obj getSlot: a)
;;           (apply (car a) (list obj))))
;;       (%get-slot-value-from-accessors (cdr accessors) slot-name obj))))

;; (define (slot-ref obj slot-name)
;;   (test-change-class obj)
;;   (%get-slot-value (send obj iClass) obj slot-name))

;; (define (%set-slot-value classe obj slot-name value)
;;   (%set-slot-value-from-accessors (send obj accessors) slot-name obj value))
    
;; (define (%set-slot-value-from-accessors accessors slot-name obj value)
;;   (if (null accessors) 
;;     (error "slot missing")
;;     (if (eq? (caar accessors) slot-name)
;;       (let ((a (cddar accessors)))
;;         (if (integer? a)
;;           (send obj setSlot:to: a value)
;;           (apply (cadr a) (list obj value))))
;;       (%set-slot-value-from-accessors (cdr accessors) slot-name obj value))))

;; (define (slot-set! obj slot-name value)
;;   (test-change-class obj)
;;   (%set-slot-value (send obj iClass) obj slot-name value))


(define (slot-ref obj slot-name)
  (send obj slotRef: slot-name))

(define (slot-set! obj slot-name value)
  (send obj slotSet:to: slot-name value))


(set! (setter slot-ref) slot-set!)

;; (define (class-of object)
;;   (if (instance? object)
;;       (progn
;;         (send object testChangeClass)
;;         (send object iClass))
;;     ;;; mapping from Squeak classes to STKLOS classes: 
;;     (cond
;;       ((send object isSymbol) <symbol>)   
;;       ((send object isConsCell) <pair>)    
;;       ((send object isNil) <null>)
;;       ((send object isProcedure) <procedure>)
;;       ((or (eq? object #t) (eq? object #f)) <boolean>)
;;       ((send object isCharacter) <char>)
;;       ((send object isString) <string>)
;;       ((send object isArray) <vector>) 
;;       ((send object isInteger) <integer>) 
;;       ((send object isFraction) <rational>)   
;;       ((send object isNumber) <real>)   
;; ;      ((send object isNumber) <number>)    
;;       ((send object isKindOf: (smalltalk-class 'Complex)) <complex>)  
;;       (else <unknown>))))

(define (class-of object)
  (send object lkosClass: (this-lisp)))


#|   note that the original STKLOS class hierarchy is a bit different:
  
  mk_cls(&Boolean,      "<boolean>",    Class,           Top,       STk_nil);
  mk_cls(&Char,         "<char>",       Class,           Top,       STk_nil);
  mk_cls(&Liste,        "<list>",       Class,           Top,       STk_nil);
  mk_cls(&Pair,         "<pair>",       Class,           Liste,     STk_nil);
  mk_cls(&Null,         "<null>",       Class,           Liste,     STk_nil);
  mk_cls(&String,       "<string>",     Class,           Top,       STk_nil);
  mk_cls(&Symbol,       "<symbol>",     Class,           Top,       STk_nil);
  mk_cls(&Vector,       "<vector>",     Class,           Top,       STk_nil);
  mk_cls(&Number,       "<number>",     Class,           Top,       STk_nil);
  mk_cls(&Complex,      "<complex>",    Class,           Number,    STk_nil);
  mk_cls(&Real,         "<real>",       Class,           Complex,   STk_nil);
  mk_cls(&Rational,	"<rational>",	Class,		 Real,	    STk_nil);
  mk_cls(&Integer,      "<integer>",    Class,           Rational,  STk_nil);
  mk_cls(&Keyword,      "<keyword>",    Class,           Top,       STk_nil);
  mk_cls(&Eof,          "<eof>",        Class,           Top,       STk_nil);
  mk_cls(&Struct,       "<struct>",     Class,           Top,       STk_nil);
  mk_cls(&Struct_type,  "<struct-type>",Class,           Top,       STk_nil);
  mk_cls(&Cond, 	"<condition>",  Class,		 Top,	    STk_nil);
  mk_cls(&Cond_type, 	"<condition-type>",Class,	 Top,	    STk_nil);
  mk_cls(&UnknownClass, "<unknown>",    Class,           Top,       STk_nil);
  mk_cls(&Procedure,    "<procedure>",  Procedure_class, Top,       STk_nil);
|# 

(define (subclass? o1 o2)
  (memq o2 (slot-ref o1 #.lkos-slot-for-cpl)))

(define (class? object)
  (and (instance? object)
       (subclass? (class-of object) <class>)))

(define (method? object)
  (and (instance? object)
       (subclass? (class-of object) <method>)))

(define (generic? object)
  (and (instance? object)
       (subclass? (class-of object) <generic>)))


;; (define (%initialize-object object initargs)
;;   (if (not (instance? object)) (error "bad instance"))
;;   (if (not (or (pair? initargs) (null? initargs)))
;;     (error "bad initialization list"))
;;   (let* ((class (send object iClass))
;;          (get-n-set (send class getSlot: #.lkos-slot-for-getters-n-setters))
;;          (slots (send class getSlot: #.lkos-slot-for-slots)))
;;     (while (not (null? slots))
;;       (let ((slot-name (car slots))
;;             (slot-value (void)))
;;         (if (pair? slot-name)
;;           (let ((tmp (key-get (cdr slot-name) :init-keyword (void))))
;;             (set! slot-name (car slot-name))
;;             (if (not (eq? tmp (void)))
;;               (setq slot-value (key-get initargs tmp (void))))))
;;         (if (not (eq? slot-value (void)))
;;              (slot-set! object slot-name slot-value)
;;           (let ((tmp (cadar get-n-set)))
;;             (if tmp 
;;               (%set-slot-value class object slot-name (apply tmp (list object)))))))
;;       (set! get-n-set (cdr get-n-set))
;;       (set! slots (cdr slots))))
;;   object)


(define (slot-bound? object slot-name)
  (send object testChangeClass)
  (not (eq? (send object getSlotValue: slot-name) (void))))

(define (slot-exists? object slot-name)
  (send object testChangeClass)
  (send object testSlotExistence: slot-name))

;; (define (test-slot-existence accessors slot-name)
;;   (if (null? accessors) #f
;;       (or (eq? (caar accessors) slot-name)
;;           (test-slot-existence (cdr accessors) slot-name))))

(define (slot-bound-using-class? class object slot-name)
  (not (eq? (send object getSlotValue: slot-name) (void))))

(define (slot-exists-using-class? class object slot-name)
  (send object testSlotExistence: slot-name))

;; (define (%modify-instance old new)
;;   (if (not (and (instance? old) (instance? new)))
;;        (error "both parameters must be instances")
;;     (send old become: new)))

;; (define (%fast-slot-ref obj index)
;;   (send obj getSlot: index))

;; (define (%fast-slot-set! obj index value)
;;   (send obj setSlot:to: index value))

(define %slot-ref slot-ref)    ;;;; à revoir (avec tous les checks manquants)

(define (slot-ref-using-class class object slot-name)
  (send object getSlotValue: slot-name))

(define (slot-set-using-class! class object slot-name value)
  (send object setSlotValue:to: slot-name value))

;======== (end of STKLOS object.c, object.h transcription)


;============================ original and mostly unchanged STKLOS code below:
;
; search "(Stef)" to locate changes
;========


(define class-redefinition (void))	;; forward declaration

;=============================================================================
;
;			      U t i l i t i e s
;
;=============================================================================

(define (%error-bad-class who obj)   (error who "bad class ~S" obj))
(define (%error-bad-generic who obj) (error who "bad generic function ~S" obj))
(define (%error-bad-method who obj)  (error who "bad method ~S" obj))

(define make-closure eval)

(define (specializers l)
  (cond
   ((null? l) '())
   ((pair? l) (cons (if (pair? (car l)) (cadar l) '<top>)
 		    (specializers (cdr l))))
   (else      '<top>)))
 
(define (formals l)
  (if (pair? l)
      (cons (if (pair? (car l)) (caar l) (car l)) (formals (cdr l)))
      l))
 

;; (define (declare-slots slots)
;;   ;; This is a *HUGE* *HACK*. Since STklos compiler displays the
;;   ;; unknown symbols, slots accessors are not known at compilation
;;   ;; time and yield a message. So, we pre-declare them here.
;;   ;; In the abolute this code is false, since the keywords :accessor
;;   ;; :getter and :setter can have a different meaning than the usual
;;   ;; one for a peculiar meta-class. Anyway, the only risk is to
;;   ;; trust that a symbol is defined whereas it is not. 
;;   (for-each (lambda (s)
;; 	      (let ((getter   (slot-definition-getter   s))
;; 		    (setter   (slot-definition-setter   s))
;; 		    (accessor (slot-definition-accessor s)))
;; 		(when getter   (new-global getter))
;; 		(when setter   (new-global setter))
;; 		(when accessor (new-global accessor))))
;; 	    slots)
;;   slots)


;--------------------------------------------------
(define (make class . l)	;; A temporary version used for bootstrapping 
  (cond
    ((eq? class <generic>) (%make class
				  'generic
				  (key-get l :name '???)))
    ((eq? class <method>) (%make class
				  'method
				  (list (key-get l :generic-function #f)
					(key-get l :specializers     '())
					(key-get l :procedure        '()))))
    (else       	    (error 'basic-make "cannot make ~S with ~S" class l))))

;=============================================================================
;
; 			      Access to Meta objects
;
;=============================================================================

;// ;;;
;// ;;; Methods
;// ;;;
;// (define-method method-body ((m <method>))
;//   (let* ((spec (map class-name (slot-ref m #.lkos-slot-for-specializers)))
;// 	 (proc (procedure-body (slot-ref m #.lkos-slot-for-procedure)))
;// 	 (args (cdadr proc))
;// 	 (body (cddr proc)))
;//     (list* 'method (map list args spec) body)))
;// 

;;;
;;; Classes
;;;
(define (class-name C)
  (if (class? C) (slot-ref C #.lkos-slot-for-name) (%error-bad-class 'class-name C)))

(define (class-direct-supers C)
  (if (class? C) 
      (slot-ref C #.lkos-slot-for-direct-supers) 
      (%error-bad-class 'class-direct-supers C)))

(define (class-direct-slots C)
  (if (class? C) 
      (slot-ref C #.lkos-slot-for-direct-slots) 
      (%error-bad-class 'class-direct-slots C)))

(define (class-direct-subclasses C)
  (if (class? C) 
      (slot-ref C #.lkos-slot-for-direct-subclasses) 
      (%error-bad-class 'class-direct-subclasses C)))

(define (class-direct-methods C)
  (if (class? C) 
      (slot-ref C #.lkos-slot-for-direct-methods) 
      (%error-bad-class 'class-direct-methods C)))

(define (class-precedence-list C)
  (if (class? C)
      (slot-ref C #.lkos-slot-for-cpl)
      (%error-bad-class 'class-precedence-list C)))

(define (class-slots C)
  (if (class? C)
      (slot-ref C #.lkos-slot-for-slots)
      (%error-bad-class 'class-slots C)))

;;;
;;; Slots
;;;
(define (slot-definition-name s)
  (if (pair? s) (car s) s))

(define (slot-definition-options s)
  (and (pair? s) (cdr s)))

(define (slot-definition-allocation s)
  (if (symbol? s)
      :instance
      (key-get (cdr s) :allocation :instance)))

(define (slot-definition-getter s)
  (and (pair? s) (key-get (cdr s) :getter #f)))

(define (slot-definition-setter s)
  (and (pair? s) (key-get (cdr s) :setter #f)))

(define (slot-definition-accessor s)
  (and (pair? s) (key-get (cdr s) :accessor #f)))

(define (slot-definition-init-form s)
  (if (pair? s)
      (key-get (cdr s) :init-form (void))
      (void)))

(define (slot-definition-init-keyword s)
  (and (pair? s) (key-get (cdr s) :init-keyword #f)))

(define (slot-init-function c s)
  (let ((s (slot-definition-name s)))
    (cadr (assq s (slot-ref c #.lkos-slot-for-getters-n-setters)))))

(define (class-slot-definition class slot-name)
  (assq slot-name (class-slots class)))

;;;
;;; Generic functions
;;;
(define (generic-function-name gf)
  (if (generic? gf)
      (slot-ref gf #.lkos-slot-for-name) 
      (%error-bad-generic 'generic-function-name gf)))

(define (generic-function-methods gf)
  (if (generic? gf)
      (slot-ref gf #.lkos-slot-for-methods)
      (%error-bad-generic 'generic-function-methods gf)))

;;;
;;; Methods
;;;
(define (method-generic-function m)
  (if (method? m)
      (slot-ref m #.lkos-slot-for-generic-function)
      (%error-bad-method 'method-generic-function m)))

(define (method-specializers m)
  (if (method? m)
      (slot-ref m #.lkos-slot-for-specializers)
      (%error-bad-method 'method-specializers m)))

(define (method-procedure m)
  (if (method? m)
      (slot-ref m #.lkos-slot-for-procedure)
      (%error-bad-method 'method-procedure m)))

;=============================================================================

;;
;; is-a?
;;
(define (is-a? obj class)
  (and (memq class (class-precedence-list (class-of obj))) #t))

;;
;; Find-class
;;
(defun find-class (name &optional default)
  (let ((cls (if (symbol? name)
		 (symbol-value* name (current-module) #f)
		 name)))
    (if (is-a? cls <class>)
	cls
        default)))

;;
;; %compute-slots
;;
(define (%compute-slots C)
  (define (remove-duplicate-slots l res)
    (if (null? l) 
	res
	(let ((s (slot-definition-name (car l))))
	  (unless (symbol? s)
	    (error 'compute-slots "bad slot name ~S" s))
	  (remove-duplicate-slots (cdr l) 
				  (if (assq s res) res (cons (car l) res))))))

  (define (build-slots-list cpl res)
    (if (null? cpl) 
	(remove-duplicate-slots (reverse res) '())
	(build-slots-list (cdr cpl) 
			  (append (class-direct-slots (car cpl)) res))))
  (define (sort-slots l)
    ;; Slots are sorted such that instance slots are always before other slots.
    ;; So, virtual slots are placed fist and, for instance, virtual slots can have
    ;; an initial value which depends of real slots.
    ;; Note that the following "sort" just displace non-instance slots at the
    ;; end keeping initial order (i.e. we don't use the STklos sort procedure
    ;; since it is not stable.
    (let Loop ((l l) (instance '()) (other '()))
      (cond
	((null? l)
	   (append (reverse! instance) (reverse! other)))
	((eq? (slot-definition-allocation (car l)) :instance)
	   (Loop (cdr l) (cons (car l) instance) other))
	(else
	   (Loop (cdr l) instance (cons (car l) other))))))
  
  (sort-slots (build-slots-list (class-precedence-list C) '())))


;=============================================================================
;
; 			M e t a c l a s s e s   s t u f f
;
;=============================================================================
(define ensure-metaclass-with-supers
  (let ((table-of-metas '()))
    (lambda (meta-supers)
      (let ((entry (assoc meta-supers table-of-metas)))
	(if entry
	    ;; Found a previously created metaclass
	    (cdr entry)
	    ;; Create a new meta-class which inherit from "meta-supers"
	    (let ((new (make <class> :dsupers meta-supers
			             :slots   '()
				     :name   (gensym "metaclass"))))
	      (set! table-of-metas (cons (cons meta-supers new) table-of-metas))
	      new))))))

(define (ensure-metaclass supers)
  (if (null? supers)
      (find-class <class>)
      (let* ((all-metas (map (lambda (x)
			       (if (is-a? x <class>)
				   x
				   (class-of (find-class x))))
			     supers))
	     (all-cpls  (apply append
			       (map (lambda (m) (cdr (class-precedence-list m))) 
				    all-metas)))
	     (needed-metas '()))
	;; Find the most specific metaclasses.  The new metaclass will be
	;; a subclass of these.
	(for-each
	 (lambda (meta)
	   (when (and (not (member meta all-cpls)) (not (member meta needed-metas)))
	     (set! needed-metas (append needed-metas (list meta)))))
	 all-metas)
	;; Now return a subclass of the metaclasses we found.
	(if (null? (cdr needed-metas))
	    (car needed-metas)  ; If there's only one, just use it.
	    (ensure-metaclass-with-supers needed-metas)))))

;=============================================================================
;
; 			D e f i n e - c l a s s
;
;=============================================================================
;==== Define-class
(define-macro (define-class name supers slots . options)
  `(define ,name 
      (ensure-class
        ',name					; name
	',supers				; supers
;	',(declare-slots slots)		; slots 
        ',slots
	,(or (key-get options :metaclass #f)	; metaclass
	     `(ensure-metaclass ',supers))
	,@options)))

;==== Ensure-class
(define (ensure-class name supers slots metaclass . options)

  (define (find-duplicate l)	; find a duplicate in a list; #f otherwise
    (cond
       ((null? l)		#f)
       ((memq (car l) (cdr l))	(car l))
       (else 			(find-duplicate (cdr l)))))
 
  (let ((supers (if (null? supers) 
		    (list <object>)
		    (map find-class supers))))
    ;; Verify that all direct slots are different and that we don't inherit
    ;; several time from the same class
    (let ((tmp (find-duplicate supers)))
      (when tmp
	(error 'define-class "super class ~S is duplicated in class ~S" tmp name)))
    (let ((tmp (find-duplicate (map slot-definition-name slots))))
      (when tmp
	(error 'define-class "slot ~S is duplicated in class ~S" tmp name)))

    ;; Everything seems correct, build the class
    (let ((old (find-class name #f))
	  (cls (apply make metaclass :dsupers supers :slots slots 
		      		     :name name options)))
      (when old (class-redefinition old cls))
      cls)))

;=============================================================================
;
; 			D e f i n e - g e n e r i c
;
;=============================================================================

; ==== Define-generic
(define-macro (define-generic gf)
  `(define ,gf (ensure-generic-function ',gf)))

;==== Ensure-generic-function
(define (ensure-generic-function name)
  (let ((old (symbol-value* name (current-module) #f)))
    (if (generic? old)
	old
	(%symbol-define name 
			(make <generic> :name name 
			      		:default (and (procedure? old) old))
			(current-module)))))

;//(define (ensure-generic-function name)
;//  (let ((old (symbol-value name (current-module) #f)))
;//    (if (generic? old)
;//	old
;//	(eval `(begin
;//		 (define ,name ,(make <generic>
;//				  :name name 
;//				  :default (and (procedure? old) old)))
;//		 ,name)))))
;//

;=============================================================================
;
; 			D e f i n e - m e t h o d
;
;=============================================================================

;==== Add-method!
(define (add-method-in-classes! m)
  ;; Add method in all the classes which appears in its specializers list
  (for-each* (lambda (x)
	       (let ((dm (class-direct-methods x)))
		 (unless (memq m dm)
		   (slot-set! x #.lkos-slot-for-direct-methods (cons m dm)))))
	     (method-specializers m)))

(define (remove-method-in-classes! m)
  ;; Remove method in all the classes which appears in its specializers list
  (for-each* (lambda (x)
	       (slot-set! x #.lkos-slot-for-direct-methods (delete! m (class-direct-methods x)
						     eq?)))
	      (method-specializers m)))

(define (compute-new-list-of-methods gf new)
  (let ((new-spec (method-specializers new))
	(methods  (generic-function-methods gf)))
    (let Loop ((l methods))
      (if (null? l)
	  (cons new methods)
	  (if (equal? (method-specializers (car l)) new-spec)
	      (begin 
		;; This spec. list already exists. Remove old method from dependents
		(remove-method-in-classes! (car l))
		(set-car! l new) 
		methods)
	      (Loop (cdr l)))))))

;;
;; Add-method!
;;
(define (add-method! gf m)
  (slot-set! gf #.lkos-slot-for-methods (compute-new-list-of-methods gf m))
  (add-method-in-classes! m)
  m)


(defun ensure-method (gf args body &optional kind-of-method)
  (let* ((new  `(method ,args ,@body))                          ;; removed extended-lambda->lambda (Stef)
	 (args (cadr new))
	 (body (cddr new)))
    `(make ,(if (null kind-of-method) '<method> kind-of-method)
          :generic-function ,gf
	  :specializers     (map* find-class ',(specializers args))
	  :procedure        (let ((next-method (void)))
			      (lambda ,(formals args)
				,@body)))))


;====  Method
(define-macro (method args . body)
  (ensure-method #f args body))


; ==== Define-method
(define-macro (define-method name args . body)
  (let ((gf (gensym "gf")))
    `(let ((,gf (ensure-generic-function ',name)))
       (add-method! ,gf ,(ensure-method gf args body))
       (values (void) ',name))))


;=============================================================================
;
; 			    Standard methods   
; 			used by the C runtime
;
;=============================================================================

;==== Methods to compare objects
(define-method object-eqv?   (x y) #f)
(define-method object-equal? (x y) (eqv? x y))

 
;==== Methods to display/write an object
 
;     Code for writing objects must test that the slots they use are
;     bound. Otherwise a slot-unbound method will be called and will 
;     conduct to an infinite loop.
 
(require 'format)

;; Write (modified by Stef)
(define-method write-object (o port)
  (prin1: "#[instance]" port))
 
(define-method write-object ((o <object>) port)
  (let ((class (class-of o)))
    (if (slot-bound? class #.lkos-slot-for-name) 
      (prin1: (format "#[~A]" (class-name class)) port)
      (next-method))))

(define-method write-object((class <class>) port)
  (let ((meta (class-of class)))
    (if (and (slot-bound? class #.lkos-slot-for-name) (slot-bound? meta #.lkos-slot-for-name)) 
      (prin1: (format "#[~A ~A]" (class-name meta) (class-name class)) port)
      (next-method))))

(define-method write-object((gf <generic>) port)
  (let ((meta (class-of gf)))
    (if (and (slot-bound? gf #.lkos-slot-for-name) (slot-bound? meta #.lkos-slot-for-name)
             (slot-bound? gf #.lkos-slot-for-methods))
      (prin1: (format "#[~A ~A (~A)]" 
                     (class-name meta)
                     (generic-function-name gf)
                     (length (generic-function-methods gf))) port)
      (next-method))))

;; Display (do the same thing as write by default)
(define-method display-object (o port) 
  (write-object o port))

;; Single argument versions (Stef):

(define-method display-object (o) 
               (display-object o (current-output-port)))

(define-method write-object (o) 
               (write-object o (current-output-port)))

;; Redefining ULisp write and display (Stef):

(define (display o . rest) 
  (if (instance? o)
      (apply display-object o rest)
    (apply display: o rest)))

(define (write o . rest) 
  (if (instance? o)
      (apply write-object o rest)
    (apply prin1: o rest)))


;==== Slot access

;; Avoid printing object itself in the error message, because it might
;; cause an infinite loop (via write-object method). Problem signaled
;; by Shiro Kawai <shiro@acm.org>.

(define-method slot-unbound ((c <class>) (o <object>) s)
  (error "slot ~S is unbound (in an object of class ~S)" s c))

(define-method slot-missing ((c <class>) (o <object>) name . value)      ;; ?? (Stef)
  (error "no slot with name `~S' (in an object of class ~S)" name c))

; ==== Methods for the possible error we can encounter when calling a gf

(define-method no-next-method ((gf <generic>) method args)
  (error "no next method for ~S in call ~S" 
	 method (cons (generic-function-name gf) args)))

(define-method no-applicable-method ((gf <generic>) args)
  (error "no applicable method for ~S\nin call ~S"
	 gf (cons (generic-function-name gf) args)))

(define-method no-method ((gf <generic>) args)
  (error "no method defined for ~S"  gf))


;// (define-macro (next-method-exists?)
;//  `((with-module STklos %next-method-exists?) next-method))



;=============================================================================
;
;	      Cloning functions (from Rob Deline <rdeline@CS.CMU.EDU>)
;
;=============================================================================
(define-method shallow-clone ((self <object>))
  (let ((clone (send LKOSInstance allocateInstance:forLisp: (class-of self) (this-lisp))) 
	(slots (map slot-definition-name
		    (class-slots (class-of self)))))
    (for-each (lambda (slot)
		(if (slot-bound? self slot)
		    (slot-set! clone slot (slot-ref self slot))))
	      slots)
    clone))

(define-method deep-clone ((self <object>))
  (let ((clone (send LKOSInstance allocateInstance:forLisp: (class-of self) (this-lisp)))
	(slots (map slot-definition-name
		    (class-slots (class-of self)))))
    (for-each (lambda (slot)
		(if (slot-bound? self slot)
		    (slot-set! clone slot
			       (let ((value (slot-ref self slot)))
				 (if (instance? value)
				     (deep-clone value)
				     value)))))
	      slots)
    clone))

;=============================================================================
;
; 		     	Class redefinition utilities
;;
;=============================================================================

;==== Remove-class-accessors
(define-method remove-class-accessors ((c <class>))
  (for-each 
     (lambda (m) (if (is-a? m <accessor-method>) (remove-method-in-classes! m)))
     (class-direct-methods c)))


;==== Update-direct-method
(define-method update-direct-method ((m  <method>) (old <class>) (new <class>))
  (let Loop ((l (method-specializers m)))
    (when (pair? l)       	; Note: the <top> in dotted list is never used. 
      (if (eq? (car l) old)     ; So we can work if we had only proper lists.
	(set-car! l new))
      (Loop (cdr l)))))

;==== Update-direct-subclass
(define-method update-direct-subclass ((c <class>) (old <class>) (new <class>))
  (let ((new-supers (map (lambda (cls) (if (eq? cls old) new cls))
			 (class-direct-supers c))))
    ;; Create a new class with same name as c. This will automagically call 
    ;; class-redefinition on this subclass and redefine all its descent
    (ensure-class (class-name c)
		  new-supers
		  (class-direct-slots c)
		  (class-of c))))

;==== Class-redefinition
(define-method class-redefinition ((old <class>) (new <class>))
  ;; Work on direct methods:
  ;;		1. Remove accessor methods from the old class 
  ;;		2. Patch the occurences of old in the specializers by new
  ;;		3. Displace the methods from old to new
  (remove-class-accessors old)					;; -1-
  (let ((methods (class-direct-methods old)))
    (for-each (lambda (m) (update-direct-method m old new))	;; -2-
	      methods)
    (slot-set! new #.lkos-slot-for-direct-methods methods))			;; -3-

  ;; Remove the old class from the direct-subclasses list of its super classes
  (for-each (lambda (c) (slot-set! c #.lkos-slot-for-direct-subclasses 
				   (delete! old (class-direct-subclasses c) eq?)))
	    (class-direct-supers old))

  ;; Redefine all the subclasses of old to take into account modification
  (for-each 
       (lambda (c) (update-direct-subclass c old new))
       (class-direct-subclasses old))

  ;; Invalidate class so that subsequent instances slot accesses invoke
  ;; change-object-class
  (slot-set! old #.lkos-slot-for-redefined new))


;=============================================================================
;
; 			Utilities for INITIALIZE methods
;
;=============================================================================


;;;
;;; Compute-get-n-set
;;;
(define-method compute-get-n-set ((class <class>) s)
  (case (slot-definition-allocation s)
    ((:instance) ;; Instance slot
     		 ;; get-n-set is just its offset
     		 (let ((already-allocated (+ (slot-ref class #.lkos-slot-for-nfields) 1)))  ;; added + 1 (Stef)
		   (slot-set! class #.lkos-slot-for-nfields already-allocated)        ;; ...and removed the + 1
		   already-allocated))

    ((:class)  ;; Class slot
               ;; Class-slots accessors are implemented as 2 closures around 
     	       ;; a Scheme variable. 
               (let ((name (slot-definition-name s)))
		 (if (memq name (map slot-definition-name 
				     (class-direct-slots class)))
		     ;; This slot is direct; create a new shared cell
		     (let ((shared-cell (void)))
		       (list (lambda (o)   shared-cell)
			     (lambda (o v) (set! shared-cell v))))
		     ;; Slot is inherited. Find its definition in superclass
		     (let Loop ((l (cdr (class-precedence-list class))))
		       (let ((r (assq name (slot-ref (car l) #.lkos-slot-for-getters-n-setters))))
			 (if r
			     (cddr r)
			     (Loop (cdr l))))))))

    ((:each-subclass) ;; slot shared by instances of direct subclass.
     		      ;; (Thomas Buerger, April 1998)
     		      (let ((shared-cell (void)))
			(list (lambda (o)   shared-cell)
			      (lambda (o v) (set! shared-cell v)))))

    ((:virtual) ;; No allocation
     		;; slot-ref and slot-set! function must be given by the user
     	     	(let ((get (key-get (slot-definition-options s) :slot-ref  #f))
		      (set (key-get (slot-definition-options s) :slot-set! #f)))
		  (unless (and get set)
		    (error "a :slot-ref and a :slot-set! must be supplied in ~S" s))
		  (list (make-closure get)
			(make-closure set))))
    ((:active) ;; Active slot
     	       ;; active slots admit a daemon before or after slot access
               (let* ((index 	   (+ (slot-ref class #.lkos-slot-for-nfields) 1))                 ;; added + 1 (Stef)
		      (name	   (car s))
		      (s	   (cdr s))
		      (before-ref  (make-closure (key-get s :before-slot-ref  #f)))
		      (after-ref   (make-closure (key-get s :after-slot-ref   #f)))
		      (before-set! (make-closure (key-get s :before-slot-set! #f)))
		      (after-set!  (make-closure (key-get s :after-slot-set!  #f))))
		 (slot-set! class #.lkos-slot-for-nfields index)                                   ;; removed + 1 (Stef)
		 (list (lambda (o)
			 (if before-ref
			     (if (before-ref o)
				 (let ((res (send o getSlot: index)))  ;; inlined %fast-slot-ref (Stef)
				   (and after-ref
					(not (eqv? res (void)))
					(after-ref o))
				   res)
				 (void))
			     (let ((res (send o getSlot: index)))      ;; inlined %fast-slot-ref (Stef)
			       (and after-ref (not (eqv? res (void))) (after-ref o))
			       res)))
		       (lambda (o v) 
			 (if before-set!
			     (when (before-set! o v)
			       (send o setSlot:to: index v)             ;; inlined %fast-slot-set!(Stef)
			       (and after-set! (after-set! o v)))
			     (begin
			       (send o setSlot:to: index v)              ;; inlined %fast-slot-set! (Stef)
			       (and after-set! (after-set! o v))))))))
    (else    (next-method))))

(define-method compute-get-n-set ((o <object>) s)
  (error "allocation type \"~S\" is unknown" (slot-definition-allocation s)))


(define (compute-slot-accessors class slots)
  ;; accessors are made her in-line using a light version of ensure-method
  ;; in particular, the class of accessors is <accessor-method> and there
  ;; is no next method defined for them, since they are terminal.

  (define (make-reader name getter)
    (let ((gf (ensure-generic-function getter)))
      (add-method! gf
		   (make <accessor-method>
		         :generic-function gf
			 :specializers (list class)
			 :procedure    (lambda (o) (slot-ref o name))))))
  (define (make-writer name setter)
    (let ((gf (ensure-generic-function setter)))
      (add-method! gf
		   (make <accessor-method>
		         :generic-function gf
			 :specializers (list class <top>)
			 :procedure    (lambda (o v) (slot-set! o name v))))))
  ;;
  ;;
  (for-each
      (lambda (s)
	(let ((name     (slot-definition-name     s))
	      (getter   (slot-definition-getter   s))
	      (setter   (slot-definition-setter   s))
	      (accessor (slot-definition-accessor s)))
	  (when getter
	    (make-reader name getter))
	  (when setter
	    (make-writer name setter))
	  (when accessor
	    (make-reader name accessor)
	    (make-writer name accessor))))
      slots))


;;;
;;; compute-getters-n-setters
;;; 

(define (compute-getters-n-setters class slots)

  (define (compute-slot-init-function s)
    (let ((init (slot-definition-init-form s)))
      (cond
	((eq? init (void))
	   #f)
	((eq? (slot-definition-allocation s) :class)
	   (let ((sn (slot-definition-name s)))
	     (make-closure `(lambda (o)
			      (let ((val (%slot-ref o ',sn)))
				(if (eq? val (void))
				    ,init
				    val))))))
	(else
	   (make-closure `(lambda (o) ,init))))))


  (define (verify-accessors slot l)
    (if (pair? l)
	(let ((get (car l)) 
	      (set (cadr l)))
	  (unless (and (closure? get) (= (%procedure-arity get) 1))
	    (error "bad getter closure for slot `~S' in ~S: ~S" slot class get))
	  (unless (and (closure? set) (= (%procedure-arity set) 2))
	    (error "bad setter closure for slot `~S' in ~S: ~S" slot class set)))))

  (map (lambda (s)
	 (let* ((s     (if (pair? s) s (list s)))
		(g-n-s (compute-get-n-set class s))
		(name  (slot-definition-name s)))
	   ;; For each slot we have '(name init-function getter setter)
	   ;; If slot is an instance one, we have the simplest
	   ;; form '(name init-function . index)
	   (verify-accessors name g-n-s)
	   (list* name (compute-slot-init-function s) g-n-s)))
       slots))

;;;
;;; compute-cpl
;;;
;;; The current implementation was provided by Anthony Beurivé
;;; <beurive@labri.u-bordeaux.fr>. This implementation is monotonic and 
;;; works "by level". For more information on monotonic superclass 
;;; linearization  algorithm look at the paper 
;;; 	A Monotonic Superclass Linearization for Dylan, 
;;;     K. Barrett, B. Cassels, P. Haahr, D. A. Moon, K. Playford, 
;;;     P. Tucker Withington, 
;;;     OOPSLA 96.
;;; also available at 
;;;	http://www.webcom.com/~haahr/dylan/linearization-oopsla96.html
;;;
(define (compute-cpl class)

  (define (reduce sequences cpl)
    (define (aux sequence)
      (if (and (pair? sequence) (memq (car sequence) cpl))
	  (aux (cdr sequence))
	  sequence))
    (map aux sequences))

  (define (delete-null-lists sequences)
    (if (pair? sequences)
	(let ((first (car sequences)))
	  (if (pair? first)
	      (cons first (delete-null-lists (cdr sequences)))
	      (delete-null-lists (cdr sequences))))
	'()))

  (define (select-candidate candidate sequences)
    (if (pair? sequences)
	(and (not (memq candidate (cdr (car sequences))))
	     (select-candidate candidate (cdr sequences)))
	candidate))

  (define (filter-candidates candidates sequences)
    (if (pair? candidates)
	(or (select-candidate (car candidates) sequences)
	    (filter-candidates (cdr candidates) sequences))
	#f)) ; No valid candidate (inconsistency).

  (define (next-class sequences)
    (let ((candidates (map car sequences)))
      (or (filter-candidates candidates sequences)
	  (car candidates)))) ; Arbitrarily brake the inconsistency.

  (define (step cpl sequences)
    (if (pair? sequences)
	(let ((new-cpl (cons (next-class sequences) cpl)))
	  (step new-cpl (delete-null-lists (reduce sequences new-cpl))))
	(reverse cpl)))

  (step '() (let ((supers (slot-ref class #.lkos-slot-for-direct-supers)))
	      (cons (cons class supers)
		    (map (lambda (super)
			   (slot-ref super #.lkos-slot-for-cpl))
			 supers)))))


;=============================================================================
;
; 			    I n i t i a l i z e
;
;=============================================================================

(define-method initialize ((object <object>) initargs)
               (send object initializeObject: initargs))
;  (%initialize-object object initargs))

(define-method initialize ((class <class>) initargs)
  (next-method)
  (let ((dslots (key-get initargs :slots   '()))
	(supers (key-get initargs :dsupers '())))

    (slot-set! class #.lkos-slot-for-name	  	(key-get initargs :name '???))
    (slot-set! class #.lkos-slot-for-direct-supers 	supers)
    (slot-set! class #.lkos-slot-for-direct-slots  	dslots)
    (slot-set! class #.lkos-slot-for-direct-subclasses '())
    (slot-set! class #.lkos-slot-for-direct-methods    '())
    (slot-set! class #.lkos-slot-for-cpl		(compute-cpl class))
    (slot-set! class #.lkos-slot-for-redefined		#f)
    (let ((slots (%compute-slots class)))
      (slot-set! class #.lkos-slot-for-slots	  	  slots)
      (slot-set! class #.lkos-slot-for-nfields	  	  0)
      (slot-set! class #.lkos-slot-for-getters-n-setters (compute-getters-n-setters class slots)))
    ;; Update the "direct-subclasses" of each inherited classes
    (for-each (lambda (x)
		(slot-set! x #.lkos-slot-for-direct-subclasses
			   (cons class (slot-ref x #.lkos-slot-for-direct-subclasses))))
	      supers)

    ;; Build getters - setters - accessors
    (compute-slot-accessors class dslots)))


(define-method initialize ((generic <generic>) initargs)
  (let ((previous-definition (key-get initargs :default #f)))
    (next-method)
    (slot-set! generic #.lkos-slot-for-name    (key-get initargs :name '???))
    (slot-set! generic #.lkos-slot-for-methods
	       (if (procedure? previous-definition)
		   (list (make <method>	; /// FAST-METHOD
			   :specializers <top>
			   :generic-function generic
			   :procedure (let ((next-method #f))
					(lambda l
					  (apply previous-definition l)))))
		   '()))))

(define-method initialize ((method <method>) initargs)
  (next-method)
  (slot-set! method #.lkos-slot-for-generic-function (key-get initargs :generic-function #f))
  (slot-set! method #.lkos-slot-for-specializers     (key-get initargs :specializers '()))
  (slot-set! method #.lkos-slot-for-procedure        (key-get initargs :procedure (lambda l '()))))


;=============================================================================
;
; 				     M a k e 
;
;     A new definition which overwrite the previous one which was built-in
;
;=============================================================================

;; (define-method allocate-instance ((class <class>) initargs)
;;   (%allocate-instance class))

;; (define-method make-instance ((class <class>) . initargs)   ;// VIRER CE SYMBOLE
;;   (let ((instance (allocate-instance class initargs)))
;;     (initialize instance initargs)
;;     instance))

;; faster version (Stef):
(define (make-instance class . initargs) 
  (let ((instance (send LKOSInstance allocateInstance:forLisp: class (this-lisp))))
    (initialize instance initargs)
    instance))

(define make make-instance)

;=============================================================================
;
; 			C h a n g e - c l a s s
;
;=============================================================================

;; (define (change-object-class old-instance old-class new-class)
;;   (let ((new-instance (allocate-instance new-class ())))
;;     ;; Initalize the slot of the new instance
;;     (for-each (lambda (slot)
;; 		(if (slot-exists-using-class? old-class old-instance slot)
;; 		    ;; Slot was present in old instance; set it 
;; 		    (if (slot-bound-using-class? old-class old-instance slot)
;; 			(slot-set-using-class!
;; 			     new-class 
;; 			     new-instance 
;; 			     slot 
;; 			     (slot-ref-using-class old-class old-instance slot)))
;; 		    ;; slot was absent; initialize it with its default value
;; 		    (let ((init (slot-init-function new-class slot)))
;; 		      (if init
;; 			  (slot-set-using-class!
;; 			       new-class 
;; 			       new-instance 
;; 			       slot
;; 			       (apply init '()))))))
;; 	      (map slot-definition-name (class-slots new-class)))
;;     ;; Exchange old an new instance in place to keep pointers valids
;;     (%modify-instance old-instance new-instance)
;;     old-instance))


;; (define-method change-class ((old-instance <object>) (new-class <class>))
;;   (change-object-class old-instance (class-of old-instance) new-class))

(define-method change-class ((old-instance <object>) (new-class <class>))
  (send old-instance changeClassFrom:to: (class-of old-instance) new-class))


;=============================================================================
;
;				a p p l y - g e n e r i c
;
; Protocol for calling standard generic functions.
; This protocol is  not used for real <generic> functions (in this case we use
; a completely C hard-coded protocol). 
; Apply-generic is used by STklos for calls to subclasses of <generic>.
;
; The code below is similar to the first MOP described in AMOP. In particular,
; it doesn't used the currified approach to gf call. There are 3 reasons for 
; that: 
;   - the protocol below is exposed to mimic completely the one written in C
;   - the currified protocol would be imho inefficient in C.
;   - nobody (except persons really interested with playing with a MOP) will 
;     probably use the following code
;=============================================================================
 
(define-method compute-applicable-methods ((gf <generic>) args)
  (apply find-method gf args))

(define-method method-more-specific? ((m1 <method>) (m2 <method>) targs)
  (%method-more-specific? m1 m2 targs))

(define-method sort-applicable-methods ((gf <generic>) methods args)
  (let ((targs (map class-of args)))
    (sort methods (lambda (m1 m2) (method-more-specific? m1 m2 targs)))))
 
(define-method apply-method ((gf <generic>) methods build-next args)
  (let ((proc (method-procedure (car methods))))
    (%set-next-method! proc (build-next methods args))
    (apply proc args)))

(define-method apply-methods ((gf <generic>) (l <pair>) args)  ;; was <list> instead of <pair> (Stef)
  (letrec ((next (lambda (procs args)
		   (lambda new-args
		     (let ((a (if (null? new-args) args new-args)))
		       (if (null? (cdr procs))
			   (no-next-method gf (car procs) a)
			   (apply-method gf (cdr procs) next a)))))))
    (apply-method gf l next args)))


;; ==== apply-generic

(define-method apply-generic ((gf <generic>) args)
  (if (null? (generic-function-methods gf))
      (no-method gf args))
  (let ((methods (compute-applicable-methods gf args)))
    (if methods
	(apply-methods gf (sort-applicable-methods gf methods args) args)
 	(no-applicable-method gf args))))


;// ;=============================================================================
;// ;
;// ;		     <Composite-metaclass> 
;// ;=============================================================================


;// 
;// (export <Composite-metaclass> <Active-metaclass>)
;// 

;=============================================================================
;
; 				T o o l s
;
;=============================================================================
(define class-subclasses #f)
(define class-methods    #f)

(let ()
  (define (list2set l)
    (let Loop ((l l) (res '()))
      (cond
       ((null? l) 	   res)
       ((memq (car l) res) (Loop (cdr l) res))
       (else		   (Loop (cdr l) (cons (car l) res))))))

  (define (mapappend func . args)
    (if (memq '()  args)
	'()
	(append (apply func (map car args))
		(apply mapappend func (map cdr args)))))
 
  ;; ==== class-subclasses  
  (set! class-subclasses 
	(lambda (c)
	  (letrec ((allsubs (lambda (c)
			      (cons c (mapappend allsubs 
						 (class-direct-subclasses c))))))
	    (list2set (cdr (allsubs c))))))
  ;; ==== class-methods
  (set! class-methods 
	(lambda (c)
	  (list2set (mapappend class-direct-methods 
			       (cons c (class-subclasses c)))))))

;==== CLOS like slot-value (but as a gf instead of proc as slot-ref or slot-set!) 
(define-method slot-value ((o <object>) s) 		;; The getter
  (slot-ref o s))
 
(define-method slot-value ((o <object>) s v)		;; The setter
  (slot-set! o s v))



