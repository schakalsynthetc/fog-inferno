;;; $Id: variante2.scm,v 1.2 1993/03/16 17:09:46 queinnec Exp $
;;; Christian Queinnec             Ecole Polytechnique & INRIA-Rocquencourt
;;;                                <Christian.Queinnec@inria.fr>
;;; This file is part of the Meroonet package.

;;; VARIANT 2:             A detailed example of metaclass.

;;; First make generic some functions so they can be specialized on
;;; new metaclasses.  Then make Meroonet use these new functions
;;; instead.

(define-generic (generate-related-names (class)))

(define-method (generate-related-names (class Class))
   (Class-generate-related-names class) )

;;; The other initialization parameters must be given in correct order.

(define-generic (initialize! (o) . args))

(define-method (initialize! (o Class) . args)
  (apply Class-initialize! o args) )

;;; This is the new metaclass, it will count the number of generated
;;; instances. Define a new macro to define classes with such a metaclass
;;; or alternatively add a :metaclass option to define-class.

(define-class CountingClass Class (counter))

(define-meroonet-macro (define-CountingClass name super-name own-field-descriptions)
  (let ((class (register-CountingClass
                name super-name own-field-descriptions )))
    (generate-related-names class) ) )

(define (register-CountingClass name super-name own-field-descriptions)
  (initialize! (allocate-CountingClass) 
               name
               (->Class super-name) 
               own-field-descriptions ) )

;;; Initialize the counter to zero before everything.

(define-method (initialize! (class CountingClass) . args)
  (set-CountingClass-counter! class 0)
  (call-next-method) ) 

;;; This is the new generation of accompanying functions. The better
;;; would be to have individual generic functions to generate makers,
;;; allocators, readers etc and to specialize them (as done in Meroon
;;; V3). Here since we only have the generic function
;;; generate-related-names, we patch the produced code to insert the
;;; management of the counter. Inefficient and ugly but sufficient to
;;; expose the point which is that metaclasses are used at
;;; macroexpansion time to generate alternative code.

(define-method (generate-related-names (class CountingClass))
  (let ((cname (symbol-append (Class-name class) '-class))
        (alloc-name (symbol-append 'allocate- (Class-name class)))
        (make-name (symbol-append 'make- (Class-name class))) )
    `(begin ,(call-next-method)
            (set! ,alloc-name           ; patch the allocator
                  (let ((old ,alloc-name))
                    (lambda sizes 
                      (set-CountingClass-counter! 
                       ,cname (+ 1 (CountingClass-counter ,cname)) )
                      (apply old sizes) ) ) )
            (set! ,make-name            ; patch the maker            
                  (let ((old ,make-name))
                    (lambda args
                      (set-CountingClass-counter! 
                       ,cname (+ 1 (CountingClass-counter ,cname)) )
                      (apply old args) ) ) ) ) ) )

;;; And now test this new metaclass, check that allocators and makers
;;; regularly increment the counter in the class object.

(define-CountingClass CountedPoint Object (x y))

(unless (and (= 0 (CountingClass-counter CountedPoint-class))
             (allocate-CountedPoint)
             (= 1 (CountingClass-counter CountedPoint-class))
             (make-CountedPoint 11 22)
             (= 2 (CountingClass-counter CountedPoint-class)) )
  ;; should not be evaluated if everything is OK
  (meroonet-error "Failed test on CountedPoint") )

;;; end of variante2.scm
