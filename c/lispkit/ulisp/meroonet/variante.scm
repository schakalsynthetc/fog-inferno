;;; $Id$

;;; Various variants for Meroonet

;;; a define-class that works only for interpreters, the class is
;;; created at macroexpansion time, grafted to the inheritance tree
;;; at macroexpansion time. 

(define-meroonet-macro (define-class name super-name own-field-descriptions)
  (let* ((super-class (->Class super-name))
         (class (add-subclass! name super-class own-field-descriptions)) )
    (Class-generate-related-names class) ) )

;;; An detailed example of metaclass.
;;; First make generic some functions and redefine some hooks in Meroonet.

(define-generic (generate-related-names (class)))

(define-method (generate-related-names (class Class))
   (Class-generate-related-names class) )

(define-generic (initialize! (o)))

(define-method (initialize! (o Class))
  (Class-initialize! o) )

(define-class Mateu-Class Class ((* super)))

(define-meroonet-macro (define-class name super-name own-field-descriptions)
  (let* ((super-class (->Class super-name))
         (class (Mateu-add-subclass! 
                 name super-class own-field-descriptions )) )
    (generate-related-names class) ) )

(define (Mateu-add-subclass! name super-class own-field-descriptions)
  (let ((class (allocate-Mateu-Class 
                (+ 1 (Mateu-Class-super-length super-class)) )))
    (initialize! class name super-class own-field-descriptions) ) )

;;; 
(define (meroonet-error . args)
  (display `(meroonet-error . ,args))
  (newline)
  (/ 3 0) )

(define is-a? 
  (let ((Class-is-a? is-a?))
    (lambda (o class)
      (let ((c (object->class class)))
        (cond ((eq? c Class-class)
               (Class-is-a? o class) )
              ((eq? c Mateu-Class-class)
               (Mateu-Class-is-a? o class) )
              (else (/ 3 0)) ) ) ) ) )

(define (Mateu-Class-is-a? o class)
  (let ((c (object->class o)))
    (let ((cn (Class-number class))
          (depth (Mateu-Class-super-length class)) )
      (and (<= depth (Mateu-Class-super-length c))
           (= cn (Mateu-Class-super c (- depth 1))) ) ) ) )

(define-method (initialize! (o Mateu-Class))
  (display o)
  (call-next-method)
  ;; fill the numbers of the super-classes
  (let fill ((i (- (Mateu-Class-super-length o) 1))
             (c o) )
    (when c
      (set-Mateu-Class-super! o i (Class-number c))
      (fill (- i 1) (Class-super-class c)) ) )
  o )

