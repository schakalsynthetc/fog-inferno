;;; $Id: variante4.scm,v 1.1 1993/03/14 10:41:06 queinnec Exp $
;;; Christian Queinnec             Ecole Polytechnique & INRIA-Rocquencourt
;;;                                <Christian.Queinnec@inria.fr>
;;; This file is part of the Meroonet package.

;;; VARIANT 4:             enhanced(?) define-method

;;; This ne define-method a la CLOS creates a generic function on the
;;; fly if it does not exist yet. The sole problem is to do it a
;;; evaluation-time and not at macro-expansion time.

(define-meroonet-macro (define-method call . body)
  (parse-variable-specifications
   (cdr call)
   (lambda (discriminant variables)
     (let ((g (gensym))(c (gensym)))    ; make g and c hygienic
       `(begin
          (unless (->Generic ',(car call)) ; new
            (define-generic ,call) )       ; new
          (register-method
           ',(car call)
           (lambda (,g ,c)
             (lambda ,(flat-variables variables)
               (define (call-next-method)
                 ((if (Class-super-class ,c)
                      (vector-ref (Generic-dispatch-table ,g) 
                                  (Class-number (Class-super-class ,c)) )
                      (Generic-default ,g) )
                  . ,(flat-variables variables) ) )
               . ,body ) )
           ',(cadr discriminant)
           ',(cdr call) ) ) ) ) ) )

;;; I am not really fond of that but we can nevertheless test the
;;; previous definition.

(define-method (ugly a (b Class) . c)
  'barf )

(unless (eq? 'barf (ugly 2 Field-class))
  (meroonet-error "Failed test on define-method") )

;;; end of variante4.scm
