;;; $Id: variante1.scm,v 1.2 1993/03/16 17:09:24 queinnec Exp $
;;; Christian Queinnec             Ecole Polytechnique & INRIA-Rocquencourt
;;;                                <Christian.Queinnec@inria.fr>
;;; This file is part of the Meroonet package.

;;; VARIANT 1:  a simple-minded define-class

;;; a define-class that works only for interpreters, the class is
;;; created at macroexpansion time and grafted to the inheritance tree
;;; at that same time. The version in Meroonet is safer.

(define-meroonet-macro (define-class name super-name own-field-descriptions)
  (let ((class (register-class name super-name own-field-descriptions)))
    (Class-generate-related-names class) ) )

;;; end of variante1.scm
