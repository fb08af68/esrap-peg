(in-package :esrap-peg)

#+ecl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (define-compiler-macro 
    parse (&whole form &rest args) 
    (declare (ignorable args))
    form)
  )

(defun basic-parse-peg (g)
  (parse 'grammar g))

(defun parse-peg-file (f)
  (parse 'grammar
	 (file-contents f)))
