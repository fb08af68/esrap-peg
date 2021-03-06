; This file is part of Esrap-PEG
; Esrap-PEG generates Esrap rules from PEG definitions
; Esrap-PEG is written by Michael Raskin
; Esrap-PEG is provided by the same license as Esrap, namely, MIT license
;
(in-package :esrap-peg)

(defun peg-code (g)
  (let*
    (
     (code-storage nil)
     (character-range-definer
       (lambda (sym ranges)
	 (let*
	   (
	    (point-ranges (remove-if 'listp ranges))
	    (full-ranges (remove-if 'integerp ranges))
	    )
	   (push
	     `(setf 
		(symbol-function ',sym)
		(lambda (x)
		  (let*
		    (
		     (c (char-code x))
		     )
		    (or
		      (find c ',point-ranges )
		      (find-if 
			(lambda (r) 
			  (<= (first r) c (second r)))
			',full-ranges)
		      )))) 
	     code-storage
	     )
	   ))
       )
     (rule-creator
       (lambda (sym expression transform)
	 (push `(add-rule 
		  ',sym
		  (make-instance 
                    'rule
                    :expression ',expression
                    :transform 
                    ,(cond
                       ((eq transform :list) 
                        `(lambda (l &rest pos) (declare (ignorable pos)) (list ',sym l)))
                       ((symbolp transform) `'(symbol-function ,transform))
                       ((functionp (eval transform)) transform)
                       (t nil)
                       )
                    :condition t
                    :guard-expression t
                    :properties (esrap::make-rule-properties
                                  :uses-cache t
                                  :uses-cache-unless-trivial t)
                    ))
	       code-storage
	       )))
     )
    (declare (special character-range-definer) (special rule-creator))
    (ast-eval g)
    `(progn ,@(reverse code-storage))
    ))

(defmacro peg-compile (g)
  (peg-code (eval g)))
