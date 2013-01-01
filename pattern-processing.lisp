(in-package :esrap-peg)

(defvar *esrap-peg-pattern-package* nil)

(defun esrap-peg-symbol (x)
  (if *esrap-peg-pattern-package*
    (intern (symbol-name x) 
	    (find-package *esrap-peg-pattern-package*))
    x))

(defmacro def-peg-matchers
  (clauses &key abbreviations package
	   (arg 'arg))
  (let
    (
     (*esrap-peg-pattern-package* package)
     )
    `(progn
       ,@(iterate
	   (for x in clauses)
	   (for name := (esrap-peg-symbol (car x)))
	   (for handlers := (cdr x))
	   (collect
	     `(flet
		,abbreviations
		(declare
		  ,@(loop for x in abbreviations
			  collect `(ignorable (function ,(first x)))))
		(def-peg-fun 
		  ,name (,arg)
		  (block ,name
			 ,@(iterate
			     (for h in handlers)
			     (for pat := (car h))
			     (for vars-dup
				  := 
				  (sort 
				    (unify:collect-template-vars pat)
				    'string<))
			     (for vars := 
				  (iter 
				    (for x in vars-dup)
				    (for p previous x)
				    (when (not (equal x p))
				      (collect x))
				    ))
			     (collect
			       `(let
				  ((m (ignore-errors 
					(unify:unify
					  ',pat ,arg
					  ))))
				  (when m
				    (return-from 
				      ,name
				      (let
					(
					 ,@(iter 
					     (for v in vars)
					     (collect
					       `(,v (unify:find-variable-value ',v m)))
					     )
					 )
					,@(cdr h)
					)
				      ))
				  )
			       )
			     ))
		  )))
	   )
       )))
