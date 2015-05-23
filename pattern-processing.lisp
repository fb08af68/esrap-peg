; This file is part of Esrap-PEG
; Esrap-PEG generates Esrap rules from PEG definitions
; Esrap-PEG is written by Michael Raskin
; Esrap-PEG is provided by the same license as Esrap, namely, MIT license
;
(in-package :esrap-peg)

(defvar *esrap-peg-pattern-package* nil)

(defun esrap-peg-symbol (x)
  (if *esrap-peg-pattern-package*
    (intern (symbol-name x)
	    (find-package *esrap-peg-pattern-package*))
    x))

(defun abbreviations-into-package (l &optional (package *package*))
  (loop for x in l
	collect
	(cons
	  (intern (symbol-name (car x)) package)
	  (cdr x))))

(defparameter *esrap-peg-matcher-default-abbreviations*
  `(
    (! (x) (ast-eval x))
    (n1 (x) (first x)) (n2 (x) (second x)) (n3 (x) (third x))
    (n4 (x) (fourth x)) (n5 (x) (fifth x)) (n6 (x) (sixth x))
    (n7 (x) (seventh x)) (n8 (x) (eighth x)) (n9 (x) (ninth x))
    (s+ (x) (apply 'concatenate 'string x))
    (l+ (x) (apply 'append x))
    (m! (f x) (mapcar f x))
    ))

(defmacro def-peg-matchers
  (clauses &key 
	   abbreviations package
	   (arg 'arg))
  (let
    (
     (*esrap-peg-pattern-package* 
       (or package *package*))
     (abbreviations
       (cond
	 ((eq abbreviations :default)
	  (abbreviations-into-package 
	    *esrap-peg-matcher-default-abbreviations*))
	 ((symbolp abbreviations) 
	  (abbreviations-into-package (symbol-value abbreviations)))
	 (t abbreviations)))
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

(defun add-peg-order-rule (node before after)
  (unless (get node 'peg-patter-tag-order)
    (setf (get node 'peg-patter-tag-order) (make-hash-table)))
  (setf (second (gethash before (get node 'peg-patter-tag-order)))
	(cons after (second (gethash before 
				     (get node 'peg-patter-tag-order)))))
  (setf (first (gethash after (get node 'peg-patter-tag-order)))
	(cons before (first (gethash after
				       (get node 'peg-patter-tag-order))))))

(defun delete-peg-order-rule (node before after)
  (unless (get node 'peg-patter-tag-order)
    (setf (get node 'peg-patter-tag-order) (make-hash-table)))
  (setf (second (gethash before (get node 'peg-patter-tag-order)))
	(remove after (second (gethash before 
				       (get node 'peg-patter-tag-order)))))
  (setf (first (gethash after (get node 'peg-patter-tag-order)))
	(remove before (first (gethash after
					 (get node 'peg-patter-tag-order))))))
