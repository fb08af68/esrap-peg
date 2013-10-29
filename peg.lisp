; This file is part of Esrap-PEG
; Esrap-PEG generates Esrap rules from PEG definitions
; Esrap-PEG is written by Michael Raskin
; Esrap-PEG is provided by the same license as Esrap, namely, MIT license
;
(in-package :esrap-peg)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *character-class-storage* nil)
  (defvar *range-list* (make-hash-table))

  (defmacro character-class-rule (&rest clauses)
    `(let*
       (
	(sem-symbol 
	  (intern 
	    ,(format 
	       nil 
	       "semantic-checker for character class with clauses ~s " 
	       clauses)))
	(rule-symbol 
	  (intern 
	    ,(format 
	       nil 
	       "character class rule with clauses ~s " 
	       clauses)))
	(already-defined-semsymbol
	  (gethash sem-symbol *range-list*))
	(code
	  `(progn
	     ,@(unless
		 already-defined-semsymbol
		 `(
		   (defun
		     ,sem-symbol (x)
		     (or
		       ,@(iterate
			   (for c in ',clauses)
			   (when (characterp c)
			     (collect `(equal x ,c)))
			   (when (stringp c)
			     (collect `(equal x ,(elt c 0))))
			   (when (listp c)
			     (collect `(<=
					 ,(char-code 
					    (let ((cc (first c)))
					      (if (characterp cc)
						cc (elt cc 0))))
					 (char-code x)
					 ,(char-code 
					    (let ((cc (second c)))
					      (if (characterp cc)
						cc (elt cc 0))))
					 )))
			   (when (eq c t) (collect t))
			   )
		       ))))
	     (add-rule 
	       ',rule-symbol 
	       (make-instance 
		 'rule
		 :expression '(,sem-symbol character)
		 :transform #'esrap::text/bounds
		 :condition t
		 :guard-expression t
		 ))
	     )
	  )
	)
       (push code *character-class-storage*)
       (setf (gethash sem-symbol *range-list*) t)
       rule-symbol
       ))
  )

(add-rule
  'any-character
  (make-instance
    'rule
    :expression 'character
    :transform #'esrap::text/bounds
    :condition t
    :guard-expression t
    )
  )

(defmacro define-character-classes ()
  `(progn
     ,@ *character-class-storage*))

(defmacro def-ast-rule (name &rest params)
  (let*
    (
     (transform-sym 
       (intern 
	 (format 
	   nil 
	   "temporary function to add symbol ~s into AST" 
	   name)))
     )
  `(progn
     (defun ,transform-sym (x &rest pos) (declare (ignorable pos)) (list ',name x))
     (add-rule 
       ',name
       (make-instance
	 'rule
	 :expression ',(first params)
	 :transform #',transform-sym
	 :condition t
	 :guard-expression t
	 )
       ))))

(def-ast-rule grammar (and spacing (+ definition) endoffile))
(def-ast-rule definition (and identifier leftarrow expression))

(def-ast-rule expression (and sequence (* (and slash sequence))))
(def-ast-rule sequence (* prefix))
(def-ast-rule prefix (and (? (or and not)) suffix))
(def-ast-rule suffix (and primary (? (or question star plus))))
(def-ast-rule primary (or (and identifier (! leftarrow))
			  (and open expression close)
			  literal
			  class 
			  dot))

(def-ast-rule identifier (and identstart (* identcont) spacing))
(def-ast-rule identstart #.(character-class-rule (#\a #\z) (#\A #\Z) #\_))
(def-ast-rule identcont (or identstart #.(character-class-rule (#\0 #\9))))

(def-ast-rule literal (or 
			(and "'" (* (and (! "'") char)) "'" spacing)
			(and "\"" (* (and (! "\"") char)) "\"" spacing)
			))
(def-ast-rule class (and "[" (* (and (! "]") range)) "]" spacing))
(def-ast-rule range (or (and char "-" char) char))
(def-ast-rule char (or
		     (and "\\" #.(character-class-rule 
				   #\n #\r #\t #\' #\" #\[ #\] #\\))
		     (and "\\" #\u
			  #.(character-class-rule (#\0 #\9) (#\a #\f) (#\A #\F))
			  #.(character-class-rule (#\0 #\9) (#\a #\f) (#\A #\F))
			  #.(character-class-rule (#\0 #\9) (#\a #\f) (#\A #\F))
			  #.(character-class-rule (#\0 #\9) (#\a #\f) (#\A #\F))
			  )
		     (and "\\" #\U
			  #.(character-class-rule (#\0 #\9) (#\a #\f) (#\A #\F))
			  #.(character-class-rule (#\0 #\9) (#\a #\f) (#\A #\F))
			  #.(character-class-rule (#\0 #\9) (#\a #\f) (#\A #\F))
			  #.(character-class-rule (#\0 #\9) (#\a #\f) (#\A #\F))
			  #.(character-class-rule (#\0 #\9) (#\a #\f) (#\A #\F))
			  #.(character-class-rule (#\0 #\9) (#\a #\f) (#\A #\F))
			  #.(character-class-rule (#\0 #\9) (#\a #\f) (#\A #\F))
			  #.(character-class-rule (#\0 #\9) (#\a #\f) (#\A #\F))
			  )
		     (and "\\" #.(character-class-rule (#\0 #\3))
			  #.(character-class-rule (#\0 #\7)) 
			  #.(character-class-rule (#\0 #\7)))
		     (and "\\" #.(character-class-rule (#\0 #\7))
			  (? #.(character-class-rule (#\0 #\7))))
		     (and (! "\\") any-character)
		     ))

(def-ast-rule leftarrow (and "<-" spacing))
(def-ast-rule slash (and "/" spacing))
(def-ast-rule and (and "&" spacing))
(def-ast-rule not (and "!" spacing))
(def-ast-rule question (and "?" spacing))
(def-ast-rule star (and "*" spacing))
(def-ast-rule plus (and "+" spacing))
(def-ast-rule open (and "(" spacing))
(def-ast-rule close (and ")" spacing))

(def-ast-rule dot (and "." spacing))

(def-ast-rule spacing (* (or space comment)))
(def-ast-rule comment (and "#" (* (and (! endofline) character)) (or endofline endoffile)))
(def-ast-rule space (or #\Space #\Tab endofline))
(def-ast-rule endofline (or #.(format nil "~a~a" #\Return #\NewLine ) #\Newline #\Return))
(def-ast-rule endoffile (! character))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-character-classes)
  )
