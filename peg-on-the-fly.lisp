; This file is part of Esrap-PEG
; Esrap-PEG generates Esrap rules from PEG definitions
; Esrap-PEG is written by Michael Raskin
; Esrap-PEG is provided by the same license as Esrap, namely, MIT license
;
(in-package :esrap-peg)

(defun ast-eval (x) (funcall (get (first x) 'peg-handler) (second x)))

(defvar character-range-definer)
(setf
  character-range-definer
  (lambda (sym ranges)
    (let*
      (
       (point-ranges (remove-if 'listp ranges))
       (full-ranges (remove-if 'integerp ranges))
       )
      (setf (symbol-function sym)
	    (lambda (x)
	      (let*
		(
		 (c (char-code x))
		 )
		(or
		  (find c point-ranges )
		  (find-if (lambda (r) (<= (first r) c (second r)))
			  full-ranges)
		  )))
	    )
      )))

(defvar rule-creator)
(setf 
  rule-creator 
  (lambda 
    (sym expression transform)
    (let*
      ((transform (eval transform)))
      (add-rule 
	sym
	(make-instance 
	  'rule
	  :expression expression
	  :transform 
	  (cond
	    ((eq transform :list) (lambda (l &rest pos) (declare (ignorable pos)) (list sym l)))
	    ((symbolp transform) (symbol-function transform))
	    ((functionp transform) transform)
	    (t nil)
	    )
	  :condition t
	  :guard-expression t
	  ))
      )))

(defmacro def-peg-fun (name (arg) &rest code)
  `(setf (get ',name 'peg-handler) (lambda (,arg) ,@code)))

(def-peg-fun grammar (x)
  (loop for y in (second x) do (ast-eval y)))

(def-peg-fun 
  definition (x)
  (let*
    (
     (sym (ast-eval (first x)))
     (expr (ast-eval (third x)))
     )
    (funcall 
      rule-creator
      sym expr 
      :list)
    ))

(def-peg-fun expression (x)
  (let*
    (
     (h (first x))
     (ts (second x))
     )
    (if ts
      (apply 
	'list 
	'or
	(ast-eval h)
	(mapcar 'ast-eval 
		(mapcar 'second ts))
	)
      (ast-eval h)
      )
    ))

(def-peg-fun sequence (x)
  (let*
    (
     (body (mapcar 'ast-eval x))
     )
    (if (> (length body) 1)
      (cons 'and body)
      (first body)
      )
    )
  )

(def-peg-fun prefix (x)
  (let*
    (
     (modifier (caar x))
     (modifier-sym
       (case modifier
	 ((not) '!)
	 ((and) '&)
	 ((nil) nil)
	 (t nil)))
     (body (second x))
     )
    (if modifier-sym
      (list modifier-sym (ast-eval body))
      (ast-eval body))))

(def-peg-fun suffix (x)
  (let*
    (
     (modifier (caadr x))
     (body (ast-eval (first x)))
     (modifier-sym
       (case modifier
	 ((question) '?)
	 ((star) '*)
	 ((plus) '+)
	 ((nil) nil)
	 (t nil)))
     )
    (if modifier-sym
      (list modifier-sym body)
      body)))

(def-peg-fun primary (x)
  (cond
    ((symbolp (car x)) (ast-eval x))
    ((equal (caar x) 'identifier) (ast-eval (first x)))
    ((equal (caar x) 'open) (ast-eval (second x)))
    (t (ast-eval (first x)))))

(def-peg-fun identifier (x)
  (intern 
    (string-upcase 
      (apply 'concatenate 'string
	   (cons (ast-eval (first x))
		 (mapcar 'ast-eval (second x)))))))

(def-peg-fun identstart (x) x)
(def-peg-fun identcont (x)
  (if (stringp x) x (ast-eval x)))

(def-peg-fun literal (x)
  (apply 'concatenate 'string
	 (mapcar 'ast-eval
		 (mapcar 'second (second x)))))

(def-peg-fun class (x)
  (let*
    (
     (ranges (mapcar 'ast-eval (mapcar 'second (second x))))
     (sem-symbol 
       (intern 
	 (format 
	   nil 
	   "peg-derived semantic-checker for character class with clauses ~s " 
	   ranges)))
     (rule-symbol 
       (intern 
	 (format 
	   nil 
	   "peg-derived character class rule with clauses ~s " 
	   ranges)))
     )
    (funcall character-range-definer sem-symbol ranges)
    (funcall rule-creator rule-symbol 
	     (list sem-symbol 'character)
	     '#'esrap::text/bounds
	     )
    rule-symbol
    ))

(def-peg-fun range (x)
  (if
    (= (length x) 2)
    (char-code (elt (ast-eval x) 0))
    (list
      (char-code (elt (ast-eval (first x)) 0))
      (char-code (elt (ast-eval (third x)) 0))
      )
    ))

(def-peg-fun char (x)
  (if (first x)
    (let*
      (
       (code (second x))
       )
      (cond
	((equal code "n") (string #\Newline))
	((equal code "r") (string #\Return))
	((equal code "t") (string #\Tab))
	((find code '("'" "\"" "[" "]" "\\") :test 'equal) code)
	((equal code "u") 
	 (let*
	   (
	    (num-str (apply 'concatenate 'string (cddr x)))
	    (num (parse-integer num-str :radix 16))
	    )
	   (string (code-char num))
	   ))
	((equal code "U") 
	 (let*
	   (
	    (num-str (apply 'concatenate 'string (cddr x)))
	    (num (parse-integer num-str :radix 16))
	    )
	   (string (code-char num))
	   ))
	(t 
	  (let*
	    (
	     (num-str (apply 'concatenate 'string (cdr x)))
	     (num (parse-integer num-str :radix 8))
	     )
	    (string (code-char num))
	    )
	  )
	)
      )
    (second x)
    )
  )

(def-peg-fun 
  dot (x)
  (declare (ignorable x))
  'any-character)
