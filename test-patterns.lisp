; This file is part of Esrap-PEG
; Esrap-PEG generates Esrap rules from PEG definitions
; Esrap-PEG is written by Michael Raskin
; Esrap-PEG is provided by the same license as Esrap, namely, MIT license
;
(in-package :esrap-peg)

(defparameter
  *tests*
  '(
    ("A()" ((call "A")))
    ("A(B)" ((call "A" :arguments ("B"))))
    ("A(B,C1)" ((call "A" :arguments ("B" "C1"))))
    ("A(B,C1) X()" ((call "A" :arguments ("B" "C1")) (call "X")))
    ))
(defparameter *the-grammar*
  "
  Text <- Call*
  Call <- FunctionName '(' Arguments ')' WhiteSpace*
  WhiteSpace <- [ \\t\\r\\n]
  FunctionName <- Identifier
  Identifier <- [A-Za-z] ([A-Za-z0-9_]*)
  Arguments <- (Identifier (',' Identifier)*)?
  ")
(def-peg-matchers
  (
   (Text (_ (m! #'! x)))
   (Call
     ((?name _ ?args _ _)
      `(call ,(! ?name)
	     ,@(when ?args (list :arguments 
				 (m! #'! ?args))))))
   (FunctionName (_ (! x)))
   (Identifier ((?start ?cont) (s+ (cons ?start (m! #'! ?cont)))))
   )
  :abbreviations :default :arg x)
(ast-eval (basic-parse-peg *the-grammar*))
#+nil(loop 
  for x in *tests*
  for input := (first x)
  for output := (second x)
  for parsed := (eval `(parse 'Text ,input))
  for processed := (ast-eval parsed)
  for ok := (equal processed output)
  do (format t "Test OK: ~s~%" ok)
  unless ok do (format t "Failure:~%~s"
		       (list input parsed processed output))
  )
