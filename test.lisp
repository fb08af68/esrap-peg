; This file is part of Esrap-PEG
; Esrap-PEG generates Esrap rules from PEG definitions
; Esrap-PEG is written by Michael Raskin
; Esrap-PEG is provided by the same license as Esrap, namely, MIT license
;
(in-package :esrap-peg)

(let*
  (
   (the-truth (read-from-string (file-contents "test.parsed")))
   )
  (format *error-output* "First stage:~%~s~%" (parse-peg-file "test.peg"))
  (ast-eval (parse-peg-file "test.peg"))
  (unless (equal (eval '(parse 'text (file-contents "test.txt"))) the-truth)
    (format *error-output* "~s~%" the-truth)
    (format *error-output* "~s~%" (eval '(parse 'text (file-contents "test.txt"))))
    (error "First-stage regression"))
  (format *error-output* "First stage passed~%")
  (eval (peg-code (parse-peg-file "peg.peg")))
  (format *error-output* "Second stage:~%~s~%" (parse-peg-file "test.peg"))
  (eval (peg-code (parse-peg-file "test.peg")))
  (unless (equal (eval '(parse 'text (file-contents "test.txt"))) the-truth)
    (error "Second-stage regression"))
  (format *error-output* "Second stage passed~%")
  (unless (equal (eval '(parse-peg-file "peg.peg") )
		 (progn 
		   (eval (peg-code (parse-peg-file "peg.peg")))
		   (eval '(parse-peg-file "peg.peg"))
		   ))
    (error "Bootstrap regression")
    )
  (format *error-output* "Bootstrap passed~%")
  )
