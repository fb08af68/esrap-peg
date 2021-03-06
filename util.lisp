; This file is part of Esrap-PEG
; Esrap-PEG generates Esrap rules from PEG definitions
; Esrap-PEG is written by Michael Raskin
; Esrap-PEG is provided by the same license as Esrap, namely, MIT license
;
(in-package :esrap-peg)

(defun stream-contents (f)
  (let* ((out-data 
	   (make-array 
	     (file-length f) 
	     :element-type 'character))
	 (n (read-sequence out-data f)))
    (subseq out-data 0 n)
    ))
(defun file-contents (fn)
  (with-open-file 
    (f fn)
    (stream-contents f)))
