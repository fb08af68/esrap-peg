(asdf:defsystem :esrap-peg
  :name "esrap-peg"
  :description "A wrapper around Esrap to allow generating Esrap grammars from PEG definitions"
  :license "MIT"
  :author "Michael Raskin"
  :depends-on (
	       :esrap 
	       :iterate 
	       :cl-unification 
	       :alexandria
	       )
  :components (
	       (:file "package")
	       (:file "util"
		      :depends-on ("package"))
	       (:file "peg"
		      :depends-on ("package"))
	       (:file "peg-functions"
		      :depends-on ("package" "util" "peg"))
	       (:file "peg-on-the-fly"
		      :depends-on ("package"))
	       (:file "peg-compile"
		      :depends-on ("package" "peg-on-the-fly"))
	       (:file "pattern-processing"
		      :depends-on 
		      ("package"))
	       )
  )
