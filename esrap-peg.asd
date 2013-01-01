(asdf:defsystem :esrap-peg
  :name "esrap-peg"
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
