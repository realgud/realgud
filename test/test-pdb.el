(require 'test-unit)
(load-file "../dbgr/debugger/pdb/pdb.el")

(test-unit-clear-contexts)

(context "pdb"
	 (tag pdb)

	 (specify "pdb-parse-cmd-args"
	      (assert-equal '(nil ("pdb") ("foo") nil)
	 		    (pdb-parse-cmd-args '("pdb" "foo")))
	      (assert-equal '(nil ("pdb") ("program.py" "foo") nil)
	 		    (pdb-parse-cmd-args 
	 		     '("pdb" "program.py" "foo")))
	      )
	 )

(test-unit "pdb")

