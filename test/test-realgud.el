;; Manually run these tests as follows:
;;
;; emacs --batch --no-site-file --no-splash \
;;   --script setup.el --chdir PACAKGESDIR/realgud \
;;   -l test/test-realgud.el -f ert-run-tests-batch-and-exit
;;
;; where setup.el looks something like:
;; (let ((elpa-dir (expand-file-name "~/.emacs.d/elpa")))
;;   (add-to-list 'load-path (concat elpa-dir "/test-simple-20170117.411"))
;;   (add-to-list 'load-path (concat elpa-dir "/load-relative-20160716.438"))
;;   (add-to-list 'load-path (concat elpa-dir "/loc-changes-20160801.1008")))

(defun realgud-test-helper()
  (delq nil
		(mapcar (lambda (x) (and (string-match-p "^\\(realgud:\\|realgud-\\)" (symbol-name x)) x))
				features)))

(ert-deftest test-feature-unload()

  ;; no realgud features exist by default
  (should (= 0 (length (realgud-test-helper))))
  (should-not (member 'realgud-pdb features))

  (load-file "realgud.el") ; manually load the first time

  ;; we should now have realgud features;
  (should-not (= 0 (length (realgud-test-helper))))
  (should (member 'realgud-pdb features))
  ;; test at least 1 by name
  (should (member 'realgud-pdb features))

  ;; unload all and test
  (let ((removed (realgud:unload-features)))
	(should-not (= 0 (length removed)))) ; check that we return removed values
  (let ((removed (realgud:unload-features))) ; should not err out if called on empty.
	(should (= 0 (length removed))))

  (realgud:load-features) ; load and test
  (should-not (= 0 (length (realgud-test-helper))))
  (should (member 'realgud-pdb features)))

