;;; boogie-friends-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "boogie-mode" "boogie-mode.el" (23006 29871
;;;;;;  901171 341000))
;;; Generated autoloads from boogie-mode.el

(add-to-list 'auto-mode-alist '("\\.bpl\\'" . boogie-mode))

(autoload 'boogie-mode "boogie-mode" "\
Major mode for editing Boogie programs.

\\{boogie-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "dafny-mode" "dafny-mode.el" (23006 29871 905171
;;;;;;  145000))
;;; Generated autoloads from dafny-mode.el

(autoload 'dafny-test-suite-open-diff "dafny-mode" "\


\(fn DFY-NAME)" t nil)

(autoload 'dafny-test-suite-accept-diff "dafny-mode" "\


\(fn DFY-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.dfy\\'" . dafny-mode))

(autoload 'dafny-mode "dafny-mode" "\
Major mode for editing Dafny programs.

\\{dafny-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "z3-smt2-mode" "z3-smt2-mode.el" (23006 29871
;;;;;;  905171 145000))
;;; Generated autoloads from z3-smt2-mode.el

(add-to-list 'auto-mode-alist '("\\.smt2\\'" . z3-smt2-mode))

(autoload 'z3-smt2-mode "z3-smt2-mode" "\
Major mode for editing SMT2 programs.

\\{z3-smt2-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("boogie-friends-pkg.el" "boogie-friends.el"
;;;;;;  "dafny-docs.el" "inferior-dafny.el") (23006 29871 912996
;;;;;;  259000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; boogie-friends-autoloads.el ends here
