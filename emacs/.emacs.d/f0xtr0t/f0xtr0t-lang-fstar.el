;; Set up F* integration

(use-package fstar-mode
  :mode (("\\.fst\\'" . fstar-mode)
         ("\\.fsti\\'" . fstar-mode))
  :config
  (setq fstar-executable "fstar.exe")
  (setq fstar-smt-executable "z3")
  (setq fstar-subp-prover-args
        (lambda ()
          `(
            "--use_hints" ;; "--record_hints"
            ;; "--detail_hint_replay"
            ;; "--include" "/home/jay/everest/kremlin/kremlib"
            "--cache_checked_modules"
            ,@(when (string-match-p (regexp-opt '("hacl-star/vale" "Vale.")) (buffer-file-name))
                '(
                  "--trivial_pre_for_unannotated_effectful_fns" "false"
                  "--warn_error" "+241@247-272"
                  "--z3cliopt" "smt.arith.nl=false"
                  "--z3cliopt" "smt.QI.EAGER_THRESHOLD=100"
                  "--z3cliopt" "smt.CASE_SPLIT=3"
                  "--use_extracted_interfaces" "true"
                  "--max_fuel" "1"
                  "--max_ifuel" "1"
                  "--initial_ifuel" "0"
                  "--smtencoding.elim_box" "true"
                  "--smtencoding.l_arith_repr" "native"
                  "--smtencoding.nl_arith_repr" "wrapped")))))
  (defun my-fstar-compute-prover-args-using-make ()
    "Construct arguments to pass to F* by calling make."
    (with-demoted-errors "Error when constructing arg string: %S"
      (let* ((fname (file-name-nondirectory buffer-file-name))
             (target (concat fname "-in"))
             (argstr (shell-command-to-string (concat "make --quiet " target " 2>/dev/null"))))
        (split-string argstr))))
  (setq fstar-subp-prover-additional-args #'my-fstar-compute-prover-args-using-make)
  (defun fstar-set-to-release-paths ()
    (interactive)
    (setq fstar-prefix-path "/opt/fstar-release/")
    (setq fstar-executable (concat fstar-prefix-path "bin/fstar.exe"))
    (setq fstar-smt-executable (concat fstar-prefix-path "bin/z3")))
  (require 'fstar-rewrite)
  (defun fstar-indent-buffer ()
    (interactive)
    (if fstar-indent-buffer-must-run
        (save-excursion
          (mark-whole-buffer)
          (fstar-indent-region (region-beginning)
                               (region-end))) ()))
  (defun toggle-fstar-indent-buffer ()
    (interactive)
    (setq fstar-indent-buffer-must-run
          (not fstar-indent-buffer-must-run))
    (message (if fstar-indent-buffer-must-run "Now on" "Now off")))
  (setq fstar-indent-buffer-must-run nil)
  (require 'ocp-indent)
  (defun ocp-indent-buffer ()
    (interactive)
    (if ocp-indent-buffer-must-run
        (save-excursion
          (mark-whole-buffer)
          (ocp-indent-region (region-beginning)
                             (region-end))) ()))
  (defun toggle-ocp-indent-buffer ()
    (interactive)
    (setq ocp-indent-buffer-must-run
          (not ocp-indent-buffer-must-run))
    (message (if ocp-indent-buffer-must-run "Now on" "Now off")))
  (setq ocp-indent-buffer-must-run nil)
  (defun killall-z3 ()
    (interactive)
    (call-process "killall" nil nil nil "z3"))
  (defun fstar-show-admits-and-assumes (&optional prefix)
    (interactive "P")
    (if prefix
        (occur "admit\\|assume\\|TODO\\|WARN\\|FIXME\\|XXX\\|UNSOUND\\|REVIEW\\|WAT\\|NEVERCO[M]MIT")
      (occur "admit\\|assume")))
  (defun fstar-confirm-before-kill (&optional arg)
    (interactive "P")
    (when (y-or-n-p "Are you sure you want to kill the F* process? ")
      (fstar-subp-kill-one-or-many arg)))
  (add-hook 'fstar-mode-hook
            (lambda ()
              (superword-mode 1)
                                        ; (auto-fill-mode)
              (ocp-setup-indent)
              ;; (add-hook 'fstar-newline-hook
              ;;              (lambda (ignored-arg) (fstar-indent-line)) nil t)
              (add-hook 'before-save-hook 'ocp-indent-buffer nil t)
              (local-set-key (kbd "C-c C-k") 'killall-z3)
              (local-set-key (kbd "C-'") 'fstar-jump-to-definition-other-window)
              (local-set-key (kbd "M-'") 'fstar-jump-to-related-error-other-window)
              (local-set-key (kbd "M-,") 'xref-pop-marker-stack) ; works nicely with M-.
              (local-set-key (kbd "<f5>") 'fstar-show-admits-and-assumes)
              (local-set-key (kbd "C-c C-x") 'fstar-confirm-before-kill)
              ))
  )

(provide 'f0xtr0t-lang-fstar)
