;; Set up Dafny integration
(use-package boogie-friends
  :mode ("\\.dfy\\'" . dafny-mode)
  :init
  (add-hook 'dafny-mode-hook
            (lambda ()
              (setq imenu-generic-expression
                    '((nil "\\_<datatype\\_> +\\(\\_<.*?\\_>\\)\\( \\|$\\)" 1)
                      (nil "\\_<type\\_> +\\(\\_<.*?\\_>\\)\\( \\|$\\)" 1)
                      (nil "\\_<class\\_> +\\(\\_<.*?\\_>\\)\\( \\|$\\)" 1)
                      (nil "\\_<function method\\_> *?\\({:.*?}\\)? +\\(\\_<.*?\\_>\\) ?(" 2)
                      (nil "\\_<function\\_> *?\\({:.*?}\\)? +\\(\\_<.*?\\_>\\) ?(" 2)
                      (nil "\\_<method\\_> *?\\({:.*?}\\)? +\\(\\_<.*?\\_>\\) ?(" 2)
                      (nil "\\_<predicate\\_> *?\\({:.*?}\\)? +\\(\\_<.*?\\_>\\) ?(" 2)
                      (nil "\\_<lemma\\_> *?\\({:.*?}\\)? +\\(\\_<.*?\\_>\\) ?(" 2)))
              (local-set-key (kbd "M-.") 'push-marker-stack-and-imenu-anywhere)
              (electric-indent-mode -1)))
  :config
  (setq dafny-prefix-path "/opt/dafny/Binaries/")
  (setq flycheck-dafny-executable (concat dafny-prefix-path "dafny"))
  (setq flycheck-z3-executable (concat dafny-prefix-path "z3/bin/z3"))
  (setq dafny-prover-background-args '("/timeLimit:30" "/autoTriggers:1" "/printTooltips" "/allocated:3"))
  (setq flycheck-inferior-dafny-executable (concat
                                            dafny-prefix-path "dafny-server"))
  (progn ;; Make it easy to jump to errors from compilation mode :)
    (push 'dafny compilation-error-regexp-alist)
    (push '(dafny "^\\([^ ]*?\\)(\\([0-9]+\\),\\([0-9]+\\)): " 1 2 3) compilation-error-regexp-alist-alist)))

(provide 'f0xtr0t-lang-dafny)
