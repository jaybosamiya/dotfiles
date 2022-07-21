;; Inspired by (but distinct from)
;; https://robert.kra.hn/posts/rust-emacs-setup/

(use-package rustic
  :ensure
  :after (lsp-mode)
  :bind (:map rustic-mode-map
              ("C-'" . xref-find-definitions-other-window)
              ("M-'" . lsp-find-references) ;; replaces `abbrev-prefix-mark`
              ("C-c C-c C-a" . lsp-execute-code-action)
              ;; ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c r" . lsp-rename) ;; replaces `rustic-cargo-rm`
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              )
  :custom-face
  (lsp-rust-analyzer-inlay-face
   ;; Default is just `font-lock-comment-face` which can be confusing
   ;; with actual comments. Consider if there is a different style
   ;; that might work better?
   ((t (:weight light :inherit (font-lock-comment-face)))))
  :config
  ;; (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)
  (setq rustic-format-on-save t)

  ;; ;; what to use when checking on-save. "check" is default
  ;; (setq lsp-rust-analyzer-cargo-watch-command "clippy")

  ;; Enable/disable specific hints
  (setq lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints nil
        lsp-rust-analyzer-display-reborrow-hints nil))

(provide 'f0xtr0t-lang-rust-lsp)
