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
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)

  (setq rustic-format-on-save t
        ;; Due to a variety of reasons, the rustfmt folks don't want
        ;; to move away from `--edition 2015` by default. I _could_
        ;; introduce a `rustfmt.toml` to every project that specifies
        ;; the edition, but that is annoying. Personally, I have never
        ;; used the 2015 edition, and also the formatting settings
        ;; between 2018 and 2021 are practically the same, so let's
        ;; just use 2021 everywhere, until I figure out a clean way to
        ;; get the edition from the nearest `Cargo.toml`.
        rustic-rustfmt-args "--edition 2021")

  ;; ;; what to use when checking on-save. "check" is default
  ;; (setq lsp-rust-analyzer-cargo-watch-command "clippy")

  ;; Enable/disable specific hints
  (setq lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints nil
        lsp-rust-analyzer-display-reborrow-hints nil)

  :hook (rust-mode . (lambda ()
                       (setq indent-tabs-mode nil)
                       (setq split-height-threshold nil)
                       ;; Set comment wrapping M-q default to `max_width`
                       ;; default from rustfmt
                       (setq-local fill-column 100))))

(provide 'f0xtr0t-lang-rust-lsp)
