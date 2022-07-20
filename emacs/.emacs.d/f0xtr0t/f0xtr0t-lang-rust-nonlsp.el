(use-package rust-mode
  :ensure t
  :defer t
  :config
  (setq rust-format-on-save t
        rust-format-show-buffer nil)
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq split-height-threshold nil)
              ;; Set comment wrapping M-q default to `max_width`
              ;; default from rustfmt
              (setq-local fill-column 100)
              ;; Prevent rust from hijacking the nice fold-this mode
              (define-key rust-mode-map (kbd "C-c C-f") nil))))

(use-package cargo
  :ensure t
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package racer
  :ensure t
  :after rust-mode
  :hook (rust-mode . racer-mode)
  :config
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  ;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t)
  (setq racer-rust-src-path
        ;; Workaround for changes to where std is stored until (see
        ;; https://github.com/racer-rust/emacs-racer/pull/143)
      (let* ((sysroot (string-trim
                       (shell-command-to-string "rustc --print sysroot")))
             (lib-path (concat sysroot "/lib/rustlib/src/rust/library"))
              (src-path (concat sysroot "/lib/rustlib/src/rust/src")))
        (or (when (file-exists-p lib-path) lib-path)
            (when (file-exists-p src-path) src-path))))
  :bind (:map racer-mode-map
         ("C-'" . racer-find-definition-other-window)))

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup)
  :hook (rust-mode . flycheck-mode))

;; (use-package flycheck-pos-tip
;;   :ensure t
;;   ;; :hook (rust-mode . flycheck-pos-tip-mode)
;; )

(use-package flycheck-popup-tip
  :ensure t
  :hook (rust-mode . flycheck-popup-tip-mode)
)

(provide 'f0xtr0t-lang-rust-nonlsp)
