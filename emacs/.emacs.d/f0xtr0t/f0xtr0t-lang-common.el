;; Automatically analyze files to decide whether to use tabs or
;; spaces, and if spaces, what indent offset to use.
;;
;; Useful commands:
;;   M-x dtrt-indent-diagnosis
;;   M-x dtrt-indent-highlight
;;   M-x dtrt-indent-undo
(use-package dtrt-indent
  :ensure t
  :init (dtrt-indent-global-mode 1))

;; Even if we don't use custom snippets, it appears that we need
;; yas-minor-mode to be set up properly so that company completions
;; don't end up with the ugly `$0`, `$1` stuff.
(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)))

(provide 'f0xtr0t-lang-common)
