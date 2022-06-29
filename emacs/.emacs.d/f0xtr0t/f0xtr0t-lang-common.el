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

(provide 'f0xtr0t-lang-common)
