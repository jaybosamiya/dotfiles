;; (require 'latex-preview-pane)

(use-package tex
  :ensure auctex
  :demand t)

(add-hook 'TeX-mode-hook
          '(lambda ()
             ;; Enable the emacs server so that synctex can work (eg,
             ;; in skim: shift+cmd+click)
             (server-mode t)
             (TeX-fold-mode 1)
             (visual-line-mode)
             ;; (define-key LaTeX-mode-map (kbd "M-p")
             ;;   '(lambda ()
             ;;      (interactive)
             ;;      (latex-preview-pane-mode)))
             ;; (define-key LaTeX-mode-map (kbd "<f9>")
             ;;   '(lambda ()
             ;;      (interactive)
             ;;      (TeX-fold-buffer)
             ;;      (preview-document)
             ;;      )
             ;;   )
             (define-key TeX-mode-map (kbd "C-c C-c")
               ;; I don't really use `TeX-command-master` and usually
               ;; have a `Makefile` lying around to actually perform
               ;; the compilation, so why not just assign to using
               ;; that?
               'recompile)
             (define-key TeX-mode-map (kbd "C-c C-S-c")
               ;; Reassign `TeX-command-master` to `C-c C-S-c` since
               ;; it still might be useful to have around
               'TeX-command-master)
             (highlight-regexp "\\\\comment{[^}]*}" 'superscript)
             (highlight-regexp "\\\\comment" 'hi-blue)
             (highlight-regexp "\\\\jay{[^}]*}" 'superscript)
             (highlight-regexp "\\\\jay" 'hi-blue)
             (highlight-regexp "\\\\todo{[^}]*}" 'superscript)
             (highlight-regexp "\\\\todo" 'hi-blue)
             (highlight-regexp "\\\\citationneeded{[^}]*}" 'superscript)
             (highlight-regexp "\\\\citationneeded" 'hi-blue)
             )
          )

;; Allow the LaTeX-narrow-to-environment command be run without
;; prompting (i.e., `C-x n e`)
(put 'LaTeX-narrow-to-environment 'disabled nil)

(provide 'f0xtr0t-lang-latex)
