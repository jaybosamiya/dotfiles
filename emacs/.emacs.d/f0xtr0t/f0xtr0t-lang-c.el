;; Set up gdb to use the many-windows functionality
(setq gdb-many-windows t)

;; Allow creation of ctags info from inside emacs itself :)
(setq path-to-ctags "/usr/bin/ctags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R '%s'" path-to-ctags (file-truename (directory-file-name dir-name)))))

;; To use gtags, must have run `apt install global exuberant-ctags`
;; first
(use-package ggtags
  :ensure t
  :hook ((c-mode c++-mode java-mode) . ggtags-mode)
  :custom-face (ggtags-highlight ((t nil)))
  :init (setq ggtags-enable-navigation-keys nil))

;; Let emacs learn and set style from a C file
(defun infer-indentation-style ()
  (interactive)
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and
  ;; if neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t)))
  (message "Inferred indentation"))
(defun c-guess-and-set-style ()		; TODO: Check file size and
                                        ; ask for permission if too
                                        ; large, to speed things up
                                        ; for large files.
  (interactive)
  (let
    ((stylename (concat "guessed-style-" (file-name-base))))
  (c-guess-buffer-no-install)
  (c-guess-install stylename)
  (c-set-style stylename)
  (message (concat "Installed and set " stylename))))
(add-hook 'c-mode-common-hook
          (lambda ()
            (infer-indentation-style)
            ;; (c-guess-and-set-style)
            ;; ;; Disabled guessing by default, to speed up file
            ;; ;; opens for large files.
            ))

;; Be able to move around C/C++ projects easily using cscope.
;; Try C-c s SOMETHING in a C/C++ buffer.
(use-package xcscope
  :ensure t
  :defer t
  :config
  (require 'cc-mode)
  (cscope-setup)
  :hook (c-mode . cscope-minor-mode)
  :bind (:map c-mode-map
         ("C-c C-s" . 'cscope-display-buffer)))

(use-package semantic
  :ensure t
  :defer t
  :hook (c-mode . semantic-mode))

(use-package srefactor
  :ensure t
  :defer t
  :config (semantic-mode 1)
  :bind ("M-RET" . 'srefactor-refactor-at-point))

(provide 'f0xtr0t-lang-c)
