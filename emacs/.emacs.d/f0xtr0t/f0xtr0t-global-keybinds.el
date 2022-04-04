;; Useful keybindings for maximizing or full-screening
(global-set-key (kbd "M-<f10>") 'toggle-frame-maximized)
(global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)

;; Prevent C-z from accidentally sending the window to background
(global-unset-key (kbd "C-z"))

;; Prevent F11 from accidentally trying to maximize the window
(global-unset-key (kbd "<f11>"))

;; Handle "Page Up" and "Page Down" better
(global-set-key (kbd "<next>") 'scroll-up-line)
(global-set-key (kbd "<prior>") 'scroll-down-line)
(global-unset-key (kbd "C-<prior>"))
(global-unset-key (kbd "C-<next>"))

;; Make F-12 clear all flycheck marks
(global-set-key (kbd "<f12>") 'flycheck-clear)

;; amx -- newer fork of smex which stopped development in 2015
(use-package amx
  :ensure t
  :demand t
  :bind (; Replace with amx
         ("M-x" . amx)
         ("M-X" . amx-major-mode-commands)
         ; and maintain old M-x via C-c M-x
         ("C-c M-x" . execute-extended-command)))

;; Be able to unfill paragraphs
(require 'unfill)
(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; Enable abbrev-mode globally
;; (setq-default abbrev-mode t) ; Disabled
;;; Useful shortcut: C-x a i g
;;; Replaces previous word and sets up an abbrev

;; Allow "C-x n n" to be done without that irritating warning
(put 'narrow-to-region 'disabled nil)

;; Set M-, to pop-tag-mark to work nicely with the M-. of ctags
;; This clobbers tags-loop-continue however
(global-set-key (kbd "M-,") 'pop-tag-mark)

;; Set C-' to correct word using flyspell, and F9 to flyspell the
;; entire buffer. C-F9 to disable flyspell.
;; (global-set-key (kbd "C-'")
;;              'flyspell-correct-word-before-point)
(defun flyspell-enable ()
  (interactive)
  (if (derived-mode-p 'prog-mode)
      (flyspell-prog-mode)
    (flyspell-mode t))
  (flyspell-buffer)
  (message "Turned on flyspell-mode"))
(defun flyspell-disable ()
  (interactive)
  (flyspell-mode -1)
  (message "Turned off flyspell-mode"))
(global-set-key (kbd "<f9>") 'flyspell-enable)
(global-set-key (kbd "C-<f9>") 'flyspell-disable)

;; Use a hippie-expand, instead of dabbrev-expand, which has
;; dabbrev-expand as one of its tactics, so leads to a guaranteed
;; superset of expansions
(global-set-key (kbd "M-/") 'hippie-expand)

;; Use ibuffer instead of list-buffer for C-x C-b
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Bind "occur" to M-o instead of facemenu stuff
(global-set-key (kbd "M-o") 'occur)

;; Allow multi line editing.
;; Use using C-; when over a symbol
(use-package iedit
  :ensure t
  :defer t
  :bind ("C-;" . iedit-mode))

;; Make C-x 1 (delete-other-windows) reversible
(use-package zygospore
  :ensure t
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

(global-set-key (kbd "C-x 4 c") 'clone-indirect-buffer)

(global-set-key (kbd "C-M-<f7>") 'normal-mode)

;; Make case-changing much better to work with
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;; Allow commenting or uncommenting full sexps in one go, rather than
;; having to do so individually by line.
(use-package comment-or-uncomment-sexp
  :ensure t
  :bind ("C-M-;" . comment-or-uncomment-sexp))


;; Add expand-region, which allows you to repeatedly press a key to
;; select larger and larger semantic regions.
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; fold-this
(use-package fold-this
  :ensure t
  :demand t
  :bind (("C-c C-f" . fold-this-all)
         ("C-c C-S-f" . fold-this)
         ("C-c M-f" . fold-this-unfold-at-point)
         ("C-c M-F" . fold-this-unfold-all)))

;; Get some distraction free goodness :)
(use-package olivetti
  :ensure t
  :bind ("C-<f11>" . olivetti-mode)
  :config
  (progn
    (setq olivetti-hide-mode-line t)
    (setq-default olivetti-body-width 116)))

(provide 'f0xtr0t-global-keybinds)
