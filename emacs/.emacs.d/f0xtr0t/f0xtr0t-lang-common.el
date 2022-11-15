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


;; Turn on global auto completion
(use-package company
  :ensure t
  :delight
  :hook (after-init . global-company-mode)
  :config
  (setq company-require-match nil) ;; Allow easily quitting out of completions
  :bind (("M-<SPC>" . company-complete)) ;; Requires disabling M-SPC
                                         ;; from Gnome (replaced with
                                         ;; Super-SPC there)
  )

;; imenu-anywhere lets you jump between relevant parts of code easily
(use-package imenu-anywhere
  :ensure t
  ;; :bind (("C-." . imenu-anywhere))
  )

(defun push-marker-stack-and-imenu-anywhere ()
  (interactive)
  (xref-push-marker-stack)
  (imenu-anywhere))

(defun replace-smart-chars-in-region (beg end)
  "Replace 'smart quotes' and such in the region with
ascii quotes."
  (interactive "r")
  (format-replace-strings
   '(("\x201C" . "\"")
     ("\x201D" . "\"")
     ("\x2018" . "'")
     ("\x2019" . "'"))
   nil beg end))
(defun replace-smart-chars-in-buffer ()
  "Replace 'smart quotes' and such in the buffer with
ascii quotes."
  (interactive)
  (replace-smart-chars-in-region (point-min) (point-max)))
(defun replace-smart-chars-dwim ()
  "Replace 'smart quotes' and such in the region or buffer with
ascii quotes."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (replace-smart-chars-in-region (region-beginning) (region-end))
          (message "Replaced smart chars in region."))
      (progn
        (replace-smart-chars-in-buffer)
        (message "Replaced smart chars in buffer.")))))

;; Much nicer format-on-save mode
(use-package apheleia
  :ensure t
  :init (apheleia-global-mode +1))

;; Use projectile for easily moving around in projects
(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind
  ("C-o" . projectile-multi-occur)) ;; Replaces open-line

(provide 'f0xtr0t-lang-common)
