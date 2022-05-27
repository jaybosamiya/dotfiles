;; Remove annoying UI elements
;;
;; By setting this as variable(s), rather than invoking the mode with
;; `-1` as argument, we effectively suppress creation even before the
;; GUI frames show up, thereby speeding up startup
;;
;; TODO: Maybe this should move to early-init?
(setq tool-bar-mode nil
      menu-bar-mode nil)
(scroll-bar-mode -1)

;; Make minibuffer history persist across sessions
(savehist-mode 1)

;; Be able to easily edit the minor mode stuff that shows up in the modeline
(use-package delight
  :ensure t
  :demand t)

;; Set up IDO nicely
(require 'ido)
(ido-mode t)
(require 'flx-ido)
(flx-ido-mode t)
(global-set-key (kbd "C-x C-d") #'ido-dired) ;; Map "C-x C-d" to do same as "C-x d" which is otherwise awkward.

;; Use IDO for yes-or-no-p and y-or-n-p
(use-package ido-yes-or-no
  :ensure t
  :init (ido-yes-or-no-mode t))

(display-time-mode 1)

(use-package which-key
  :ensure t
  :demand t
  :delight
  :config (which-key-mode))

;; Disable audible bell
(setq ring-bell-function 'ignore)


;; Turn on show-trailing-whitespace, but disable on some modes
(setq-default show-trailing-whitespace t)
(dolist (hook '(term-mode-hook))
  (add-hook hook '(lambda () (setq show-trailing-whitespace nil))))

;; Smoothen scrolling
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)
;; (use-package smooth-scrolling ;; Doesn't seem to work well with F*
;;   :ensure t
;;   :config
;;   (setq smooth-scroll-margin 1))
;; (smooth-scrolling-mode 1)

;; Display a nicer startup message :D
(defun display-startup-echo-area-message ()
  (message "Let the hacking begin!"))

;; Make scratch buffer be a fundamental-mode buffer
;; and give a better message (empty :D)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "")

;; Do a pulse whenever jumping to a line
(require 'pulse)
(defun goto-line-and-pulse ()
  "Equivalent to goto-line interactively except that it also does
a pulse"
  (interactive)
  (let* ((line
          (read-number (format "Goto line: ")
                       (list (line-number-at-pos)))))
    (goto-line line)
    (pulse-momentary-highlight-one-line (point))))
(global-set-key (kbd "M-g M-g") 'goto-line-and-pulse)
(global-set-key (kbd "M-g g") 'goto-line-and-pulse)


;; Turn on line numbers for all buffers
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode) ;; use faster version when available
  (global-linum-mode))

;; Disable linum-mode for incompatible cases
;;
;; NOTE: olivetti-mode does not work well with linum, but we don't
;; need to disable display-line-numbers-mode for it, so I've removed
;; it from here.
(dolist (hook '(pdf-view-mode-hook image-mode-hook))
  (add-hook hook '(lambda ()
                    (linum-mode 0)
                    (when (version<= "26.0.50" emacs-version)
                      (display-line-numbers-mode 0)))))


;; Theme flipper
(setq
 theme-flipper-list '(misterioso solarized-light solarized-dark adwaita)
 theme-flipper-index 0)
(defun theme-flip ()
  (interactive)
  (setq theme-flipper-index (+ 1 theme-flipper-index))
  (when (>= theme-flipper-index (length theme-flipper-list))
    (setq theme-flipper-index 0))
  (let ((this-theme (cl-nth-value theme-flipper-index theme-flipper-list)))
    (load-theme this-theme t t)
    (dolist (theme theme-flipper-list)
      (when (not (eq theme this-theme))
        (disable-theme theme)))
    (enable-theme this-theme)))
(global-set-key (kbd "C-<f12>") 'theme-flip)

;; Ensure that copying from another program and then running a kill
;; command in emacs doesn't cause things to disappear from the
;; clipboard
(setq save-interprogram-paste-before-kill t)

;; Make sure the mouse yanking pastes at point instead of at click
(setq mouse-yank-at-point t)

;; Be able to move between buffers more easily, using M-up, M-down,
;; M-left, M-right.
(require 'framemove)
(windmove-default-keybindings 'meta)
(setq framemove-hook-into-windmove t)

(use-package buffer-move
  :ensure t
  :bind
  ("<M-S-up>" . buf-move-up)
  ("<M-S-down>" . buf-move-down)
  ("<M-S-left>" . buf-move-left)
  ("<M-S-right>" . buf-move-right))


;; Always have column number mode on
(column-number-mode 1)

;; Add the doom modeline
;; If fonts don't work, use "M-x all-the-icons-install-fonts"
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom-face (doom-modeline-info ((t (:inherit bold))))
  :config
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-vcs-max-length 40))

;; Introduce C-M-= and C-M-- for changing the font size all across emacs.
(use-package default-text-scale
  :ensure t
  :demand t
  :config (default-text-scale-mode))

;; Set the default height-split threshold to a larger value (default
;; otherwise is 80) so that horizontal splits are preferred more
;; strongly over vertical splits, when a split in a non-specific
;; direction is requested.
;;
;; TODO: Consider setting this to `nil` to completely disallow
;; vertical splits.
(setq-default split-height-threshold 120)

;; Enable delete-selection-mode which allows behavior that is more
;; consistent with other applications- selections are replaced when
;; you type over them, rather than just inserting at point.
(delete-selection-mode t)

;; Add search counters to the modeline
(use-package anzu
  :ensure t
  :init
  (global-anzu-mode +1))

;; Add indentation guides to make code easier to read
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'bitmap
              highlight-indent-guides-responsive 'top))

;; Save and restore emacs sessions upon restart
;;
;; Use `emacs --no-desktop` to startup without previously saved
;; desktop, and `M-x desktop-clear` to actually clear it.
(use-package desktop
  :demand t ;; Ensure that this package is not deferred
  :init
  ;; Restore these many buffers eagerly. Remaining are
  ;; restored lazily. Helps speed things up a bit.
  (setq desktop-restore-eager 5)
  :config
  (desktop-save-mode 1))

;; Clear out any buffers that have not been used for a few days
;;
;; Use `(cancel-timer midnight-timer)` to disable midnight mode.
(use-package midnight
  :demand t ;; Ensure that this package is not deferred
  :init
  ;; Clear out any buffer not used for a full week.
  (setq clean-buffer-list-delay-general 7)
  ;; Since the machine may not be up at midnight itself, try
  ;; to do a clean every few hours. This should hopefully be
  ;; frequent enough to clear out old buffers, but not too
  ;; frequent to cause annoyances.
  (setq midnight-period (* 6 60 60))
  :config
  (midnight-mode 1))

(provide 'f0xtr0t-gui)
