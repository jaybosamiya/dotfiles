;; MacOS specific stuff
(if (string-equal system-type "darwin")
    (progn

      ;; MacOS-specific keybindings to make faster movements match with rest
      ;; of system. Ctrl-Movements however unfortunately are captured by the
      ;; OS and end up not being passed to Emacs
      (global-set-key (kbd "A-<right>") 'right-word)
      (global-set-key (kbd "A-<left>") 'left-word)
      (global-set-key (kbd "A-<up>") 'backward-paragraph)
      (global-set-key (kbd "A-<down>") 'forward-paragraph)
      (global-set-key (kbd "A-<backspace>") 'backward-kill-word)

      ;; Unset left/right two-finger swipes, otherwise Emacs on MacOS
      ;; decides to switch buffers when you do it which is quite unsettling.
      (global-unset-key (kbd "<swipe-left>"))
      (global-unset-key (kbd "<swipe-right>"))

      ;; Enable font ligatures on MacOS
      (mac-auto-operator-composition-mode t)

      ;; Add a convenient override to `toggle-frame-fullscreen` that takes
      ;; into account the notch on the Mac
      (defun toggle-frame-fullscreen (&optional frame)
        "Toggle fullscreen state of FRAME.
Modified from original code in frame.el, replacing 'fullboth with 'fullscreen
because otherwise on MacOS, it expands too far and overflows into the notch."
        (interactive)
        (let ((fullscreen (frame-parameter frame 'fullscreen)))
          (if (memq fullscreen '(fullscreen fullboth))
	      (let ((fullscreen-restore (frame-parameter frame 'fullscreen-restore)))
	        (if (memq fullscreen-restore '(maximized fullheight fullwidth))
	            (set-frame-parameter frame 'fullscreen fullscreen-restore)
	          (set-frame-parameter frame 'fullscreen nil)))
            (modify-frame-parameters
             frame `((fullscreen . fullscreen) (fullscreen-restore . ,fullscreen))))
          ;; Manipulating a frame without waiting for the fullscreen
          ;; animation to complete can cause a crash, or other unexpected
          ;; behavior, on macOS (bug#28496).
          (when (featurep 'cocoa) (sleep-for 0.5))))

      ;; Set up dired to use `gls` instead of `ls`. This helps fix the
      ;; issue around `dired-use-ls-dired`; specifically, dired likes
      ;; to use the `--dired` option with ls, which is only supported
      ;; on GNU ls, and not on BSD ls. A fix for this is to just use
      ;; lisp-implemented `ls` (described in the help for
      ;; `dired-use-ls-dired`), or to tell it not to use `--dired`,
      ;; but since we've got `gls` around, why not use it?
      (setq insert-directory-program "gls")

;; End of MacOS specific stuff
      ))

;;;;;; Notes on setup for other programs on MacOS
;;
;;;; SyncTex support for Skim
;;
;; Make sure `emacsclient` is on the PATH
;;
;;    /usr/local/bin/emacsclient -> /Applications/Emacs.app/Contents/MacOS/bin/emacsclient
;;
;; and synctex is enabled in Skim with default Emacs settings.
;; Cmd-Shift-Click should then just work to jump from Skim to Emacs.
;;
;; TODO: Add support for forward search from emacs to skim

(provide 'f0xtr0t-mac-os-specific)
