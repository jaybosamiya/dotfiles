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

;; End of MacOS specific stuff
      ))


(provide 'f0xtr0t-mac-os-specific)
