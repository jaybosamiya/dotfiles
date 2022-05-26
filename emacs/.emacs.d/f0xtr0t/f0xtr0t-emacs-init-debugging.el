(defun f0xtr0t---exit-load-of-init-file-here ()
  "This function exits loading of the init.el file at the point
that it is called. Should only be used temporarily for debugging
purposes.

Emacs uses the load function to execute lisp file through a
temporary buffer, that's ` *load*` (with the space in front), if
the name is not already taken. The above code moves the point to
the end of the buffer, thus the read function will not read
further code."
  (with-current-buffer " *load*"
    (goto-char (point-max))))

;; Emacs Start Up Profiler
(use-package esup
  :ensure t
  :pin melpa
  :init (setq
         esup-depth 0
         esup-user-init-file (file-truename "~/.emacs.d/init.el")))


(provide 'f0xtr0t-emacs-init-debugging)
