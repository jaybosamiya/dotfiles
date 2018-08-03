(defun opam-path (path)
  (let ((opam-share-dir
	 (shell-command-to-string
	  "echo -n `opam config var share`")))
    (concat opam-share-dir "/" path)))

(add-to-list 'load-path (opam-path "emacs/site-lisp"))
(add-to-list 'load-path (opam-path "tuareg"))


(load "tuareg-site-file")

(require 'ocp-indent)
(require 'merlin)
(require 'company)
(add-to-list 'company-backends 'merlin-company-backend)
(add-hook 'merlin-mode-hook 'company-mode)

(define-key merlin-mode-map (kbd "C-c TAB") 'company-complete)
(define-key merlin-mode-map (kbd "C-c C-d") 'merlin-document)
(define-key merlin-mode-map (kbd "C-c d") 'merlin-destruct)


(setq merlin-completion-with-doc t)
(setq merlin-use-auto-complete-mode nil)
(setq tuareg-font-lock-symbols t)
(setq merlin-command 'opam)
(setq merlin-locate-preference 'mli)

(defun change-symbol (x y)
  (setcdr (assq x tuareg-font-lock-symbols-alist) y))

(add-hook 'tuareg-mode-hook
	  (lambda ()
	    (merlin-mode)
	    (local-set-key (kbd "C-c c") 'recompile)
	    (local-set-key (kbd "C-c C-c") 'recompile)
	    (auto-fill-mode)
	    (tuareg-make-indentation-regexps)
	    (setq ocp-indent-buffer-must-run t) ; This enables use of the
						; ocp-indent-buffer from init.el
	    (add-hook 'before-save-hook 'ocp-indent-buffer nil t)))

(defun opam-env ()
  (interactive nil)
  (dolist (var
	   (car (read-from-string
		 (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var))))

(provide 'ocaml)
