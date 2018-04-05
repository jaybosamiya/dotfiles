(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(delete-selection-mode nil)
 '(display-time-mode t)
 '(flyspell-default-dictionary "british")
 '(font-use-system-font t)
 '(global-auto-revert-mode t)
 '(ido-enable-flex-matching t)
 '(ido-ubiquitous-mode t)
 '(inhibit-startup-screen t)
 '(magit-diff-refine-hunk t)
 '(menu-bar-mode nil)
 '(minimap-dedicated-window nil)
 '(minimap-hide-fringes t)
 '(minimap-highlight-line nil)
 '(minimap-sync-overlay-properties (quote (face invisible)))
 '(minimap-update-delay 0.0)
 '(minimap-width-fraction 0.1)
 '(minimap-window-location (quote right))
 '(send-mail-function (quote mailclient-send-it))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 113 :width normal))))
 '(minimap-active-region-background ((t (:inverse-video t)))))

;; Start off emacs server, so that `emacsclient` can be used
(load "server")
(if (server-running-p)
    (message "%s" "Server already started by someone else")
  (server-start))

;;;; Have a good dev environment for OCaml
;;; Disabled for now
;; (setenv "PATH" (concat (getenv "PATH") ":/home/jay/.opam/4.02.3/bin"))
;; (setq exec-path (append exec-path '("/home/jay/.opam/4.02.3/bin")))
;; (add-to-list 'load-path "~/.emacs.d/ocaml/")
;; (require 'ocaml)

;; Remove annoying UI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Allow loading customizations from the f0xtr0t directory
(add-to-list 'load-path
	     (concat user-emacs-directory
		     (convert-standard-filename "f0xtr0t/")))


;; Set up IDO nicely
(require 'ido)
(ido-mode t)
(require 'flx-ido)
(flx-ido-mode t)

;; Get some distraction free goodness :)
(require 'olivetti)
(global-set-key (kbd "C-<f11>") 'olivetti-mode)
(setq olivetti-hide-mode-line t)

;; smex
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is the old M-x.
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

;; imenu-anywhere lets you jump between relevant parts of code easily
(require 'imenu-anywhere)
(global-set-key (kbd "C-.") #'imenu-anywhere)

(display-time-mode 1)

(require 'pdf-tools)
(pdf-tools-install)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'org-mode-hook
	  #'(lambda ()
	      ;; make the lines in the buffer wrap
	      ;; around the edges of the screen.
	      (visual-line-mode)
	      (org-indent-mode)
	      ;; Allow shift selection
	      (setq org-support-shift-select t)
	      ;; Hide emphasis markers (/, _, etc)
	      (setq org-hide-emphasis-markers t)
	      ;; Disable table-of-contents generation
	      (setq org-export-with-toc nil)
	      ;; Allow quotes inside of emphasis sections : Based off
	      ;; of https://stackoverflow.com/a/24173780/3696619
	      (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n")
	      (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
	      ;; Enable extra backends
	      (setq org-export-backends
		    (quote (ascii html icalendar latex md deck)))))

(require 'which-key)
(which-key-mode)

;; Prevent C-z from accidentally sending the window to background
(global-unset-key (kbd "C-z"))

;; Handle "Page Up" and "Page Down" better
(global-set-key (kbd "<next>") 'scroll-up-line)
(global-set-key (kbd "<prior>") 'scroll-down-line)
(global-unset-key (kbd "C-<prior>"))
(global-unset-key (kbd "C-<next>"))

(require 'latex-preview-pane)
(add-hook 'TeX-mode-hook
	  '(lambda ()
	     (TeX-fold-mode 1)
	     (visual-line-mode)
	     (define-key LaTeX-mode-map (kbd "M-p")
	       '(lambda ()
		  (interactive)
		  (latex-preview-pane-mode)))
	     (define-key LaTeX-mode-map (kbd "<f9>")
	       '(lambda ()
		  (interactive)
		  (TeX-fold-buffer)
		  (preview-document)
		  )
	       )
	     )
	  )
;; Allow the LaTeX-narrow-to-environment command be run without
;; prompting (i.e., `C-x n e`)
(put 'LaTeX-narrow-to-environment 'disabled nil)

;; Keeps the linum column at constant fontsize when doing a zoom of
;; rest of the buffer
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 100))

;; Turn on global auto completion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; ;; Set up SLIME for common add
;; (lisp-hook 'lisp-mode-hook (lambda ()
;; 			    (slime-mode t)))
;; (setq inferior-lisp-program "/usr/local/bin/sbcl"
;;       slime-contribs '(slime-fancy)
;;       lisp-indent-function 'common-lisp-indent-function
;;       slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
;; ; TODO: Have to confirm what exactly slime-fancy does, and whether any
;; ; of the settings above, apart from the inferior-lisp-program one even
;; ; work

;; Set up Dafny integration
(require 'boogie-friends)
(setq dafny-prefix-path "/opt/dafny/")
(setq flycheck-dafny-executable (concat dafny-prefix-path "dafny"))
(setq flycheck-z3-executable (concat dafny-prefix-path "z3/bin/z3"))
(setq flycheck-inferior-dafny-executable (concat
					  dafny-prefix-path "dafny-server"))


;; Set up F* integration
(require 'fstar-mode)
(setq fstar-prefix-path "/home/jay/everest/FStar/")
(setq fstar-executable (concat fstar-prefix-path "bin/fstar.exe"))
(setq fstar-smt-executable (concat fstar-prefix-path "bin/z3"))
(setq fstar-subp-prover-args (lambda () `(
					  "--use_hints" ;; "--record_hints"
					  "--detail_hint_replay"
					  "--include" "/home/jay/everest/kremlin/kremlib"
					  "--__no_positivity" ;; For QUIC (due to pointers and stuff)
					  "--cache_checked_modules"
					  )))
(defun fstar-set-to-release-paths ()
  (interactive)
  (setq fstar-prefix-path "/opt/fstar-release/")
  (setq fstar-executable (concat fstar-prefix-path "bin/fstar.exe"))
  (setq fstar-smt-executable (concat fstar-prefix-path "bin/z3")))
(require 'fstar-indent)
(require 'ocp-indent)
(defun ocp-indent-buffer ()
  (interactive)
  (if ocp-indent-buffer-must-run
      (save-excursion
	(mark-whole-buffer)
	(ocp-indent-region (region-beginning)
			   (region-end))) ()))
(defun toggle-ocp-indent-buffer ()
  (interactive)
  (setq ocp-indent-buffer-must-run
	(not ocp-indent-buffer-must-run))
  (message (if ocp-indent-buffer-must-run "Now on" "Now off")))
(setq ocp-indent-buffer-must-run nil)
(defun killall-z3 ()
  (interactive)
  (call-process "killall" nil nil nil "z3"))
(add-hook 'fstar-mode-hook
	  (lambda ()
	    ; (auto-fill-mode)
	    (ocp-setup-indent)
	    ;; (add-hook 'fstar-newline-hook
	    ;; 	      (lambda (ignored-arg) (ocp-indent-line)) nil t)
	    (add-hook 'before-save-hook 'ocp-indent-buffer nil t)
	    (font-lock-add-keywords nil
				    `((,(regexp-opt
					 '("assume"
					   "admit"
					   "admitP"
					   "magic"
					   "unsafe_coerce") 'symbols) . font-lock-warning-face))
				    ) ; Temporary (might get merged
				      ; into fstar-mode). To display
				      ;  "cheating" keywords.
	    (local-set-key (kbd "C-c C-k") 'killall-z3)
	    (local-set-key (kbd "M-,") 'pop-global-mark) ; works nicely with M-.
	    ))

;; Set up markdown editing
(require 'markdown-mode)
(setq markdown-command "pandoc")

;; Disable audible bell
(setq ring-bell-function 'ignore)

;; Set up gdb to use the many-windows functionality
(setq gdb-many-windows t)

;; Turn on show-trailing-whitespace
(setq-default show-trailing-whitespace t)

;; Be able to unfill paragraphs
(require 'unfill)
(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; Enable abbrev-mode globally
(setq-default abbrev-mode t)
;;; Useful shortcut: C-x a i g
;;; Replaces previous word and sets up an abbrev

;; Allow "C-x n n" to be done without that irritating warning
(put 'narrow-to-region 'disabled nil)

;; Set M-, to pop-tag-mark to work nicely with the M-. of ctags
;; This clobbers tags-loop-continue however
(global-set-key (kbd "M-,") 'pop-tag-mark)

;; Allow creation of ctags info from inside emacs itself :)
(setq path-to-ctags "/usr/bin/ctags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R '%s'" path-to-ctags (file-truename (directory-file-name dir-name)))))

;; Elpy for Python. Requires to have run "pip install jedi flake8
;; autopep8 yapf" on system in advance.
(package-initialize)
(elpy-enable)

;; Smoothen scrolling
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
	      scroll-down-aggressively 0.01)

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
(defun c-guess-and-set-style ()
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
	    (c-guess-and-set-style)))

;; Make sure packages stay updated; but prompt before each update
;; Runs approximately once every 7 days
(require 'auto-package-update)
(setq auto-package-update-prompt-before-update t)

;; Do whitespace cleanup iff the file was opened with clean whitespace
(require 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)

;; Display a nicer startup message :D
(defun display-startup-echo-area-message ()
  (message "Let the hacking begin!"))

;; Make scratch buffer be a fundamental-mode buffer
;; and give a better message (empty :D)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "")
