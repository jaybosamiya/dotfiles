(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Remove annoying UI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(eval-when-compile
  (require 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(coq-double-hit-enable t)
 '(custom-enabled-themes (quote (misterioso)))
 '(delete-selection-mode nil)
 '(display-time-mode t)
 '(flyspell-default-dictionary "english")
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
 '(proof-electric-terminator-enable nil)
 '(send-mail-function (quote mailclient-send-it))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 113 :width normal))))
 '(minimap-active-region-background ((t (:inverse-video t))))
 '(writegood-duplicates-face ((t (:underline (:color "DodgerBlue1" :style wave)))))
 '(writegood-passive-voice-face ((t (:underline "PaleTurquoise4"))))
 '(writegood-weasels-face ((t (:underline (:color "yellow4" :style wave))))))

;; Exec Path from Shell
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (exec-path-from-shell-initialize))

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
(use-package smex
  :ensure t
  :demand t
  :bind (; Replace with smex
	 ("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ; and maintain old M-x via C-c M-x
	 ("C-c M-x" . execute-extended-command)))

;; imenu-anywhere lets you jump between relevant parts of code easily
(use-package imenu-anywhere
  :ensure t
  :bind (("C-." . imenu-anywhere)))

(display-time-mode 1)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-support-shift-select t "Allow selecting with shift")
  (org-hide-emphasis-markers t "Hide emphasis markers (/, _, etc.)")
  (org-export-with-toc nil "Disable table-of-contents generation")
  :config
  (progn
    ;; Allow quotes inside of emphasis sections : Based off
    ;; of https://stackoverflow.com/a/24173780/3696619
    (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n")
    ;; Allow emphasised regions to also end with a "s"
    (setcar (nthcdr 1 org-emphasis-regexp-components)
  	    (concat (nth 1 org-emphasis-regexp-components) "s"))
    ;; Actually update the emphasis regexp
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))
  ;; Enable extra backends
  (setq org-export-backends
	(quote (ascii html icalendar latex md deck)))
  (add-hook 'org-mode-hook #'(lambda ()
			       ;; Wrap lines
			       (visual-line-mode 1)
			       ;; Get better looking org-mode buffer
			       (org-indent-mode 1))))

(use-package which-key
  :ensure t
  :demand t
  :config (which-key-mode))

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
(use-package boogie-friends
  :mode ("\\.dfy\\'" . dafny-mode)
  :config
  (setq dafny-prefix-path "/opt/dafny/")
  (setq flycheck-dafny-executable (concat dafny-prefix-path "dafny"))
  (setq flycheck-z3-executable (concat dafny-prefix-path "z3/bin/z3"))
  (setq flycheck-inferior-dafny-executable (concat
					    dafny-prefix-path "dafny-server")))


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
;; (setq-default abbrev-mode t) ; Disabled
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
(use-package elpy
  :mode ("\\.py\\'" . elpy-mode)
  :config
  (elpy-enable))

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
(global-linum-mode)

;; Handle escape sequence colorization properly for compilation-mode
;; See : https://emacs.stackexchange.com/a/38531
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Fix escape sequence issues in shell, compilation-mode etc.
;; See : https://emacs.stackexchange.com/a/38531
(defun regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))
(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")
(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F"
         )))
(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))
(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))
(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)

;; Use undo-tree everywhere (Use C-x u to visualize the undo-tree)
(require 'undo-tree)
(global-undo-tree-mode)
(add-hook 'undo-tree-visualizer-mode-hook
	  (lambda ()
	    (setq show-trailing-whitespace nil)))

;; Help writing correct text. TODO: Figure out how to make it trigger
;; only in "pure text" buffers
(require 'writegood-mode)
(add-hook 'text-mode-hook
	  (lambda ()
	    (flyspell-mode t)
	    (flyspell-buffer)
	    (writegood-mode t)))

;; Adding a word to the flycheck dictionary causes it to stop showing
;; the rest of the underlined words. The following adds "advice" to
;; run (flycheck-buffer) after saving a word to the dictionary.
(defun flyspell-buffer-after-pdict-save (&rest _)
  (flyspell-buffer))
(advice-add 'ispell-pdict-save :after
	    #'flyspell-buffer-after-pdict-save)

;; Set C-' to correct word using flyspell, and F9 to flyspell the
;; entire buffer. C-F9 to disable flyspell.
(global-set-key (kbd "C-'")
		'flyspell-correct-word-before-point)
(global-set-key (kbd "<f9>")
		(lambda ()
		  (interactive)
		  (if (derived-mode-p 'prog-mode)
		      (flyspell-prog-mode)
		    (flyspell-mode t))
		  (flyspell-buffer)))
(global-set-key (kbd "C-<f9>")
		'(lambda () (interactive) (flyspell-mode -1)))

;; Speed up flyspell by using no messages
(setq-default flyspell-issue-message-flag nil)

;; Opam
;; Add opam emacs directory to the load-path
(setq opam-share "/home/jay/.opam/4.05.0/share")
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Coq
;; Proof General
(load-file "/usr/share/emacs/site-lisp/proofgeneral/generic/proof-site.el")
;; Load company-coq when opening Coq files
(require 'company-coq)
(add-hook 'coq-mode-hook #'company-coq-mode)

;; Agda mode
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; Theme flipper
(setq
 theme-flipper-list '(misterioso solarized-light solarized-dark adwaita)
 theme-flipper-index 0)
(defun theme-flip ()
  (interactive)
  (setq theme-flipper-index (+ 1 theme-flipper-index))
  (when (>= theme-flipper-index (length theme-flipper-list))
    (setq theme-flipper-index 0))
  (let ((this-theme (nth-value theme-flipper-index theme-flipper-list)))
    (load-theme this-theme t t)
    (dolist (theme theme-flipper-list)
      (when (not (eq theme this-theme))
	(disable-theme theme)))
    (enable-theme this-theme)))
(global-set-key (kbd "C-<f12>") 'theme-flip)

;; Microsoft IVy
(use-package ivy-mode)

;; Use a hippie-expand, instead of dabbrev-expand, which has
;; dabbrev-expand as one of its tactics, so leads to a guaranteed
;; superset of expansions
(global-set-key (kbd "M-/") 'hippie-expand)

;; Ensure that copying from another program and then running a kill
;; command in emacs doesn't cause things to disappear from the
;; clipboard
(setq save-interprogram-paste-before-kill t)

;; Make sure the mouse yanking pastes at point instead of at click
(setq mouse-yank-at-point t)

;; Prevent stale elisp bytecode from shadowing more up-to-date source
;; files
(setq load-prefer-newer t)

;; Use ibuffer instead of list-buffer for C-x C-b
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use ido-occur to be able to easily move around the buffer
(use-package ido-occur
  :ensure t
  :bind (("C-o" . 'ido-occur)
	 :map isearch-mode-map
	 ("C-o" . 'ido-occur-from-isearch)))

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

;; Persistent undo history across saves. TODO: Decide if this is a
;; good idea or not.
(use-package undohist
  :ensure t
  :config
  (undohist-initialize))
