(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Remove annoying UI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(eval-when-compile
  (or (require 'use-package nil t)
      (progn
	(package-refresh-contents)
	(package-install 'use-package)
	(message "On a new system. Just installed use-package!"))))

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
 '(package-selected-packages
   (quote
    (rainbow-delimiters delight golden-ratio langtool rainbow-identifiers-mode wc-mode vagrant-tramp undohist solarized-theme restart-emacs powerline php-mode paredit ocp-indent markdown markdown-mode guru-mode elpy dockerfile-mode caml boogie-friends visual-fill-column ido-yes-or-no ag xcscope ido-occur auctex fold-this eclim haskell-mode zygospore iedit ini-mode keyfreq vlf semantic-mode srefactor cl-lib zpresent org-present ox-reveal undo-tree minimap epresent)))
 '(proof-electric-terminator-enable nil)
 '(safe-local-variable-values
   (quote
    ((fstar-subp-prover-additional-args lambda nil
					(require
					 (quote magit))
					(split-string
					 (string-join
					  (cl-remove-if
					   (lambda
					     (s)
					     (string-match-p "^$" s))
					   (mapcar
					    (lambda
					      (s)
					      (replace-regexp-in-string "--include "
									(concat "--include "
										(replace-regexp-in-string "^/ssh.*:/" "/"
													  (file-relative-name
													   (magit-toplevel))))
									s))
					    (mapcar
					     (lambda
					       (s)
					       (replace-regexp-in-string "[[:space:]]*#.*$" "" s))
					     (split-string
					      (with-temp-buffer
						(insert-file-contents
						 (concat
						  (magit-toplevel)
						  "fstar-args"))
						(buffer-substring-no-properties
						 (point-min)
						 (point-max)))
					      "
" t))))
					  " ")
					 " " t)))))
 '(send-mail-function (quote mailclient-send-it))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 113 :width normal))))
 '(ggtags-highlight ((t nil)))
 '(minimap-active-region-background ((t (:inverse-video t))))
 '(writegood-duplicates-face ((t (:underline (:color "DodgerBlue1" :style wave)))))
 '(writegood-passive-voice-face ((t (:underline "PaleTurquoise4"))))
 '(writegood-weasels-face ((t (:underline (:color "yellow4" :style wave))))))

;; Make minibuffer history persist across sessions
(savehist-mode 1)

;; Be able to easily edit the minor mode stuff that shows up in the modeline
(use-package delight
  :ensure t
  :demand t)

;; Exec Path from Shell
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (exec-path-from-shell-initialize))

;; Be able to restart emacs from within emacs
(use-package restart-emacs
  :ensure t)

;; ;; Start off emacs server, so that `emacsclient` can be used
;; (load "server")
;; (if (server-running-p)
;;     (message "%s" "Server already started by someone else")
;;   (server-start))

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
(use-package olivetti
  :ensure t
  :bind ("C-<f11>" . olivetti-mode)
  :config
  (progn
    (setq olivetti-hide-mode-line t)
    (setq-default olivetti-body-width 90)))

;; fold-this
(use-package fold-this
  :ensure t
  :demand t
  :bind (("C-c C-f" . fold-this-all)
	 ("C-c C-S-f" . fold-this)
	 ("C-c M-f" . fold-this-unfold-at-point)
	 ("C-c M-F" . fold-this-unfold-all)))

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
  :ensure t
  :defer t
  :magic ("%PDF" . pdf-view-mode))
(pdf-tools-install)

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
  (org-startup-with-inline-images t "Show images inline upon startup")
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
  ;; Allow snippets (eg: "< s TAB" for source,
  ;;                  or "< q TAB" for quote,
  ;;                  or "< l TAB" for LaTeX etc)
  (require 'org-tempo)
  ;; Enable extra backends
  (setq org-export-backends
	(quote (ascii html icalendar latex md deck reveal)))
  (add-hook 'org-mode-hook #'(lambda ()
			       ;; Wrap lines
			       (visual-line-mode 1)
			       ;; Get better looking org-mode buffer
			       (org-indent-mode 1))))

(use-package ox-reveal
  :ensure t
  :defer t
  :config
  (progn
    (setq org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.0.0/")
    (setq org-reveal-title-slide "<h1 class=\"title\">%t</h1><h5 class=\"author\">%a</h5><h5 class=\"date\">%d</h5>")))

(use-package htmlize
  :ensure t)

(use-package which-key
  :ensure t
  :demand t
  :delight
  :config (which-key-mode))

;; Prevent C-z from accidentally sending the window to background
(global-unset-key (kbd "C-z"))

;; Handle "Page Up" and "Page Down" better
(global-set-key (kbd "<next>") 'scroll-up-line)
(global-set-key (kbd "<prior>") 'scroll-down-line)
(global-unset-key (kbd "C-<prior>"))
(global-unset-key (kbd "C-<next>"))

(require 'latex-preview-pane)
(use-package tex
  :ensure auctex
  :demand t)
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
(use-package company
  :ensure t
  :delight
  :hook (after-init . global-company-mode)
  :bind (("C-<tab>" . company-complete)))

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
(setq fstar-executable "fstar.exe")
(setq fstar-smt-executable "z3")
(setq fstar-subp-prover-args (lambda () `(
					  "--use_hints" ;; "--record_hints"
					  ;; "--detail_hint_replay"
					  ;; "--include" "/home/jay/everest/kremlin/kremlib"
					  "--cache_checked_modules"
					  )))
(defun fstar-set-to-release-paths ()
  (interactive)
  (setq fstar-prefix-path "/opt/fstar-release/")
  (setq fstar-executable (concat fstar-prefix-path "bin/fstar.exe"))
  (setq fstar-smt-executable (concat fstar-prefix-path "bin/z3")))
(require 'fstar-rewrite)
(add-to-list 'load-path "/home/jay/.local/share/emacs/site-lisp")
(require 'fstar-indent)
(defun fstar-indent-buffer ()
  (interactive)
  (if fstar-indent-buffer-must-run
      (save-excursion
	(mark-whole-buffer)
	(fstar-indent-region (region-beginning)
			   (region-end))) ()))
(defun toggle-fstar-indent-buffer ()
  (interactive)
  (setq fstar-indent-buffer-must-run
	(not fstar-indent-buffer-must-run))
  (message (if fstar-indent-buffer-must-run "Now on" "Now off")))
(setq fstar-indent-buffer-must-run nil)
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
	    (superword-mode 1)
	    ; (auto-fill-mode)
	    (ocp-setup-indent)
	    ;; (add-hook 'fstar-newline-hook
	    ;; 	      (lambda (ignored-arg) (fstar-indent-line)) nil t)
	    (add-hook 'before-save-hook 'ocp-indent-buffer nil t)
	    (local-set-key (kbd "C-c C-k") 'killall-z3)
	    (local-set-key (kbd "C-'") 'fstar-jump-to-definition-other-window)
	    (local-set-key (kbd "M-'") 'fstar-jump-to-related-error-other-window)
	    (local-set-key (kbd "M-,") 'xref-pop-marker-stack) ; works nicely with M-.
	    (local-set-key (kbd "<f12>") 'flycheck-clear)
	    ))

;; Set up markdown editing
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-command "pandoc"))

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

;; To use gtags, must have run `apt install global exuberant-ctags`
;; first
(use-package ggtags
  :ensure t
  :hook ((c-mode c++-mode java-mode) . ggtags-mode)
  :custom-face (ggtags-highlight ((t nil))))

;; Elpy for Python. Requires to have run "pip install jedi flake8
;; autopep8 yapf" on system in advance.
(use-package elpy
  :ensure t
  :hook (python-mode . elpy-mode)
  :config
  (elpy-enable))

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

;; Make sure packages stay updated; but prompt before each update
;; Runs approximately once every 7 days
(require 'auto-package-update)
(setq auto-package-update-prompt-before-update t)

;; Do whitespace cleanup iff the file was opened with clean whitespace
(use-package whitespace-cleanup-mode
  :ensure t
  :delight
  :config (global-whitespace-cleanup-mode t))

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

;; Disable linum-mode for incompatible cases
(dolist (hook '(pdf-view-mode-hook olivetti-mode-hook))
  (add-hook hook '(lambda () (linum-mode 0))))

;; Handle escape sequence colorization properly for compilation-mode
;; See : https://emacs.stackexchange.com/a/38531
;; Disabled right now because it seems to cause bad interactions with
;; the thing below, especially in the ag buffer
;; (require 'ansi-color)
;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region (point-min) (point-max))
;;   (toggle-read-only))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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
;; (require 'undo-tree)
;; (global-undo-tree-mode)
;; (add-hook 'undo-tree-visualizer-mode-hook
;; 	  (lambda ()
;; 	    (setq show-trailing-whitespace nil)))

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

;; Speed up flyspell by using no messages
(setq-default flyspell-issue-message-flag nil)

;; Opam, OCaml
(require 'ocaml)

;; Coq
;; Proof General
(load-file "/usr/share/emacs/site-lisp/proofgeneral/generic/proof-site.el")
;; Load company-coq when opening Coq files
(use-package company-coq
  :hook (coq-mode . company-coq-mode))

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

;; ;; Use ido-occur to be able to easily move around the buffer
;; (use-package ido-occur
;;   :ensure t
;;   :bind (("C-o" . 'ido-occur)
;; 	 :map isearch-mode-map
;; 	 ("C-o" . 'ido-occur-from-isearch)))

;; Bind "occur" to C-o instead of open-line
(global-set-key (kbd "C-o") 'occur)

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

;; Be able to move between buffers more easily, using M-up, M-down,
;; M-left, M-right.
(windmove-default-keybindings 'meta)

;; Be able to use ag from emacs
(use-package ag
  :ensure t
  :bind (("M-s M-s" . 'ag)
	 ("M-s s"   . 'ag)))

;; Use IDO for yes-or-no-p and y-or-n-p
(use-package ido-yes-or-no
  :ensure t
  :config (ido-yes-or-no-mode))

;; Temporary workaround for the lisp-mode-symbol-regexp "bug" for
;; magit. Either have to fix it and remove this, or maybe move to
;; emacs 25?
(defconst lisp-mode-symbol-regexp "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")

;; Make large files less painful to use
(use-package vlf
  :ensure t
  :config
  (progn
    (require 'vlf-setup)
    (add-hook 'vlf-mode-hook
	      #'(lambda ()
		  (require 'vlf-follow)
		  (vlf-start-follow 0.01)))))

;; Make tramp uses the entire path it gets from the remote
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; Be able to easily jump over to my servers
(defun open-home-on-server ()
  "Opens up the home directory on a server"
  (interactive)
  (defun my-write (file data)
    (with-temp-file file
      (prin1 data (current-buffer))))
  (defun my-read (file)
    (if (file-exists-p file)
	(with-temp-buffer
	  (insert-file-contents file)
	  (goto-char (point-min))
	  (read (current-buffer)))
      nil))
  (let* ((history-path "~/.emacs.d/servers_accessed.txt")
	 (serv-history-list (my-read history-path))
	 (serv-name (ido-completing-read "Server: " serv-history-list)))
    (my-write history-path (cons serv-name (remove serv-name serv-history-list)))
    (dired (concat "/sshx:" serv-name ":~"))))
(global-set-key (kbd "<f6>") 'open-home-on-server)

;; Enable ".dir-locals.el" usage even via tramp
(setq enable-remote-dir-locals t)

;; Keep track of key frequencies. Can use `keyfreq-show` to see how
;; many times I've used each command
;; (use-package keyfreq
;;   :ensure t
;;   :init
;;   (progn
;;     (keyfreq-mode 1)
;;     (keyfreq-autosave-mode 1)))

;; Allow multi line editing.
;; Use using C-; when over a symbol
(use-package iedit
  :ensure t)

;; Make C-x 1 (delete-other-windows) reversible
(use-package zygospore
  :ensure t
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

;; Java Support via Eclim (requires Eclipse)
(use-package eclim
  :config
  (progn
    ;; Autostart Eclim
    (setq eclimd-autostart t)
    (setq eclimd-default-workspace "~/tmp/eclim-ws/")
    (setq eclimd-autostart-with-default-workspace t))
  :hook (java-mode . eclim-mode))
(use-package company-emacs-eclim
  :after (eclim company)
  :config
  (progn
    (company-emacs-eclim-setup)
    (setq company-emacs-eclim-ignore-case t)))

;; Use visual-fill-column to wrap buffers at a specific width instead
;; of full width, as visual-line-mode does
(use-package visual-fill-column
  :ensure t)

;; Set up "C-x v" to switch into and out of visual line wrapping
(setq-default visual---state nil)
(make-variable-buffer-local 'visual---state)
(defun toggle-visual ()
  (interactive)
  (setq visual---state (not visual---state))
  (let ((x (if visual---state 1 -1)))
    (visual-fill-column-mode x)
    (visual-line-mode x)))
(global-set-key (kbd "C-x v") 'toggle-visual)

;; Make sure to get markdown-mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

;; Bring in rainbow-identifiers-mode; super useful when trying to
;; deobfuscate or working with complex code with too many similar
;; variable names.
(use-package rainbow-identifiers
  :ensure t
  :delight " rainbow-ident")

;; Bring in rainbow-delimiters-mode; super useful when some parens are
;; very nested
(use-package rainbow-delimiters
  :ensure t
  :delight " rainbow-delim")

;; Use dharma-mode for .dg files
;; See dharma-mode defined in f0xtr0t/dharma-mode.el
(use-package dharma-mode
  :mode ("\\.dg\\'" . dharma-mode))

;; Language Tool support to be able to perform grammar checks.
;; Useful commands:
;;   M-x langtool-check
;;   M-x langtool-check-done
;; Requires https://www.languagetool.org/ to be installed into the
;; right location
(use-package langtool
  :config
  (setq langtool-language-tool-jar
	"/home/jay/.local/share/languagetool/languagetool-commandline.jar")
  (setq langtool-default-language "en-US"))

;; Allow mini-buffer to get quite large. Super useful for F*'s
;; evaluation to see what the hell really went on. Might decide to
;; remove this or restrict to F* only if it gets too annoying.
(setq max-mini-window-height 0.5)

;; Use golden-ratio-mode to help keep the current window in better
;; focus by making it a bit larger.
(use-package golden-ratio
  :ensure t
  :delight
  :config
  (setq golden-ratio-auto-scale t)
  (golden-ratio-mode 1))

;; Prevent magit transient window from popping up so damn fast!
(setq transient-show-popup 0.5)
