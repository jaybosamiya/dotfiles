(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(when (< emacs-major-version 27)
  (package-initialize))

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
 '(coq-double-hit-enable t)
 '(custom-enabled-themes '(misterioso))
 '(delete-selection-mode nil)
 '(display-time-mode t)
 '(flyspell-default-dictionary "english")
 '(font-use-system-font t)
 '(global-auto-revert-mode t)
 '(ido-default-buffer-method 'selected-window)
 '(ido-enable-flex-matching t)
 '(ido-ubiquitous-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(magit-diff-refine-hunk t)
 '(menu-bar-mode nil)
 '(minimap-dedicated-window nil)
 '(minimap-hide-fringes t)
 '(minimap-highlight-line nil)
 '(minimap-sync-overlay-properties '(face invisible))
 '(minimap-update-delay 0.0)
 '(minimap-width-fraction 0.1)
 '(minimap-window-location 'right)
 '(package-selected-packages
   '(company-lean lean-mode expand-region company-tabnine comment-or-uncomment-sexp neuron-mode elmacro org-fragtog rg amx org-noter company-lsp flymake-shellcheck flycheck-popup-tip flycheck-pos-tip flycheck-rust racer cargo rust-mode graphviz-dot-mode urlenc bug-hunter fill-column-indicator sane-term default-text-scale dumb-jump lsp-ui lsp-mode suggest projectile doom-modeline vale-mode buffer-move magit-todos sublimity framemove ox-gfm zoom rainbow-identifiers flycheck-package package-lint rainbow-delimiters delight golden-ratio langtool rainbow-identifiers-mode wc-mode vagrant-tramp undohist solarized-theme restart-emacs powerline php-mode paredit ocp-indent markdown markdown-mode guru-mode elpy dockerfile-mode caml boogie-friends visual-fill-column ido-yes-or-no ag xcscope ido-occur auctex fold-this eclim haskell-mode zygospore iedit ini-mode keyfreq vlf semantic-mode srefactor cl-lib zpresent org-present ox-reveal undo-tree minimap epresent))
 '(proof-electric-terminator-enable nil)
 '(safe-local-variable-values
   '((fstar-subp-prover-additional-args lambda nil
                                        (require 'magit)
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
                                         " " t))))
 '(send-mail-function 'mailclient-send-it)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 113 :width normal))))
 '(doom-modeline-info ((t (:inherit bold))))
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
(global-set-key (kbd "C-x C-d") #'ido-dired) ;; Map "C-x C-d" to do same as "C-x d" which is otherwise awkward.

;; Get some distraction free goodness :)
(use-package olivetti
  :ensure t
  :bind ("C-<f11>" . olivetti-mode)
  :config
  (progn
    (setq olivetti-hide-mode-line t)
    (setq-default olivetti-body-width 116)))

;; fold-this
(use-package fold-this
  :ensure t
  :demand t
  :bind (("C-c C-f" . fold-this-all)
	 ("C-c C-S-f" . fold-this)
	 ("C-c M-f" . fold-this-unfold-at-point)
	 ("C-c M-F" . fold-this-unfold-all)))

;; amx -- newer fork of smex which stopped development in 2015
(use-package amx
  :ensure t
  :demand t
  :bind (; Replace with amx
	 ("M-x" . amx)
	 ("M-X" . amx-major-mode-commands)
	 ; and maintain old M-x via C-c M-x
	 ("C-c M-x" . execute-extended-command)))

;; imenu-anywhere lets you jump between relevant parts of code easily
;; (use-package imenu-anywhere
;;   :ensure t
;;   :bind (("C-." . imenu-anywhere)))

(display-time-mode 1)

(use-package pdf-tools
  :ensure t
  :defer t
  :config (pdf-tools-install)
  :magic ("%PDF" . pdf-view-mode))

;; Easy kill-switch for magit's wip mode stuff. Generally it is fine
;; to keep on, but after having used it a while, it doesn't seem to be
;; terribly useful to me and does incur a tiny bit of a slowdown, so
;; instead I am going to keep it off by default from this point
;; forward. The reason it is as a config option is so that I can
;; re-enable it at any point easily :)
(setq f0xtr0t-magit-wip-mode-enabled nil)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  ;; Make the `q` key in magit not revert to the window configuration
  ;; to whatever it was before the buffer was open, making one lose
  ;; progress. Instead, now it will just kill itself, leaving
  ;; everything else as-is.
  (setq magit-bury-buffer-function #'magit-mode-quit-window)
  ;; Disable Emacs's normal VCS stuff, since I'm never using it, and
  ;; am using only Magit instead.
  (setq vc-handled-backends nil)
  ;; Set up stuff for magit wip -- See magit-wip below.
  (when f0xtr0t-magit-wip-mode-enabled
    (setq magit-wip-merge-branch t)
    (transient-append-suffix 'magit-log "a"
      '("i" "Index wipref" magit-wip-log-index))
    (transient-append-suffix 'magit-log "i"
      '("w" "Worktree wipref" magit-wip-log-worktree))))

;; Temporary workaround for the lisp-mode-symbol-regexp "bug" for
;; magit. Either have to fix it and remove this, or maybe move to
;; emacs 25?
(defconst lisp-mode-symbol-regexp "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")

;; Prevent magit transient window from popping up so damn fast!
(setq transient-show-popup 0.5)

;; Make magit smarter at keeping progress of changes along the way.
;; See https://emacs.stackexchange.com/a/45153
(when f0xtr0t-magit-wip-mode-enabled
  (use-package magit-wip
    :after magit
    :config
    (magit-wip-before-change-mode)
    (magit-wip-after-apply-mode)
    (magit-wip-after-save-mode))
  (add-hook 'before-save-hook 'magit-wip-commit-initial-backup))

;; (use-package magit-todos
;;   :ensure t
;;   :custom
;;   (magit-todos-keyword-suffix "" "No suffix needed")
;;   (magit-todos-keywords (quote ("TODO" "XXX" "WARN" "UNSOUND" "admit" "assume")) "Show todos for all of these")
;;   :config
;;   (magit-todos-mode 1))

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-support-shift-select t "Allow selecting with shift")
  (org-hide-emphasis-markers t "Hide emphasis markers (/, _, etc.)")
  (org-export-with-toc nil "Disable table-of-contents generation")
  (org-startup-with-inline-images t "Show images inline upon startup")
  (org-startup-with-latex-preview t
   ; Use C-c C-x C-l to reload previews
   t "Show LaTeX fragment previews upon startup")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (progn
    ;; Allow windmove to continue working
    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right)
    (add-hook 'org-metaup-final-hook 'windmove-up)
    (add-hook 'org-metaleft-final-hook 'windmove-left)
    (add-hook 'org-metadown-final-hook 'windmove-down)
    (add-hook 'org-metaright-final-hook 'windmove-right)
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
    (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.8.0/")
    (setq org-reveal-title-slide "<h1 class=\"title\">%t</h1><h5 class=\"author\">%a</h5><h5 class=\"date\">%d</h5>")))

;; Makes org-mode latex fragment previews nice. Automatically
;; hides/unhides them as you go over them.
(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(use-package htmlize
  :ensure t)

(use-package which-key
  :ensure t
  :demand t
  :delight
  :config (which-key-mode))

;; Prevent C-z from accidentally sending the window to background
(global-unset-key (kbd "C-z"))

;; Prevent F11 from accidentally trying to maximize the window
(global-unset-key (kbd "<f11>"))

;; Handle "Page Up" and "Page Down" better
(global-set-key (kbd "<next>") 'scroll-up-line)
(global-set-key (kbd "<prior>") 'scroll-down-line)
(global-unset-key (kbd "C-<prior>"))
(global-unset-key (kbd "C-<next>"))

;; (require 'latex-preview-pane)
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
             (highlight-regexp "\\\\comment{[^}]*}" 'superscript)
             (highlight-regexp "\\\\comment" 'hi-blue)
             (highlight-regexp "\\\\jay{[^}]*}" 'superscript)
             (highlight-regexp "\\\\jay" 'hi-blue)
             (highlight-regexp "\\\\todo{[^}]*}" 'superscript)
             (highlight-regexp "\\\\todo" 'hi-blue)
	     )
	  )
;; Allow the LaTeX-narrow-to-environment command be run without
;; prompting (i.e., `C-x n e`)
(put 'LaTeX-narrow-to-environment 'disabled nil)

;; Keeps the linum column at constant fontsize when doing a zoom of
;; rest of the buffer
;; (eval-after-load "linum"
;;   '(set-face-attribute 'linum nil :height 100))

;; Fix linum for scaled text
(eval-after-load "linum"
  #'(progn
      (defun linum-update-window-scale-fix (win)
        "fix linum for scaled text"
        (set-window-margins win
                            (ceiling (* (if (boundp 'text-scale-mode-step)
                                            (expt text-scale-mode-step
                                                  text-scale-mode-amount) 1)
                                        (if (car (window-margins))
                                            (car (window-margins)) 1)
                                        ))))
      (advice-add #'linum-update-window :after #'linum-update-window-scale-fix)))

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

(use-package imenu-anywhere
  :ensure t)

(defun push-marker-stack-and-imenu-anywhere ()
  (interactive)
  (xref-push-marker-stack)
  (imenu-anywhere))

;; Set up Dafny integration
(use-package boogie-friends
  :mode ("\\.dfy\\'" . dafny-mode)
  :init
  (add-hook 'dafny-mode-hook
            (lambda ()
              (setq imenu-generic-expression
                    '((nil "\\_<datatype\\_> +\\(\\_<.*?\\_>\\)\\( \\|$\\)" 1)
                      (nil "\\_<type\\_> +\\(\\_<.*?\\_>\\)\\( \\|$\\)" 1)
                      (nil "\\_<class\\_> +\\(\\_<.*?\\_>\\)\\( \\|$\\)" 1)
                      (nil "\\_<function method\\_> *?\\({:.*?}\\)? +\\(\\_<.*?\\_>\\) ?(" 2)
                      (nil "\\_<function\\_> *?\\({:.*?}\\)? +\\(\\_<.*?\\_>\\) ?(" 2)
                      (nil "\\_<method\\_> *?\\({:.*?}\\)? +\\(\\_<.*?\\_>\\) ?(" 2)
                      (nil "\\_<predicate\\_> *?\\({:.*?}\\)? +\\(\\_<.*?\\_>\\) ?(" 2)
                      (nil "\\_<lemma\\_> *?\\({:.*?}\\)? +\\(\\_<.*?\\_>\\) ?(" 2)))
              (local-set-key (kbd "M-.") 'push-marker-stack-and-imenu-anywhere)
              (electric-indent-mode -1)))
  :config
  (setq dafny-prefix-path "/opt/dafny/Binaries/")
  (setq flycheck-dafny-executable (concat dafny-prefix-path "dafny"))
  (setq flycheck-z3-executable (concat dafny-prefix-path "z3/bin/z3"))
  (setq dafny-prover-background-args '("/timeLimit:30" "/autoTriggers:1" "/printTooltips" "/allocated:3"))
  (setq flycheck-inferior-dafny-executable (concat
					    dafny-prefix-path "dafny-server"))
  (progn ;; Make it easy to jump to errors from compilation mode :)
    (push 'dafny compilation-error-regexp-alist)
    (push '(dafny "^\\([^ ]*?\\)(\\([0-9]+\\),\\([0-9]+\\)): " 1 2 3) compilation-error-regexp-alist-alist)))

;; Set up F* integration
(use-package fstar-mode
  :mode (("\\.fst\\'" . fstar-mode)
         ("\\.fsti\\'" . fstar-mode))
  :config
  (setq fstar-executable "fstar.exe")
  (setq fstar-smt-executable "z3")
  (setq fstar-subp-prover-args
        (lambda ()
          `(
	    "--use_hints" ;; "--record_hints"
	    ;; "--detail_hint_replay"
	    ;; "--include" "/home/jay/everest/kremlin/kremlib"
	    "--cache_checked_modules"
            ,@(when (string-match-p (regexp-opt '("hacl-star/vale" "Vale.")) (buffer-file-name))
                '(
                  "--trivial_pre_for_unannotated_effectful_fns" "false"
                  "--warn_error" "+241@247-272"
                  "--z3cliopt" "smt.arith.nl=false"
                  "--z3cliopt" "smt.QI.EAGER_THRESHOLD=100"
                  "--z3cliopt" "smt.CASE_SPLIT=3"
                  "--use_extracted_interfaces" "true"
                  "--max_fuel" "1"
                  "--max_ifuel" "1"
                  "--initial_ifuel" "0"
                  "--smtencoding.elim_box" "true"
                  "--smtencoding.l_arith_repr" "native"
		  "--smtencoding.nl_arith_repr" "wrapped")))))
  (defun my-fstar-compute-prover-args-using-make ()
    "Construct arguments to pass to F* by calling make."
    (with-demoted-errors "Error when constructing arg string: %S"
      (let* ((fname (file-name-nondirectory buffer-file-name))
	     (target (concat fname "-in"))
	     (argstr (shell-command-to-string (concat "make --quiet " target " 2>/dev/null"))))
        (split-string argstr))))
  (setq fstar-subp-prover-additional-args #'my-fstar-compute-prover-args-using-make)
  (defun fstar-set-to-release-paths ()
    (interactive)
    (setq fstar-prefix-path "/opt/fstar-release/")
    (setq fstar-executable (concat fstar-prefix-path "bin/fstar.exe"))
    (setq fstar-smt-executable (concat fstar-prefix-path "bin/z3")))
  (require 'fstar-rewrite)
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
  (defun fstar-show-admits-and-assumes (&optional prefix)
    (interactive "P")
    (if prefix
        (occur "admit\\|assume\\|TODO\\|WARN\\|FIXME\\|XXX\\|UNSOUND\\|REVIEW\\|WAT\\|NEVERCO[M]MIT")
      (occur "admit\\|assume")))
  (defun fstar-confirm-before-kill (&optional arg)
    (interactive "P")
    (when (y-or-n-p "Are you sure you want to kill the F* process? ")
      (fstar-subp-kill-one-or-many arg)))
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
	      (local-set-key (kbd "<f5>") 'fstar-show-admits-and-assumes)
              (local-set-key (kbd "C-c C-x") 'fstar-confirm-before-kill)
	      ))
  ) ;; end of (use-package fstar-mode)


(add-to-list 'load-path "/home/jay/.local/share/emacs/site-lisp")

;; Make F-12 clear all flycheck marks
(global-set-key (kbd "<f12>") 'flycheck-clear)

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

;; Disable on some modes
(dolist (hook '(term-mode-hook))
  (add-hook hook '(lambda () (setq show-trailing-whitespace nil))))


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
  :custom-face (ggtags-highlight ((t nil)))
  :init (setq ggtags-enable-navigation-keys nil))

;; ------ CURRENTLY DISABLED ------ Using lsp-mode instead.
;; ;; Elpy for Python. Requires to have run "pip install jedi flake8
;; ;; autopep8 yapf" on system in advance.
;; (use-package elpy
;;   :ensure t
;;   :hook (python-mode . elpy-mode)
;;   :config
;;   (elpy-enable))

;; Use lsp-mode.
;;
;; For Python: Requires to have run the following in advance:
;;      `pip install 'python-language-server[all]'`
(use-package lsp-mode
  :ensure t
  :commands lsp
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  ;; Use flake8 instead of pycodestyle+pyflakes+...
  (setq lsp-pyls-plugins-pyflakes-enabled nil
        lsp-pyls-plugins-pylint-enabled nil
        lsp-pyls-plugins-mccabe-enabled nil
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-flake8-enabled t)
  ;; Disable the annoying symbol highlighting behavior
  (setq lsp-enable-symbol-highlighting nil))

;; Nice UI features :)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after (lsp-mode)
  :hook (lsp-mode-hook . lsp-ui-mode)
  :config
  ;; Make sure lsp prefers flycheck over flymake
  (setq lsp-prefer-flymake nil))

;; Connect things up to company via lsp.
(use-package company-lsp
  :ensure t
  :commands company-lsp
  ;; :config (push 'company-lsp company-backends)
  )

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

;; Handle escape sequence colorization properly for compilation-mode
;; See : https://emacs.stackexchange.com/a/38531
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))
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
;; (global-set-key (kbd "C-'")
;; 		'flyspell-correct-word-before-point)
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
(use-package ocaml
  :mode ("\\.ml\\'" . merlin-mode))

;; Coq
;; Proof General
;; (load-file "/usr/share/emacs/site-lisp/proofgeneral/generic/proof-site.el")
;; ;; Load company-coq when opening Coq files
;; (use-package company-coq
;;   :hook (coq-mode . company-coq-mode))

;; Agda mode
;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

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
;; (use-package ivy-mode)

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

;; Bind "occur" to M-o instead of facemenu stuff
(global-set-key (kbd "M-o") 'occur)

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
(require 'framemove)
(windmove-default-keybindings 'meta)
(setq framemove-hook-into-windmove t)

;; Be able to use ag from emacs
(use-package ag
  :ensure t)

;; Be able to use rg from emacs
(use-package rg
  :ensure t
  :config
  (setq rg-executable "rg") ;; Use rg from the $PATH; allows
                            ;; working via TRAMP too!
  (setq rg-default-alias-fallback "everything")
  :bind (("M-s M-s" . 'rg-dwim)
	 ("M-s s"   . 'rg-menu)))

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
(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

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

;; ;; Use golden-ratio-mode to help keep the current window in better
;; ;; focus by making it a bit larger.
;; (use-package golden-ratio
;;   :ensure t
;;   :delight
;;   :config
;;   (setq golden-ratio-auto-scale t)
;;   (golden-ratio-mode 1))

;; Use zoom-mode which is a more modern version of golden-ratio-mode
;; (use-package zoom
;;   :ensure t
;;   :delight
;;   :config
;;   (zoom-mode 1))

;; Prevent magit transient window from popping up so damn fast!
(setq transient-show-popup 0.5)

;; ;; Be able to open nautilus with some nice keybindings
;; (defun open-nautilus-in-directory (dir)
;;   (interactive "D")
;;   (let ((dir (expand-file-name dir)))
;;     (start-process "nautilus" nil "nautilus" dir)))
;; (global-set-key (kbd "C-x C-d") 'open-nautilus-in-directory)

;; Be able to open gnome-terminal with some nice keybindings
(setenv "SHELL" "/usr/bin/zsh")
(defun open-gnome-terminal-in-directory (dir)
  (interactive "D")
  (let ((dir (expand-file-name dir)))
    (start-process "gnome-terminal" nil "dbus-launch" "gnome-terminal" dir)))
(global-set-key (kbd "C-x C-t") 'open-gnome-terminal-in-directory)

;; enable recent files mode.
;; (recentf-mode t)

;; ; 50 files ought to be enough.
;; (setq recentf-max-saved-items 50)

;; (defun ido-recentf-open ()
;;   "Use `ido-completing-read' to \\[find-file] a recent file"
;;   (interactive)
;;   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;;       (message "Opening file...")
;;     (message "Aborting")))

;; ;; get rid of `find-file-read-only' and replace it with something
;; ;; more useful.
;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)


;;;;;;;; Cursor color

;; (set-cursor-color "#ffffff")

;; (defvar blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65")
;;   "On each blink the cursor will cycle to the next color in this list.")

;; (setq blink-cursor-count 0)
;; (defun blink-cursor-timer-function ()
;;   "Zarza wrote this cyberpunk variant of timer `blink-cursor-timer'. 
;; Warning: overwrites original version in `frame.el'.

;; This one changes the cursor color on each blink. Define colors in `blink-cursor-colors'."
;;   (when (not (internal-show-cursor-p))
;;     (when (>= blink-cursor-count (length blink-cursor-colors))
;;       (setq blink-cursor-count 0))
;;     (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
;;     (setq blink-cursor-count (+ 1 blink-cursor-count))
;;     )
;;   (internal-show-cursor nil (not (internal-show-cursor-p)))
;;   )

(add-to-list 'default-frame-alist '(cursor-color . "#ffffff"))

(global-set-key (kbd "C-x 4 c") 'clone-indirect-buffer)

(use-package vale-mode
  :ensure t
  :custom
  (vale-interact-path "/home/jay/everest/vale/tools/scripts/interact.py" "Path to vale's interact.py")
  :mode ("\\.vaf\\'" . vale-mode))

;; (desktop-save-mode 1)
;; (desktop-auto-save-enable)
;; (setq desktop-restore-eager 3)
;; (push '(mouse-color . :never) frameset-filter-alist)
;; (midnight-mode 1) ;; Enable midnight mode to automatically purge old unvisited buffers at midnight.

(use-package buffer-move
  :ensure t
  :bind
  ("<M-S-up>" . buf-move-up)
  ("<M-S-down>" . buf-move-down)
  ("<M-S-left>" . buf-move-left)
  ("<M-S-right>" . buf-move-right))

(global-set-key (kbd "C-M-<f7>") 'normal-mode)

(defun narrow-region-to-indirect-readonly-buffer (start end)
  "Narrow to selected region in an indirect readonly buffer"
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer
              (concat (buffer-name) "-" (int-to-string start) "-" (int-to-string end))
              nil
              t)))
    (switch-to-buffer buf nil t)
    (with-current-buffer buf
      (setq-local buffer-read-only t)
      (set-window-dedicated-p nil t)
      (narrow-to-region start end)
      (use-local-map (copy-keymap (car (current-active-maps))))
      (local-set-key (kbd "q") 'kill-this-buffer))))
(global-set-key (kbd "C-<f7>") 'narrow-region-to-indirect-readonly-buffer)

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

;; Make emacs reload TAGS files automatically
(setq tags-revert-without-query 1)

;; ;; Bring in F# mode
;; (use-package fsharp-mode)

;; Use projectile for easily moving around in projects
(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind
  ("C-o" . projectile-multi-occur)) ;; Replaces open-line

;; Use the "suggest" package to easily find lisp functions via
;; input-output pairs.
(use-package suggest
  :ensure t
  :commands (suggest))

;; Make case-changing much better to work with
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

;; ;; Use language agnostic dumb-jump as a way to jump around for
;; ;; languages that don't have pre-built jumping.
;; (use-package dumb-jump
;;   :ensure t
;;   :bind (("M-g ." . dumb-jump-go)
;;          ("M-g ," . dumb-jump-back)))

;; Introduce C-M-= and C-M-- for changing the font size all across emacs.
(use-package default-text-scale
  :ensure t
  :demand t
  :config (default-text-scale-mode))

(use-package sane-term
  :ensure t)

;; (use-package fill-column-indicator
;;   :ensure t
;;   :config
;;   (setq fci-rule-column 80
;;         fci-rule-use-dashes nil
;;         fci-rule-width 1
;;         fci-rule-color "#3b4b4b")
;;   (define-globalized-minor-mode global-fci-mode fci-mode
;;     (lambda ()
;;       (if (and
;;            (not (string-match "^\*.*\*$" (buffer-name)))
;;            (not (member major-mode
;;                         '(dired-mode magit-diff-mode magit-status-mode))))
;;           (fci-mode 1))))
;;   (global-fci-mode 1))

(use-package bug-hunter
  :ensure t)

(use-package smerge-mode
  :init
  (setq smerge-command-prefix (kbd "C-c v")))

(use-package graphviz-dot-mode
  :ensure t)

(use-package urlenc
  :ensure t)

;; Rust configuration
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t
        rust-format-show-buffer nil)
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              ;; Prevent rust from hijacking the nice fold-this mode
              (define-key rust-mode-map (kbd "C-c C-f") nil))))
(use-package cargo
  :ensure t
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))
(use-package racer
  :ensure t
  :after rust-mode
  :hook (rust-mode . racer-mode)
  :config
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  ;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t)
  :bind ("C-'" . racer-find-definition-other-window))
(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup)
  :hook (rust-mode . flycheck-mode))
(use-package flycheck-pos-tip
  :ensure t
  ;; :hook (rust-mode . flycheck-pos-tip-mode)
)
(use-package flycheck-popup-tip
  :ensure t
  :hook (rust-mode . flycheck-popup-tip-mode)
)

(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flycheck-mode))

;; Use `M-x org-noter` inside a PDF document to be able to write up
;; notes for it in a really nice way
(use-package org-noter
  :ensure t
  :config
  (add-hook 'pdf-view-mode-hook
            (lambda () (local-set-key (kbd "i")
                                      #'org-noter)))
  (setq org-noter-always-create-frame nil))

;; elmacro is useful to convert a keyboard macro into elisp.
;; Use `M-x elmacro-show-last-macro`
(use-package elmacro
  :ensure t)

;; TODO: Look into el-patch (https://github.com/raxod502/el-patch)

;; Add neuron-mode for zetterlkasten.  Requires `neuron`
;; (https://neuron.zettel.page/) on the PATH to be useful.
(use-package neuron-mode
  :ensure t
  :mode "zettelkasten/.*\\.md\\'"
  :bind (("C-c C-z" . neuron-new-zettel)
         ("C-c z" . neuron-edit-zettel)))

;; Allow commenting or uncommenting full sexps in one go, rather than
;; having to do so individually by line.
(use-package comment-or-uncomment-sexp
  :ensure t
  :bind ("C-M-;" . comment-or-uncomment-sexp))

;; Keep a profiler to keep track of what might be causing pauses.
;; Currently not in MELPA so I'm just keeping the file around in the
;; f0xtr0t/ directory, but it may need to be manually updated from
;; time to time. Also, have to figure out how to make it act nice with
;; use-package :D
(require 'explain-pause-mode)
(explain-pause-mode t)

;; Use tabnine for autocompletions.
;;
;; Run `M-x company-tabnine-install-binary` to install the tabnine
;; binary.
;;
;; At any point, simply type `TabNine::config` in the editor, when
;; tabnine is enabled to enter config. Use
;; `TabNine::hide_promotional_message` to prevent popup of the pro
;; version. Use `TabNine::disable_auto_update` to disable auto-update.
;;
;; See the FAQ for more such options: https://www.tabnine.com/faq
;; (use-package company-tabnine
;;   :ensure t
;;   :init (add-to-list 'company-backends #'company-tabnine))

;; Add expand-region, which allows you to repeatedly press a key to
;; select larger and larger semantic regions.
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Enable delete-selection-mode which allows behavior that is more
;; consistent with other applications- selections are replaced when
;; you type over them, rather than just inserting at point.
(delete-selection-mode t)

;; Install lean-mode for LEAN prover programs
(use-package lean-mode
  :ensure t
  :config
  (lean-message-boxes-enable))
(use-package company-lean
  :ensure t
  :bind ("S-SPC" . company-complete))
