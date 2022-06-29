(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(when (or (version< emacs-version "27.0")
          ;; esup-child shows up this early only if `esup` is invoked
          (featurep 'esup-child))
  (package-initialize))

;; Allow loading customizations from the f0xtr0t directory
(add-to-list 'load-path
	     (concat user-emacs-directory
		     (convert-standard-filename "f0xtr0t/")))

;; Ensure use-package exists
(eval-when-compile
  (or (require 'use-package nil t)
      (progn
	(package-refresh-contents)
	(package-install 'use-package)
	(message "On a new system. Just installed use-package!"))))

;; Require disable-able features right out of f0xtr0t/
(require 'f0xtr0t-gui) ;; This one must be the first of the `f0xtr0t-` loads
(require 'f0xtr0t-emacs-init-debugging)
(require 'f0xtr0t-global-keybinds)
(require 'f0xtr0t-mac-os-specific)
(require 'f0xtr0t-version-control)
(require 'f0xtr0t-orgmode)
(require 'f0xtr0t-lang-common)
(require 'f0xtr0t-large-files)
;; (require 'f0xtr0t-lang-ocaml)
;; (require 'f0xtr0t-lang-dafny)
(require 'f0xtr0t-lang-config-files)
(require 'f0xtr0t-lang-latex)

;; Ensure that customization infromation edited through "Custom" has a
;; separate place to go into.
(setq custom-file (concat
                   user-emacs-directory
                   "f0xtr0t/f0xtr0t-custom-set-variables.el"))
(load custom-file)

;; Exec Path from Shell
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  ;; Use zsh as the shell to get the path from
  (setq exec-path-from-shell-shell-name "zsh")
  ;; Speed up by using non-interactive shell. This assumes that PATH
  ;; modifications are being done correctly via `.profile` and not via
  ;; editing `.zshrc`/`.bashrc` and such.
  (setq exec-path-from-shell-arguments
        (remove "-i" exec-path-from-shell-arguments))
  (exec-path-from-shell-initialize))

;; ;; Start off emacs server, so that `emacsclient` can be used
;; (load "server")
;; (if (server-running-p)
;;     (message "%s" "Server already started by someone else")
;;   (server-start))

;; imenu-anywhere lets you jump between relevant parts of code easily
;; (use-package imenu-anywhere
;;   :ensure t
;;   :bind (("C-." . imenu-anywhere)))

(use-package pdf-tools
  :ensure t
  :defer t
  :config (pdf-tools-install)
  :magic ("%PDF" . pdf-view-mode))

(use-package htmlize
  :ensure t)

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

;; Set up markdown editing
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-command "pandoc"))

;; Set up gdb to use the many-windows functionality
(setq gdb-many-windows t)

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

(use-package python
  :defer t
  :init
  ;; Set default python shell to be python3
  (setq python-shell-interpreter "python3"))

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
  :bind (:map lsp-ui-mode-map
         ("C-?" . 'lsp-ui-doc-glance)
         ("C-]" . 'lsp-ui-peek-find-references))
  :init
  ;; Make sure lsp prefers flycheck over flymake
  (setq lsp-prefer-flymake nil)
  ;; Disable the semi-annoying hover-to-see-docs view
  (setq lsp-ui-doc-enable nil))

;; ;; Connect things up to company via lsp.
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp
;;   ;; :config (push 'company-lsp company-backends)
;;   )

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
(use-package auto-package-update
  :ensure t
  :init (setq auto-package-update-prompt-before-update t
              auto-package-update-show-preview t
              auto-package-update-interval 7)
  :defer 300
  :config (auto-package-update-maybe))

;; Do whitespace cleanup iff the file was opened with clean whitespace
(use-package whitespace-cleanup-mode
  :ensure t
  :defer 5
  :delight
  :config (global-whitespace-cleanup-mode t))

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

;; Speed up flyspell by using no messages
(setq-default flyspell-issue-message-flag nil)

;; Coq
;; Proof General
;; (load-file "/usr/share/emacs/site-lisp/proofgeneral/generic/proof-site.el")
;; ;; Load company-coq when opening Coq files
;; (use-package company-coq
;;   :hook (coq-mode . company-coq-mode))

;; Agda mode
;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

;; Microsoft IVy
;; (use-package ivy-mode)

;; Prevent stale elisp bytecode from shadowing more up-to-date source
;; files
(setq load-prefer-newer t)

;; ;; Use ido-occur to be able to easily move around the buffer
;; (use-package ido-occur
;;   :ensure t
;;   :bind (("C-o" . 'ido-occur)
;; 	 :map isearch-mode-map
;; 	 ("C-o" . 'ido-occur-from-isearch)))

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

;; Be able to use rg from emacs
(use-package rg
  :ensure t
  :defer t
  :config
  (setq rg-executable "rg") ;; Use rg from the $PATH; allows
                            ;; working via TRAMP too!
  (setq rg-default-alias-fallback "everything")
  :bind (("M-s M-s" . 'rg-dwim)
	 ("M-s s"   . 'rg-menu)))

;; Make tramp uses the entire path it gets from the remote
(use-package tramp
  :defer t
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
(global-set-key (kbd "<f5>") 'visual-line-mode)

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
;; (use-package langtool
;;   :config
;;   (setq langtool-language-tool-jar
;; 	"/home/jay/.local/share/languagetool/languagetool-commandline.jar")
;;   (setq langtool-default-language "en-US"))

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

;; ;; Use language agnostic dumb-jump as a way to jump around for
;; ;; languages that don't have pre-built jumping.
;; (use-package dumb-jump
;;   :ensure t
;;   :bind (("M-g ." . dumb-jump-go)
;;          ("M-g ," . dumb-jump-back)))

;; (use-package sane-term
;;   :ensure t)

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

(use-package smerge-mode
  :init
  (setq smerge-command-prefix (kbd "C-c v")))

(use-package graphviz-dot-mode
  :defer t
  :ensure t)

(use-package urlenc
  :ensure t)

;; Rust configuration
(use-package rust-mode
  :ensure t
  :defer t
  :config
  (setq rust-format-on-save t
        rust-format-show-buffer nil)
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq split-height-threshold nil)
              ;; Set comment wrapping M-q default to `max_width`
              ;; default from rustfmt
              (setq-local fill-column 100)
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
  (setq racer-rust-src-path
        ;; Workaround for changes to where std is stored until (see
        ;; https://github.com/racer-rust/emacs-racer/pull/143)
      (let* ((sysroot (string-trim
                       (shell-command-to-string "rustc --print sysroot")))
             (lib-path (concat sysroot "/lib/rustlib/src/rust/library"))
              (src-path (concat sysroot "/lib/rustlib/src/rust/src")))
        (or (when (file-exists-p lib-path) lib-path)
            (when (file-exists-p src-path) src-path))))
  :bind (:map racer-mode-map
         ("C-'" . racer-find-definition-other-window)))
(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup)
  :hook (rust-mode . flycheck-mode))
;; (use-package flycheck-pos-tip
;;   :ensure t
;;   ;; :hook (rust-mode . flycheck-pos-tip-mode)
;; )
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
;; (use-package org-noter
;;   :ensure t
;;   :config
;;   (add-hook 'pdf-view-mode-hook
;;             (lambda () (local-set-key (kbd "i")
;;                                       #'org-noter)))
;;   (setq org-noter-always-create-frame nil))

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

;; Keep a profiler to keep track of what might be causing pauses.
;; Currently not in MELPA so I'm just keeping the file around in the
;; f0xtr0t/ directory, but it may need to be manually updated from
;; time to time. Also, have to figure out how to make it act nice with
;; use-package :D
;; (require 'explain-pause-mode)
;; (explain-pause-mode t)

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

;; Install lean-mode for LEAN prover programs
;; (use-package lean-mode
;;   :ensure t
;;   :config
;;   (lean-message-boxes-enable))
;; (use-package company-lean
;;   :ensure t)

;; Add fast and easy jumping to places using Avy
(use-package avy
  :ensure t
  :config
  (setq avy-single-candidate-jump nil)
  :bind
  (("M-j" . avy-goto-char-timer)
   ("M-J" . pop-global-mark)
   (:map isearch-mode-map
         ("M-j" . avy-isearch))))

(use-package just-mode
  :ensure t)

(use-package nix-mode
  :ensure t
  :defer t
  :mode ("\\.nix\\'" . nix-mode))

;; TODO: Look into org-tree-slide
;; TODO: Look into org-variable-pitch
