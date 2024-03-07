;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOTES:
;;
;; + Must run `doom sync` after modifying this file
;;
;; + Doom-specific functions/macros to help configure packages:
;;   - `load!',
;;   - `use-package!'
;;   - `after!' <-- use this to set up packages when possible, to prevent
;;                  settings overriding.
;;   - `add-load-path!'
;;   - `map!'
;;
;; TODOS:
;;
;; + `persp-mode' doesn't play nicely with `uniquify' and thus the `workspaces'
;;   doom module sets the `uniquify-buffer-name-style' to `nil'. However, this
;;   means if we visit two files with the same name, we end up with the ugly
;;   <1>, <2>, ... garbage, rather than the much nicer defaults that we should
;;   have. Also, `persp-mode' means that `C-x b' doesn't work as nicely as I'd
;;   like (in particular, buffers like `*scratch*' and `*Messages*' and such
;;   just completely disappear). So yeah overall, I've just disabled
;;   `persp-mode' by disabling `workspaces' module. However, this also means all
;;   the nicety around reloading sessions is gone. Might be nice to figure out
;;   how to get that back.
;;
;; + Is there a more DoomEmacs specific alternative for "C-'" (i.e.,
;;   `xref-find-definitions-other-window'). "M-." is assigned to
;;   `lookup/definition' which seems nicer, so probably there is a version for
;;   other-window too?
;;
;; + Doom Emacs's `ido' module messes with `~` behavior. I should try to switch
;;   it back. Relevant portion, search for `ido-file-completion-map' in
;;   `ido/config.el'
;;
;; + (Maybe) Enable line numbers everywhere, rather than just the selective
;;   places it tends to right now
;;
;; + Migrate my old TeX/LaTeX/AucTeX config.
;;
;; + Migrate my old Python config.
;;
;; + Migrate my old Org config.
;;
;; + Migrate my old magit config.
;;
;; + Consider introducing elmacro?
;;
;; + Fix show-paren highlighting behavior. Search for `show-paren-match' below.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Identification, used in some situations like Emacs/GPG/...
(setq user-full-name "Jay Bosamiya"
      user-mail-address "doomemacsconfig@jaybosamiya.com")

;; MacOS specific overrides
(when IS-MAC
  ;; mac-* variables are used by the special emacs-mac build of Emacs by
  ;; Yamamoto Mitsuharu, while other builds use ns-*.
  (setq mac-command-modifier      'meta
        ns-command-modifier       'meta
        mac-option-modifier       'super
        ns-option-modifier        'super
        mac-control-modifier      'control
        ns-control-modifier       'control
        ;; Free up the right option for character composition
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none)
  ;; Doom Emacs seems to disable smooth scrolling due to "perf reasons" or some
  ;; such. I haven't found performance problems, so why not re-enable smooth
  ;; scrolling. :)
  (setq mac-mouse-wheel-smooth-scroll t)
  ;; MacOS-specific keybindings to make faster movements match with rest of
  ;; system. Ctrl-Movements however unfortunately are captured by the OS and end
  ;; up not being passed to Emacs. Weirdly though, setting up the Super-*
  ;; versions of these seems to give back control over the Ctrl-* versions.
  (map! "s-<right>" #'right-word
        "s-<left>" #'left-word
        "s-<up>" #'backward-paragraph
        "s-<down>" #'forward-paragraph
        "s-<backspace>" #'backward-kill-word)
  ;; Allow increasing and decreasing the size of the text via the scroll wheel,
  ;; for when `C-x C-=` or `C-M-=` or similar seems too annoying to type
  (map! "s-<wheel-up>" #'text-scale-increase
        "s-<wheel-down>" #'text-scale-decrease
        "S-s-<wheel-left>" #'default-text-scale-increase
        "S-s-<wheel-right>" #'default-text-scale-decrease)
  ;; Unset left/right two-finger swipes, otherwise Emacs on MacOS
  ;; decides to switch buffers when you do it which is quite unsettling.
  (map! "<swipe-left>" nil
        "<swipe-right>" nil)
  ;; Enable font ligatures on MacOS
  (mac-auto-operator-composition-mode t)
  ;; Add a convenience variant of `toggle-frame-fullscreen` that takes into
  ;; account the notch on the Mac
  (map! "M-<f11>"
        (defun toggle-frame-fullscreen-accounting-for-notch (&optional frame)
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
            (when (featurep 'cocoa) (sleep-for 0.5)))))

  ;; Doom Emacs also sets up a lot of keybindings on in `default`, and I would
  ;; like for it to not add any OS-specific things. Unfortunately, looks like
  ;; (at least until Doom v3) adding a flag to disable this is not going to
  ;; happen (see https://github.com/doomemacs/doomemacs/pull/7027) and thus I
  ;; have to manually get rid of those keybindings.
  ;;
  ;; A convenience macro that confirms that a keybinding is used by a specific
  ;; function, before unsetting it; if it is not being used by that specific
  ;; function, then it throws a warning message and leaves it as-is.
  (defmacro unset-if-used-by! (&rest args)
    "Unset a keybinding if it is used by a specific function. Otherwise, throw
a warning message and leave it as-is. ARGS accepts the syntax as in `map!'.

Example usage:

        (unset-if-used-by!
           \"s-z\" #'undo
           \"s-Z\" #'redo
           \"s-q\" (if (daemonp) #'delete-frame #'save-buffers-kill-terminal))"
    (let ((keybindings (seq-partition args 2)))
      `(progn
         ,@(mapcar (lambda (keybinding)
                     (let ((origkey (car keybinding))
                           (fn (cadr keybinding)))
                       (let ((key (if (stringp origkey) (kbd origkey) origkey)))
                         `(if (eq (lookup-key (current-global-map) ,key) ,fn)
                              (map! ,origkey nil)
                            (warn "Keybinding %s is not used by %s (used by %s instead), so not unsetting it"
                                  ,origkey ,fn (lookup-key (current-global-map) ,key))))))
                   keybindings))))
  (unset-if-used-by!
    "s-`" #'other-frame
    "s-w" #'delete-window
    "s-W" #'delete-frame
    "s-n" #'+default/new-buffer
    "s-N" #'make-frame
    "s-q" (if (daemonp) #'delete-frame #'save-buffers-kill-terminal)
    "C-s-f" #'toggle-frame-fullscreen
    "s-l" #'goto-line
    "s-f" (if (modulep! :completion vertico) #'consult-line #'swiper)
    "s-z" #'undo
    "s-Z" #'redo
    "s-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
    "s-v" #'yank
    "s-s" #'save-buffer
    "s-x" #'execute-extended-command
    "s-+" #'doom/reset-font-size
    "s-=" #'doom/increase-font-size
    "s--" #'doom/decrease-font-size
    "s-a" #'mark-whole-buffer
    "s-RET"        #'+default/newline-below
    [s-return]     #'+default/newline-below
    "S-s-RET"      #'+default/newline-above
    [S-s-return]   #'+default/newline-above)
  ;; `s-/' is used by (cmd! (save-excursion (comment-line 1))) which is an
  ;; anonymous lambda, so we just need to unset it unconditionally
  (map! "s-/" nil)

  ;; Notes on setup for other programs on MacOS:
  ;;
  ;;   SyncTex support for Skim
  ;;
  ;;     Make sure `emacsclient` is on the PATH
  ;;
  ;;        /usr/local/bin/emacsclient -> /Applications/Emacs.app/Contents/MacOS/bin/emacsclient
  ;;
  ;;     and synctex is enabled in Skim with default Emacs settings.
  ;;     Cmd-Shift-Click should then just work to jump from Skim to Emacs.
  ;;
  ;;     Support for forward search from emacs to skim is added later in this file.
  )

;; Visual niceties
(progn
  ;; Doom exposes five (optional) variables for controlling fonts in Doom:
  ;;
  ;; - `doom-font' -- the primary font to use
  ;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
  ;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
  ;;   presentations or streaming.
  ;; - `doom-unicode-font' -- for unicode glyphs
  ;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
  ;;
  ;; Here, we set up just one for now.
  (setq doom-font "Iosevka Nerd Font Mono")
  ;; Set up the theme. If we want to change this on the fly, `M-x load-theme`
  ;; instead.
  (setq doom-theme 'doom-nord)
  ;; Enable a visual bell rather than the audible bell
  (doom-themes-visual-bell-config)
  ;; Use absolute line numbers
  (setq display-line-numbers-type t)
  ;; Add search counters to the modeline
  (use-package! anzu
    :init (global-anzu-mode +1))
  ;; Set up more subtle visuals for matching parentheses, by simply
  ;; maximum-bolding them.
  ;;
  ;; TODO: Figure out why this doesn't seem to work when a new Emacs instance
  ;; begins. Likely some hook I need to wait for?
  (after! paren
    (set-face-background 'show-paren-match (face-background 'default))
    (set-face-foreground 'show-paren-match nil)
    (set-face-attribute 'show-paren-match nil :weight 'black))
  ;; Enable word-wrapping
  (global-visual-line-mode 1)
  ;; Add my personal git-modifying keywords to make them stand out, although we
  ;; have to play a tiny bit of trickery to actually let them stay in the
  ;; dotfiles repo without being nuked by the same git magic.
  (use-package! hl-todo
    :config
    (add-to-list 'hl-todo-keyword-faces (list (concat "DO " "NOT COMMIT") 'error 'bold))
    (add-to-list 'hl-todo-keyword-faces (list (concat "NEVER" "COMMIT") 'warning 'bold))))

;; Disabling things from Doom Emacs, where I prefer Vanilla Emacs instead.
(progn
  ;; Don't stay in the comment when a newline is hit within a comment
  (setq +default-want-RET-continue-comments nil)
  ;; Tab should indent the line independent of wherever you are on it.
  (setq tab-always-indent t)
  ;; Switch C-a and C-e to Emacs defaults, while setting home/end to DoomEmacs's
  ;; default C-a and C-e.
  (map! "C-a" nil "C-e" nil)
  (map! "C-a" #'move-beginning-of-line
        "C-e" #'move-end-of-line
        [home] #'doom/backward-to-bol-or-indent
        [end] #'doom/forward-to-last-non-comment-or-eol)
  ;; Disable doom-emacs's default behavior of smartparens being enabled. It
  ;; appears we can't completely remove/disable the package, but instead need to
  ;; do this. See
  ;; https://github.com/doomemacs/doomemacs/blob/35865ef5e89442e3809b8095199977053dd4210f/docs/faq.org#how-to-disable-smartparensautomatic-parentheses-completion
  (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
  ;; Disable doom-emacs's default behavior of omitting files in dired.
  (remove-hook 'dired-mode-hook #'dired-omit-mode)
  ;; Switch to default backspace behavior, rather than deleting upto previous
  ;; tab-width (and doing other shenanigans when in smartparens mode, but since we
  ;; have smartparens disabled, that bit is not particularly important; just the
  ;; tab-width deletion that is really annoying).
  (advice-remove #'delete-backward-char #'+default--delete-backward-char-a)
  ;; Rather than the annoying "immediately upon hitting tilde" change that
  ;; happens in IDO due to doom's `ido' package setting up a binding for tilde,
  ;; switch back to IDO's defaults for it, which is "do nothing" and then the
  ;; "/" causes it to actually apply the "$HOME" as expected.
  (map! (:after ido (:map ido-file-completion-map "~" nil)))
  ;; Disable the thing that DoomEmacs does, where `C-g' does not abort the macro.
  ;; I like aborting macros with `C-g'. Use `el-patch' to patch it so that in the
  ;; future, if/when DoomEmacs changes things there, I'll know soon and can fix it
  ;; easily.
  (el-patch-defun doom/escape (&optional interactive)
    "Run `doom-escape-hook'."
    (interactive (list 'interactive))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           ;; quit the minibuffer if open.
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ;; Run all escape hooks. If any returns non-nil, then stop there.
          ((run-hook-with-args-until-success 'doom-escape-hook))
          (el-patch-remove
            ;;;; PATCH: Do actually abort macros
            ;; don't abort macros
            ((or defining-kbd-macro executing-kbd-macro) nil))
          ;; Back to the default
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit))))))
  ;; `doom-snippets' is annoying and breaks so much stuff, but disabling it
  ;; directly doesn't really work, so instead what we've got to do replace out
  ;; the snippets directory with the regular snippets dir, so that when it
  ;; loads, it will just re-load our regular stuff.
  (when (file-exists-p +snippets-dir)
    (setq doom-snippets-dir +snippets-dir)))

;; Sensible defaults, that I believe should be enabled no matter what.
(progn
  ;; Ensure that copying from another program and then running a kill
  ;; command in emacs doesn't cause things to disappear from the
  ;; clipboard
  (setq save-interprogram-paste-before-kill t)
  ;; Make sure the mouse yanking pastes at point instead of at click
  (setq mouse-yank-at-point t)
  ;; Don't highlight parentheses when immediately inside an open/close pair.
  (after! paren (setq show-paren-when-point-inside-paren nil))
  ;; Restrict yasnippet expansion to only happen when the last letter of the
  ;; snippet tab trigger was typed immediately before the trigger key itself.
  (setq yas-expand-only-for-last-commands '(self-insert-command)))

;; Useful (and imho, somewhat essential) global keybindings
(progn
  ;; Up and down are set to visual lines, so use C-n and C-p for next logical
  ;; line instead; otherwise they're doing the same job as up and down, which
  ;; isn't particularly being helpful.
  (map! "C-n" #'next-logical-line
        "C-p" #'previous-logical-line)
  ;; F5 should toggle line wrapping
  (map! "<f5>" #'visual-line-mode)
  ;; F8 lets you toggle between different tab widths, to make it easier to view
  ;; files that use tabs.
  (map! "<f8>" (defun cycle-tab-widths ()
                 (interactive)
                 (let ((tab-widths '(2 4 8)))
                   (setq tab-width
                         (car (or (cdr (member tab-width tab-widths)) tab-widths)))
                   (message "Tab width set to %d" tab-width))))
  ;; Use the default M-<left>, M-<right>,... bindings to move across the screen.
  (use-package! windmove
    :init
    (windmove-default-keybindings 'meta))
  (use-package! framemove
    ;; Wait for 5 idle seconds before loading framemove. We usually don't need it
    ;; immediately, and for whatever reason, loading it immediately seems to cause
    ;; weirdness that triggers a warning/error.
    :defer 5
    :init (setq framemove-hook-into-windmove t))
  (use-package! buffer-move
    :bind (("<M-S-up>" . buf-move-up)
           ("<M-S-down>" . buf-move-down)
           ("<M-S-left>" . buf-move-left)
           ("<M-S-right>" . buf-move-right)))
  ;; Use a better alternative M-x, completely replacing out any usage of
  ;; previous M-x and M-X with the amx variants.
  (use-package! amx
    :config
    (setq amx-save-file (concat doom-cache-dir "amx-items")))
  (map!
   (:after amx
           [remap execute-extended-command] #'amx
           [remap execute-extended-command-for-buffer] #'amx-major-mode-commands))
  ;; Make C-x 1 (delete-other-windows) reversible
  (use-package! zygospore
    :bind ("C-x 1" . zygospore-toggle-delete-other-windows))
  ;; Set up `C-;' to mean `iedit-mode'. This replaces out the default company
  ;; complete keybinding.
  (use-package! iedit
    :bind ("C-;" . iedit-mode))
  ;; Add a convenience command that saves verbatim, without running any hooks
  ;; that might modify the buffer (such as reformatting). Inspired by
  ;; https://stackoverflow.com/questions/14913398/in-emacs-how-do-i-save-without-running-save-hooks
  (progn
    (defun save-buffer-while-read-only ()
      "Save file \"as is\", temporarily toggling it into read-only mode if necessary."
      (interactive)
      (if buffer-read-only
          (save-buffer)
        (read-only-mode 1)
        (save-buffer)
        (read-only-mode 0)))
    (map! "C-x C-S-s"
          (defun toggle-save-buffer-while-readonly ()
            ;; Swap C-x C-s between `save-buffer' and `save-buffer-while-read-only'
            (interactive)
            (if (eq (key-binding (kbd "C-x C-s")) 'save-buffer)
                (progn
                  (map! "C-x C-s" #'save-buffer-while-read-only)
                  (message "C-x C-s now saves verbatim"))
              (progn
                (map! "C-x C-s" #'save-buffer)
                (message "C-x C-s now saves normally"))))))
  ;; Allow fast and convenient traveling from one location to another on the
  ;; screen using avy.
  (use-package! avy
    :config (setq avy-all-windows t
                  avy-all-windows-alt nil
                  avy-background t
                  avy-single-candidate-jump nil)
    :bind (("M-j" . avy-goto-char-timer)
           ("M-J" . pop-global-mark)
           (:map isearch-mode-map
                 ("M-j" . avy-isearch))))
  ;; Handle page up/down the way I like it instead (move only by lines), and
  ;; remove the scroll left/right behavior, replacing it with the original
  ;; up/down.
  (map! [next] #'scroll-up-line
        [C-next] #'scroll-up-command
        [prior] #'scroll-down-line
        [C-prior] #'scroll-down-command)
  ;; Instantly jump to the definition, but open in another window.
  (map! "C-'" #'xref-find-definitions-other-window)
  ;; Perform the inverse of M-q
  (use-package! unfill :bind ("M-Q" . unfill-paragraph))
  ;; Toggle spelling. When enabled, maybe we should trigger `flyspell-buffer'?
  ;; Currently it happens after waiting 3 seconds anyways, so maybe not crucial
  ;; to fix too soon?
  (map! (:after flyspell "<f9>" #'flyspell-mode))
  ;; Remove C-; from flyspell-mode's map, so that we use iedit instead
  (use-package! flyspell
    :bind (:map flyspell-mode-map ("C-;" . nil)))
  ;; Be able to instantly clear any flycheck errors. Useful when looking at code
  ;; that otherwise triggers too many flycheck warnings.
  (map! "<f12>" #'flycheck-clear
        "C-<f12>" #'flycheck-mode)
  ;; Use a hippie-expand, instead of dabbrev-expand, which has
  ;; dabbrev-expand as one of its tactics, so leads to a guaranteed
  ;; superset of expansions
  (map! "M-/" #'hippie-expand)
  ;; Much more sensible default case commands, working via DWIM rather than via
  ;; word only.
  (map! "M-u" #'upcase-dwim
        "M-l" #'downcase-dwim
        "M-c" #'capitalize-dwim)
  ;; Get some of that distract-free goodness :) Previously, I used to use
  ;; olivetti-mode, but this actually seems nicer.
  (map! "C-<f11>"
        (defun zen-mode ()
          "Toggle `display-line-numbers-mode' and `+zen/toggle'"
          (interactive)
          (call-interactively 'display-line-numbers-mode)
          (call-interactively '+zen/toggle)))
  ;; Use the awesome ripgrep, along with its awesome Emacs package
  (use-package! rg
    :config (setq
             ;; Use rg from the $PATH; allows working via TRAMP too!
             rg-executable "rg"
             rg-default-alias-fallback "everything")
    :bind (("M-s M-s" . 'rg-dwim)
           ("M-s s"   . 'rg-menu)))
  ;; Introduce C-M-= and C-M-- for changing the font size all across emacs.
  (use-package! default-text-scale
    :bind (("C-M-=" . default-text-scale-increase)
           ("C-M--" . default-text-scale-decrease)
           ("C-M-0" . default-text-scale-reset)))
  ;; Use dired even for `C-x C-d` (otherwise `ido-list-directory') which is easy
  ;; to accidentally hit when trying to do `C-x d`.
  (map! (:after ido "C-x C-d" #'ido-dired))
  ;; Be able to easily jump over to my servers
  (map! "<f6>"
        (defun open-home-on-server ()
            "Opens up the home directory on a server"
            (interactive)
            (defun open-home-on-server--write (file data)
              (with-temp-file file
                (prin1 data (current-buffer))))
            (defun open-home-on-server--read (file)
              (if (file-exists-p file)
                  (with-temp-buffer
                    (insert-file-contents file)
                    (goto-char (point-min))
                    (read (current-buffer)))
                nil))
            (let* ((history-path "~/.emacs.d/.local/servers_accessed.txt")
                   (serv-history-list (open-home-on-server--read history-path))
                   (serv-name (ido-completing-read "Server: " serv-history-list)))
              (open-home-on-server--write history-path (cons serv-name (remove serv-name serv-history-list)))
              (dired (concat "/sshx:" serv-name ":~")))))
  ;; Allow `C-x n n` without the irritating warning about it being an advanced
  ;; command that confuses folks.
  (put 'narrow-to-region 'disabled nil)
  ;; Make it easy to clear out any weirdness in the current buffer, and reset to
  ;; the expected mode for it.
  (map! "<f7>" #'normal-mode)
  ;; Conveniently grab the selected part of the file into a separate indirect
  ;; read-only buffer.
  (map! "C-<f7>"
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
              (local-set-key (kbd "q") 'kill-this-buffer))))))

;; Enable hi-lock-mode globally, since it is ridiculously useful to be able to
;; do custom highlighting within a single file, to keep track of (say)
;; variables. Also super useful for reverse-engineering.
;;
;; All the keybindings show up within the namespace `C-x w`, with the most
;; useful ones being `C-x w h` and `C-x w r` to highlight and reset. `C-x w b`
;; to persist them to the file (adds comments to the file).
;;
;; Note: alternatively `M-s h r`, `M-s h u`, and `M-s h w` are equivalent,
;; respectively. Not quite sure why both versions exist.
(progn
  (global-hi-lock-mode 1)
  (map! "C-x w w" (defun highlight-region (start end)
                    "Highlight the currently selected region"
                    (interactive "r")
                    (deactivate-mark)
                    (highlight-regexp
                     (regexp-quote (buffer-substring start end))
                     (hi-lock-read-face-name)))
        "C-x w s" (defun highlight-region-symbol (start end)
                    "Highlight the currently selected symbol"
                    ;; TODO: Improve this behavior to automatically pick up the
                    ;; symbol without needing to manually specify it.
                    (interactive "r")
                    (deactivate-mark)
                    (highlight-regexp
                     (concat "\\_<" (regexp-quote (buffer-substring start end)) "\\_>")
                     (hi-lock-read-face-name)))))

;; Updating format-on-save to my personal preferences
(after! format
  (setq +format-on-save-disabled-modes
        '(;; Personal preferences for major modes I would like it disabled
          mhtml-mode
          rustic-mode
          ;; LaTeX mode isn't the same as latex-mode, smdh
          LaTeX-mode
          ;; The built-in defaults for which format-on-save is disabled
          emacs-lisp-mode
          sql-mode
          tex-mode
          latex-mode
          org-msg-edit-mode)))
;; Load aphelia immediately, as a workaround so that the stuff above is
;; respected. See
;; https://github.com/doomemacs/doomemacs/issues/7599#issuecomment-1914953178
(use-package! apheleia)

;; Set up for LSP mode across all languages.
(progn
  ;; Disable the annoying symbol-highlighting that LSP does by default
  (setq-hook! lsp-mode
    lsp-enable-symbol-highlighting nil))

;; Verus language specific things
(use-package! verus-mode
  :init (setq verus-home "~/this-sem/verus/verus"
              verus-analyzer "~/this-sem/verus/rust-analyzer"))

;; Rust language specific things
(progn
  ;; Set up Rust-specific keybindings I am used to
  (map! (:after rustic
                (:map rustic-mode-map
                      "M-'" #'lsp-find-references ;; replaces `abbrev-prefix-mark`
                      "C-c C-c C-a" #'lsp-execute-code-action
                      "C-c C-c r" #'lsp-rename ;; replaces `rustic-cargo-rm`
                      "C-c C-c q" #'lsp-workspace-restart
                      "C-c C-c Q" #'lsp-workspace-shutdown
                      "C-c C-c s" #'lsp-rust-analyzer-status)))
  ;; Enable format-on-save
  (setq rustic-format-on-save t
        ;; Due to a variety of reasons, the rustfmt folks don't want
        ;; to move away from `--edition 2015` by default. I _could_
        ;; introduce a `rustfmt.toml` to every project that specifies
        ;; the edition, but that is annoying. Personally, I have never
        ;; used the 2015 edition, and also the formatting settings
        ;; between 2018 and 2021 are practically the same, so let's
        ;; just use 2021 everywhere, until I figure out a clean way to
        ;; get the edition from the nearest `Cargo.toml`.
        rustic-rustfmt-args "--edition 2021"
        ;; Don't display the rustfmt buffer if/when there are errors.
        rustic-format-display-method #'ignore)
  ;; Enable/disable specific hints
  (setq lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-max-inlay-hint-length 10
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints nil
        lsp-rust-analyzer-display-reborrow-hints nil)
  ;; Set up a custom inlay face, to distinguish inlays from comments
  (custom-set-faces! '(lsp-rust-analyzer-inlay-face
                       ;; Default is just `font-lock-comment-face` which can be confusing
                       ;; with actual comments. Consider if there is a different style
                       ;; that might work better?
                       :inherit font-lock-comment-face
                       :weight light))
  ;; Set comment wrapping M-q default to `max_width` default from rustfmt.
  ;; Consider looking into if there is a clean way to get this from rustfmt.
  (add-hook 'rust-mode-hook (defun --improve-rust-comment-wrapping-default ()
                             (setq-local fill-column 100))))

;; ;; Org-mode
;; (progn
;;   ;; TODO: If you use `org' and don't want your org files in the default
;;   ;; location below, change `org-directory'. It must be set before org loads!
;;   (setq org-directory "~/org/"))

;; Improve horizontal scrolling behavior, preventing the buffer from scrolling
;; right any further than the longest line would let it, using code from
;; https://andreyorst.gitlab.io/posts/2022-07-20-limiting-horizontal-scroll-in-emacs/
;;
;; Additionally, it adds an advice to `M-q` to automatically make it scroll
;; horizontally again.
(progn
  (defun truncated-lines-p ()
    "Non-nil if any line is longer than `window-width' + `window-hscroll'.

Returns t if any line exceeds the right border of the window.
Used for stopping scroll from going beyond the longest line.
Based on `so-long-detected-long-line-p'."
    (save-excursion
      (goto-char (point-min))
      (let* ((window-width
              ;; This computes a more accurate width rather than `window-width', and
              ;; respects `text-scale-mode' font width.
              (/ (window-body-width nil t) (window-font-width)))
             (hscroll-offset
              ;; `window-hscroll' returns columns that are not affected by
              ;; `text-scale-mode'.  Because of that, we have to recompute the correct
              ;; `window-hscroll' by multiplying it with a non-scaled value and
              ;; dividing it with a scaled width value, rounding it to the upper
              ;; boundary.  Since there's no way to get unscaled value, we have to get
              ;; a width of a face that is not scaled by `text-scale-mode', such as
              ;; `window-divider' face.
              (ceiling (/ (* (window-hscroll) (window-font-width nil 'window-divider))
                          (float (window-font-width)))))
             (line-number-width
              ;; Compensate for line number width.  Add support for
              ;; other modes if you use any, like `linum-mode'.
              (if (bound-and-true-p display-line-numbers-mode)
                  (- display-line-numbers-width)
                0))
             (threshold (+ window-width hscroll-offset line-number-width
                           -2))) ; -2 to compensate rounding during calculation
        (catch 'excessive
          (while (not (eobp))
            (let ((start (point)))
              (save-restriction
                (narrow-to-region start (min (+ start 1 threshold)
                                             (point-max)))
                (forward-line 1))
              (unless (or (bolp)
                          (and (eobp) (<= (- (point) start)
                                          threshold)))
                (throw 'excessive t))))))))
  (define-advice scroll-left (:before-while (&rest _) prevent-overscroll)
    (and truncate-lines
         (not (memq major-mode '(vterm-mode term-mode)))
         (truncated-lines-p)))
  (define-advice fill-paragraph (:after (&rest _) scroll-back)
    ;; `scroll-right' moves us to the left edge of the screen, the argument is
    ;; number of columns; it'll never over-scroll, so we just pick a
    ;; sufficiently large number, and that'll force it to get to the left edge.
    (scroll-right 100000))
  (define-advice move-beginning-of-line (:after (&rest _) scroll-back)
    (scroll-right 100000)))

;; Set up copilot-based completions.
;;
;; NOTE: We use `(company +childframe)` in `init.el` to prevent an overlay
;; conflict. This is strongly recommended by the `copilot.el` documentation.
(use-package! copilot
  ;; Run only when `node` is installed on the system.
  :when (executable-find "node")
  ;; Currently, only run in `python-mode' buffers automatically. Require
  ;; manually invoking in other buffers.
  :hook (python-mode . copilot-mode)
  ;; By default, "f2" is bound to two-column mode, which I don't use. Override
  ;; it so that I can easily access copilot-mode.
  :bind ("<f2>" . 'copilot-mode)
  ;; Accept completions with control+tab and control+shift+tab. This only runs
  ;; if a completion overlay is active, which means that it won't interfere with
  ;; existing keybindings.
  :bind (:map copilot-completion-map
              ("C-S-<tab>" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion)))

;; Set up org-mode
(use-package! org
  :config
  (setq
   ;; Allow selecting with shift
   org-support-shift-select t
   ;; Hide emphasis markers (/, _, etc.)
   org-hide-emphasis-markers t
   ;; Disable table-of-contents generation
   org-export-with-toc nil
   ;; Show images inline upon startup
   org-startup-with-inline-images t
   ;; Show LaTeX fragment previes on startup
   ;;
   ;; Use `C-c C-x C-l' to reload previews
   org-startup-with-latex-preview t)
  ;; Reset the shift-arrow-key keywords to their defaults from what Doom Emacs
  ;; sets up. Doom Emacs sets up _waay_ more things there.
  (setq org-todo-keywords '((sequence "TODO" "DONE")))
  ;; Improve emphasis usage
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
  (add-to-list 'org-modules 'org-tempo)
  ;; Enable extra backends
  (setq org-export-backends
        (quote (ascii html icalendar latex md deck reveal)))
  (add-hook 'org-mode-hook
            (defun f0xtr0t--improve-org-look ()
              ;; Wrap lines
              (visual-line-mode 1)
              ;; Get better looking org-mode buffer
              (org-indent-mode 1))))

;; Set up F* mode
(when (featurep! :lang fstar)
  (use-package! fstar-mode
    :config
    (add-hook 'fstar-mode-hook #'ocp-setup-indent)))

;; Set up convenient hiding/unhiding
;;
;; TODO: Consider switching back to `fold-this` which I used to use before I
;; switched to Doom Emacs?
;;
;; Interestingly, Doom Emacs has a `fold` module, but it doesn't seem to do
;; things the way I like it; more specifically, it ends up completely overriding
;; the keybindings I would like to have, by doing things for vimish-fold and
;; such, that I don't want. So instead, I have to pull a bunch of their
;; configuration, and then manually tweak it into what I like here.
(use-package! hideshow
  ;; Doom Emacs already sets up some configuration to make hideshow nicer (in
  ;; its `fold` module), specifically to handle things like LaTeX, but also I
  ;; don't fully enjoy its defaults, so setting up a few more.
  :config
  ;; Copied from Doom Emacs' `fold` module
  (progn
    (setq hs-hide-comments-when-hiding-all nil
          ;; Nicer code-folding overlays (with fringe indicators)
          hs-set-up-overlay #'+fold-hideshow-set-up-overlay-fn)
    (defadvice! +fold--hideshow-ensure-mode-a (&rest _)
      "Ensure `hs-minor-mode' is enabled when we need it, no sooner or later."
      :before '(hs-toggle-hiding hs-hide-block hs-hide-level hs-show-all hs-hide-all)
      (unless (bound-and-true-p hs-minor-mode)
        (hs-minor-mode +1)))
    ;; extra folding support for more languages
    (unless (assq 't hs-special-modes-alist)
      (setq hs-special-modes-alist
            (append
             '((vimrc-mode "{{{" "}}}" "\"")
               (yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                          ""
                          "#"
                          +fold-hideshow-forward-block-by-indent-fn nil)
               (haml-mode "[#.%]" "\n" "/" +fold-hideshow-haml-forward-sexp-fn nil)
               (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
                          "end\\|[]}]"
                          "#\\|=begin"
                          ruby-forward-sexp)
               (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch"
                            "end"
                            nil (lambda (_arg) (matlab-forward-sexp)))
               (nxml-mode "<!--\\|<[^/>]*[^/]>"
                          "-->\\|</[^/>]*[^/]>"
                          "<!--" sgml-skip-tag-forward nil)
               (latex-mode
                ;; LaTeX-find-matching-end needs to be inside the env
                ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
                "\\\\end{[a-zA-Z*]+}"
                "%"
                (lambda (_arg)
                  ;; Don't fold whole document, that's useless
                  (unless (save-excursion
                            (search-backward "\\begin{document}"
                                             (line-beginning-position) t))
                    (LaTeX-find-matching-end)))
                nil))
             hs-special-modes-alist
             '((t))))))
  ;; The specific things _I_ want to change from Doom Emacs' defaults:
  (setq
   ;; Open all blocks when searching (code + comments)
   hs-isearch-open t
   ;; Hide comments too when hiding all
   hs-hide-comments-when-hiding-all t
   ;; A custom overlay function to make the overlays look nicer
   hs-set-up-overlay (defun my-hs-set-up-overlay-fn (ov)
                       ;; Add a triangle to the left fringe
                       (when (display-graphic-p)
                         (overlay-put ov 'before-string
                                      (propertize " ... "
                                                  'display
                                                  '(left-fringe
                                                    right-triangle
                                                    (color . "grey")))))))
  ;;   ;; Toggling showing and hiding code.
  ;; (map! (:after hideshow
  ;;        "C-c C-f C-f" #'hs-toggle-hiding
  ;;        "C-c C-f C-a" #'hs-show-all
  ;;        "C-c C-f C-h" #'hs-hide-all
  ;;        "C-c C-f C-s" #'hs-show-block
  ;;        "C-c C-f C-d" #'hs-hide-block))
  :bind
  (:map prog-mode-map
        ("C-c C-f C-f" . hs-toggle-hiding)
        ("C-c C-f C-s" . hs-show-block)
        ("C-c C-f C-b" . hs-hide-block)
        ("C-c C-f C-a" . hs-show-all)
        ("C-c C-f C-h" . hs-hide-all)))

(use-package! verifpal-mode)

(use-package! bibtex
  :init
  ;; Add bibtex-align-at-equal-sign to safe-local-variable-values
  (add-to-list 'safe-local-variable-values
               '(bibtex-align-at-equal-sign . t))
  (defun bibtex-clean-all-entries ()
    "Clean all entries in the current bibtex file."
    (interactive)
    (bibtex-map-entries (lambda (_ _ _) (bibtex-clean-entry))))
  :bind (:map bibtex-mode-map
              ("C-c C-S-c" . bibtex-clean-all-entries))
  :config
  (setq bibtex-entry-format t
        bibtex-align-at-equal-sign t)
  (progn
    ;; Make sure that Ctrl-shift up/down works as expected; replacing the "p"
    ;; with "^p" makes sure that the selection handling is done correctly.
    (el-patch-defun bibtex-next-entry (&optional arg)
      "Move point ARG entries forward.
ARG defaults to one.  Called interactively, ARG is the prefix
argument."
      (interactive (el-patch-swap "p" "^p"))
      (bibtex-end-of-entry)
      (when (re-search-forward bibtex-entry-maybe-empty-head nil t (or arg 1))
        (goto-char (match-beginning 0))))
    (el-patch-defun bibtex-previous-entry (&optional arg)
      "Move point ARG entries backward.
ARG defaults to one.  Called interactively, ARG is the prefix
argument."
      (interactive (el-patch-swap "p" "^p"))
      (bibtex-beginning-of-entry)
      (when (re-search-backward bibtex-entry-maybe-empty-head nil t (or arg 1))
        (goto-char (match-beginning 0))))))

(use-package! pest-mode
  :mode "\\.pest\\'"
  :hook (pest-mode . flymake-mode))

;; LaTeX specific things
(use-package! tex ; provided by auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  ;; Automatically help figure out the master file if it is not specified
  (setq-default TeX-master 'dwim)
  ;; Set up forward-search from Emacs to Skim, when on Mac
  (when IS-MAC
    ;; Set up Skim as the default PDF viewer, and set up forward-search (trigger
    ;; using "View" from C-c C-c, or directly C-c C-v)
    (add-to-list 'TeX-view-program-list
                 '("PDF Viewer"
                   "/Applications/Skim.app/Contents/SharedSupport/displayline -background -readingbar -revert %n %o %b"))
    (setq-default
     TeX-view-program-selection '((output-pdf "PDF Viewer"))))
  ;; If the "iA Quattro" font is available, switch the font buffer default to it
  (when (find-font (font-spec :name "iA Writer Quattro V"))
    (add-hook 'LaTeX-mode-hook
              (defun font-set-ia-quattro ()
                (interactive)
                (setq buffer-face-mode-face '(:family "iA Writer Quattro V"))
                (buffer-face-mode 1))))
  :hook
  (LaTeX-mode . TeX-source-correlate-mode))

;; Markdown specific things
(use-package! markdown-mode
  ;; Get rid of the annoying backtick binding
  :bind (
         (:map markdown-mode-map
               ("`" . nil))
         (:map gfm-mode-map
               ("`" . nil))))
