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
;;
;; Hopefully at some point
;; https://github.com/jaybosamiya/doomemacs/tree/allow-disabling-os-specific-keybindings
;; will be merged (https://github.com/doomemacs/doomemacs/pull/7027), which
;; allows disabling all the MacOS-specific keybindings that `default` adds
;; otherwise. Until that point, on MacOS, I need to stay on my fork of
;; doomemacs for this config to really be usable.
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
  ;;     TODO: Add support for forward search from emacs to skim
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
    (set-face-attribute 'show-paren-match nil :weight 'black)))

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
  ;; `doom-snippets' is annoying and breaks so much stuff, but disabling it directly doesn't really work, so instead what we've got to do is to patch out doom-snippets.
  (el-patch-defun doom-snippets-initialize ()
    (el-patch-remove
      "Add `doom-snippets-dir' to `yas-snippet-dirs', replacing the default
yasnippet directory."
      (setq yas-wrap-around-region nil)
      (add-to-list 'yas-snippet-dirs 'doom-snippets-dir)
      (yas-load-directory doom-snippets-dir t))))

;; Sensible defaults, that I believe should be enabled no matter what.
(progn
  ;; Ensure that copying from another program and then running a kill
  ;; command in emacs doesn't cause things to disappear from the
  ;; clipboard
  (setq save-interprogram-paste-before-kill t)
  ;; Make sure the mouse yanking pastes at point instead of at click
  (setq mouse-yank-at-point t)
  ;; Don't highlight parentheses when immediately inside an open/close pair.
  (after! paren (setq show-paren-when-point-inside-paren nil)))

;; Useful (and imho, somewhat essential) global keybindings
(progn
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
  (map! "C-<f11>" #'+zen/toggle)
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

;; Set up for LSP mode across all languages.
(progn
  ;; Disable the annoying symbol-highlighting that LSP does by default
  (setq-hook! lsp-mode
    lsp-enable-symbol-highlighting nil))

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
