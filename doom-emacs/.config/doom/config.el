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
;; + `doom-snippets' appears to break stuff if you don't have it enabled
;;   initially, before ignoring it in `./packages.el`. Weirdly, completely
;;   clearing out `~/.emacs.d` and reinstalling Doom Emacs seems to have done
;;   the trick though.
;;
;; + (Maybe) Enable line numbers everywhere, rather than just the selective
;;   places it tends to right now
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Identification, used in some situations like Emacs/GPG/...
(setq user-full-name "Jay Bosamiya"
      user-mail-address "doomemacsconfig@jaybosamiya.com")

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
    :init (global-anzu-mode +1)))

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
  ;; Switch to default backspace behavior, rather than deleting upto previous
  ;; tab-width (and doing other shenanigans when in smartparens mode, but since we
  ;; have smartparens disabled, that bit is not particularly important; just the
  ;; tab-width deletion that is really annoying).
  (advice-remove #'delete-backward-char #'+default--delete-backward-char-a)
  ;; Rather than the annoying "immediately upon hitting tilde" change that
  ;; happens in IDO due to doom's `ido' package setting up a binding for tilde,
  ;; switch back to IDO's defaults for it, which is "do nothing" and then the
  ;; "/" causes it to actually apply the "$HOME" as expected.
  (map! (:after ido (:map ido-file-completion-map "~" nil))))

;; Sensible defaults, that I believe should be enabled no matter what.
(progn
  ;; Ensure that copying from another program and then running a kill
  ;; command in emacs doesn't cause things to disappear from the
  ;; clipboard
  (setq save-interprogram-paste-before-kill t)
  ;; Make sure the mouse yanking pastes at point instead of at click
  (setq mouse-yank-at-point t))

;; Useful (and imho, somewhat essential) global keybindings
(progn
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
           ("C-M-0" . default-text-scale-reset))))


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
        rustic-rustfmt-args "--edition 2021")
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
