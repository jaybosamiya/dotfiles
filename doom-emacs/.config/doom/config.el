;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jay Bosamiya"
      user-mail-address "doomemacsconfig@jaybosamiya.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font "Iosevka Nerd Font Mono")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-nord)
(doom-themes-visual-bell-config)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tab should indent the line independent of wherever you are on it.
(setq tab-always-indent t)

;; Ensure that copying from another program and then running a kill
;; command in emacs doesn't cause things to disappear from the
;; clipboard
(setq save-interprogram-paste-before-kill t)

;; Make sure the mouse yanking pastes at point instead of at click
(setq mouse-yank-at-point t)

;; Disable the annoying symbol-highlighting that LSP does by default
(setq-hook! lsp-mode
  lsp-enable-symbol-highlighting nil)

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

;; ;; Bring home/end in sync with C-a and C-e
;; (map! [home] #'doom/backward-to-bol-or-indent
;;       [end]  #'doom/forward-to-last-non-comment-or-eol)

;; Handle page up/down the way I like it instead (move only by lines), and
;; remove the scroll left/right behavior, replacing it with the original
;; up/down.
(map! [next] #'scroll-up-line
      [C-next] #'scroll-up-command
      [prior] #'scroll-down-line
      [C-prior] #'scroll-down-command)

;; Global keybinding to instantly jump to the definition, but open in another
;; window. TODO: Is there an alternative to this that is more DoomEmacs
;; specific? "M-." is assigned to #'lookup/definition which seems a lot nicer,
;; so probably there is a version for other-window?
(map! "C-'" #'xref-find-definitions-other-window)

;; Perform the inverse of M-q
(use-package! unfill :bind ("M-Q" . unfill-paragraph))

;; Toggle spelling. TODO: When enabled, maybe we should trigger
;; `flyspell-buffer'?
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

;; Set up Rust-specific keybindings I am used to
(map! (:after rustic
              (:map rustic-mode-map
                    "M-'" #'lsp-find-references ;; replaces `abbrev-prefix-mark`
                    "C-c C-c C-a" #'lsp-execute-code-action
                    "C-c C-c r" #'lsp-rename ;; replaces `rustic-cargo-rm`
                    "C-c C-c q" #'lsp-workspace-restart
                    "C-c C-c Q" #'lsp-workspace-shutdown
                    "C-c C-c s" #'lsp-rust-analyzer-status)))

;; Disable doom-emacs's default behavior of smartparens being enabled. It
;; appears we can't completely remove/disable the package, but instead need to
;; do this. See
;; https://github.com/doomemacs/doomemacs/blob/35865ef5e89442e3809b8095199977053dd4210f/docs/faq.org#how-to-disable-smartparensautomatic-parentheses-completion
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; Introduce C-M-= and C-M-- for changing the font size all across emacs.
(use-package! default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)
         ("C-M-0" . default-text-scale-reset)))

;; Add search counters to the modeline
(use-package! anzu
  :init (global-anzu-mode +1))

;; Use the awesome ripgrep, along with its awesome Emacs package
(use-package! rg
  :ensure t
  :config (setq
           ;; Use rg from the $PATH; allows working via TRAMP too!
           rg-executable "rg"
           rg-default-alias-fallback "everything")
  :bind (("M-s M-s" . 'rg-dwim)
         ("M-s s"   . 'rg-menu)))

;; FIXME:
;;
;; 1. `persp-mode' doesn't play nicely with `uniquify' and thus the `workspaces'
;;    doom module sets the `uniquify-buffer-name-style' to `nil'. However, this
;;    means if we visit two files with the same name, we end up with the ugly
;;    <1>, <2>, ... garbage, rather than the much nicer defaults that we should
;;    have.
