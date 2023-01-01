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

(setq tab-always-indent t)

(setq-hook! lsp-mode
  ;; Disable the annoying symbol-highlighting that LSP does by default
  lsp-enable-symbol-highlighting nil)

(use-package! windmove
  :init
  ;; Use the default M-<left>, M-<right>,... bindings to move across the screen.
  (windmove-default-keybindings 'meta))

(use-package! amx
  :config
  (setq amx-save-file (concat doom-cache-dir "amx-items")))

;; Make C-x 1 (delete-other-windows) reversible
(use-package! zygospore
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

;; Set up `C-;' to mean `iedit-mode'. This replaces out the default company
;; complete keybinding.
(use-package! iedit
  :bind ("C-;" . iedit-mode))

(map!
 ;; ;; Bring home/end in sync with C-a and C-e
 ;; [home]     #'doom/backward-to-bol-or-indent
 ;; [end]      #'doom/forward-to-last-non-comment-or-eol

 ;; Handle page up/down the way I like it instead (move only by lines), and
 ;; remove the scroll left/right behavior, replacing it with the original
 ;; up/down.
 [next] #'scroll-up-line
 [C-next] #'scroll-up-command
 [prior] #'scroll-down-line
 [C-prior] #'scroll-down-command

 ;; Global keybinding to instantly jump to the definition, but open in another
 ;; window. TODO: Is there an alternative to this that is more DoomEmacs
 ;; specific? "M-." is assigned to #'lookup/definition which seems a lot nicer,
 ;; so probably there is a version for other-window?
 "C-'" #'xref-find-definitions-other-window

 ;; Perform the inverse of M-q
 (:after unfill "M-Q" #'unfill-paragraph)

 ;; Toggle spelling. TODO: When enabled, maybe we should trigger
 ;; `flyspell-buffer'?
 (:after flyspell "<f9>" #'flyspell-mode)

 ;; Use a hippie-expand, instead of dabbrev-expand, which has
 ;; dabbrev-expand as one of its tactics, so leads to a guaranteed
 ;; superset of expansions
 "M-/" #'hippie-expand

 ;; Much more sensible default case commands, working via DWIM rather than via
 ;; word only.
 "M-u" #'upcase-dwim
 "M-l" #'downcase-dwim
 "M-c" #'capitalize-dwim

 ;; Get some of that distract-free goodness :)
 ;;
 ;; Previously, I used to use olivetti-mode, but this actually seems nicer.
 "C-<f11>" #'+zen/toggle

 ;; Set up amx keybindings for better M-x, completely replacing out any usage of
 ;; previous `M-x' and `M-X' with the amx variants.
 (:after amx
         [remap execute-extended-command] #'amx
         [remap execute-extended-command-for-buffer] #'amx-major-mode-commands)

 ;; Set up Rust-specific keybindings I am used to
 (:after rustic
         (:map rustic-mode-map
              "M-'" #'lsp-find-references ;; replaces `abbrev-prefix-mark`
              "C-c C-c C-a" #'lsp-execute-code-action
              "C-c C-c r" #'lsp-rename ;; replaces `rustic-cargo-rm`
              "C-c C-c q" #'lsp-workspace-restart
              "C-c C-c Q" #'lsp-workspace-shutdown
              "C-c C-c s" #'lsp-rust-analyzer-status
              )
         )
 )
