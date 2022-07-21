;; Note: This file is special in that must be `load`ed and not
;; `require`d, and the `custom-file` needs to point to the absolute
;; location. See `init.el` for something that works. Also, this file
;; is meant to not be edited manually, but instead should be
;; interacted with through `customize-...` functions.
;;
;; Over time, it would be good to move things away from here and
;; instead have them be specified in their relevant portions of my
;; dotfiles.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(coq-double-hit-enable t)
 '(custom-enabled-themes '(misterioso))
 '(delete-selection-mode nil)
 '(display-time-mode t)
 '(flyspell-default-dictionary "english")
 '(font-use-system-font nil)
 '(global-auto-revert-mode t)
 '(global-display-line-numbers-mode t)
 '(ido-default-buffer-method 'selected-window)
 '(ido-enable-flex-matching t)
 '(ido-ubiquitous-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(minimap-dedicated-window nil)
 '(minimap-hide-fringes t)
 '(minimap-highlight-line nil)
 '(minimap-sync-overlay-properties '(face invisible))
 '(minimap-update-delay 0.0)
 '(minimap-width-fraction 0.1)
 '(minimap-window-location 'right)
 '(package-selected-packages
   '(dtrt-indent yaml-mode highlight-indent-guides fstar-mode nix-mode esup shut-up just-mode avy anzu company-lean lean-mode expand-region company-tabnine comment-or-uncomment-sexp neuron-mode elmacro org-fragtog rg amx org-noter flymake-shellcheck flycheck-popup-tip flycheck-pos-tip flycheck-rust racer cargo rust-mode graphviz-dot-mode urlenc bug-hunter fill-column-indicator default-text-scale dumb-jump lsp-ui lsp-mode suggest projectile doom-modeline vale-mode buffer-move magit-todos sublimity framemove ox-gfm zoom rainbow-identifiers flycheck-package package-lint rainbow-delimiters delight langtool rainbow-identifiers-mode wc-mode vagrant-tramp undohist solarized-theme restart-emacs powerline php-mode paredit ocp-indent markdown markdown-mode guru-mode elpy dockerfile-mode caml boogie-friends visual-fill-column ido-yes-or-no ag xcscope auctex fold-this haskell-mode zygospore iedit ini-mode keyfreq vlf semantic-mode srefactor cl-lib zpresent org-present ox-reveal undo-tree minimap epresent))
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
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#2d3743" :foreground "#e1e1e0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "Iosevka Nerd Font Mono"))))
 '(doom-modeline-info ((t (:inherit bold))))
 '(ggtags-highlight ((t nil)))
 '(lsp-rust-analyzer-inlay-face ((t (:weight light :inherit (font-lock-comment-face)))))
 '(minimap-active-region-background ((t (:inverse-video t))))
 '(writegood-duplicates-face ((t (:underline (:color "DodgerBlue1" :style wave)))))
 '(writegood-passive-voice-face ((t (:underline "PaleTurquoise4"))))
 '(writegood-weasels-face ((t (:underline (:color "yellow4" :style wave))))))
