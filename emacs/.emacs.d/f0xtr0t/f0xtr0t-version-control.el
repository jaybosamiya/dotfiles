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

(provide 'f0xtr0t-version-control)
