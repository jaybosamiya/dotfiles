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

;; Add automated performance mitigations for files with excessively
;; long lines.
(use-package so-long
  :ensure t
  :init (global-so-long-mode 1))

(provide 'f0xtr0t-large-files)
