(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package just-mode
  :ensure t)

(use-package nix-mode
  :ensure t
  :defer t
  :mode ("\\.nix\\'" . nix-mode))

(provide 'f0xtr0t-lang-config-files)
