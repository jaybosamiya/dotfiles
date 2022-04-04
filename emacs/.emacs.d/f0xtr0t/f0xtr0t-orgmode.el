(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-support-shift-select t "Allow selecting with shift")
  (org-hide-emphasis-markers t "Hide emphasis markers (/, _, etc.)")
  (org-export-with-toc nil "Disable table-of-contents generation")
  (org-startup-with-inline-images t "Show images inline upon startup")
  (org-startup-with-latex-preview t
   ; Use C-c C-x C-l to reload previews
   t "Show LaTeX fragment previews upon startup")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (progn
    ;; Allow windmove to continue working
    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right)
    (add-hook 'org-metaup-final-hook 'windmove-up)
    (add-hook 'org-metaleft-final-hook 'windmove-left)
    (add-hook 'org-metadown-final-hook 'windmove-down)
    (add-hook 'org-metaright-final-hook 'windmove-right)
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
  (require 'org-tempo)
  ;; Enable extra backends
  (setq org-export-backends
        (quote (ascii html icalendar latex md deck reveal)))
  (add-hook 'org-mode-hook #'(lambda ()
                               ;; Wrap lines
                               (visual-line-mode 1)
                               ;; Get better looking org-mode buffer
                               (org-indent-mode 1))))

(use-package ox-reveal
  :ensure t
  :defer t
  :config
  (progn
    (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.8.0/")
    (setq org-reveal-title-slide "<h1 class=\"title\">%t</h1><h5 class=\"author\">%a</h5><h5 class=\"date\">%d</h5>")))

;; Makes org-mode latex fragment previews nice. Automatically
;; hides/unhides them as you go over them.
(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(provide 'f0xtr0t-orgmode)
