;;; vale-mode.el --- Major mode for writing Vale vaf files -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jay Bosamiya
;; Author: Jay Bosamiya <jaybosamiya@gmail.com>
;; URL: https://github.com/jaybosamiya/vale-mode.el

;; Created: 7 June 2019
;; Version: 0.1
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "25"))

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; https://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; This file implements support for Vale programming in Emacs, including:
;;
;; * Syntax highlighting
;; * Prettification (prettify-symbols-mode)
;; * Interactive proving
;; * Jumping to procedure under cursor
;;
;; See https://github.com/jaybosamiya/vale-mode.el for setup and usage tips.

;;; Code:

(defvar vale-constants
  '(
    "false"
    "true"
    ))

(defvar vale-keywords
  '(
    "calc"
    "const"
    "decreases"
    "else"
    "ensures"
    "exists"
    "extern"
    "forall"
    "friend"
    "fun"
    "function"
    "ghost"
    "if"
    "in"
    "include"
    "inline"
    "inout"
    "let"
    "lets"
    "modifies"
    "module"
    "old"
    "open"
    "operand_type"
    "operator"
    "out"
    "procedure"
    "reads"
    "requires"
    "returns"
    "reveal"
    "then"
    "this"
    "type"
    "var"
    "while"
    ))

(defvar vale-directives
  '(
    "#reset-options"
    "#set-options"
    "#verbatim"
    "#endverbatim"
    ))

(defvar vale-font-lock-defaults
  `((
     ;; NOTE: The order here matters. Once a color is set, it won't be
     ;; overwritten.

     ;; Directives
     ( ,(concat (regexp-opt vale-directives nil) ".*$") . font-lock-preprocessor-face)
     ( "{:.*?}" . font-lock-preprocessor-face)

     ;; Keywords
     ( ,(regexp-opt vale-keywords 'symbols) . font-lock-keyword-face)

     ;; Constants
     ( ,(regexp-opt vale-constants 'symbols) . font-lock-constant-face)
     ("\\_<\\([0-9]+\\)\\_>" . font-lock-constant-face)
     ("\\_<\\(0[xX][0-9]+\\)\\_>" . font-lock-constant-face)

     ;; Definitions/Declarations
     ("procedure \\_<\\([^ ]*?\\)\\_>" . (1 font-lock-function-name-face))
     ("const \\_<\\([^ ]*?\\)\\_>" . (1 font-lock-constant-face))
     ("let \\_<\\([^ ]*?\\)\\_>" . (1 font-lock-variable-name-face))
     ("\\_<\\([^ ]*?\\)\\_> *?[:@]=" . (1 font-lock-variable-name-face))
     ("\\_<\\([^ ]*?\\)\\_> *?:" . (1 font-lock-variable-name-face))
     )))

(defun vale-backtab-to-tab-stop ()
  "Like `tab-to-tab-stop', but backwards."
  (interactive)
  (let ((nexttab (indent-next-tab-stop (current-column) t)))
    (delete-horizontal-space t)
    (indent-to nexttab)))

(defgroup vale nil
  "Vale mode."
  :group 'languages)

(defcustom vale-interact-path nil
  "Path to Vale's interact.py."
  :type '(file :must-match t)
  :risky t
  :group 'vale)

(defcustom vale-tab-width 4
  "Width of a tab for Vale code."
  :type 'integer
  :group 'vale)

(defcustom vale-symbols-alist
  '(("old" . ?𝕆)
    ("nat" . ?ℕ) ("int" . ?ℤ)
    ("*" . ?×)
    ("<=" . ?≤) (">=" . ?≥)
    ("!=" . ?≠)
    ("&&" . ?∧) ("||" . ?∨)
    ("==>" . ?⟹) ("<==>" . ?⟺)
    ("exists" . ?∃) ("forall" . ?∀)
    (":=" . ?≔)
    ;; For "@=" use one of these:
    ;;    ≝ ≞ ≟ ≠ ∹ ≎ ≏ ⪮ ≐ ≑ ≒ ≓ ≔ ≕ ≖ ≗ ≘ ≙ ≚ ≛ ≜ ⩬ ⩭
    ;;    ⩮ ⩱ ⩲ ⩦ ⩴ ⩵ ⩶ ⩷ ≡ ≢ ⩧ ≍ ≭ ≣ ⩸ ≁ ≂ ≃ ≄ ⋍ ≅ ≆ ≇
    ;;    ≈ ≉ ≊ ≋ ≌ ⩯ ⩰
    ("@=" . ?≝))
  "Vale symbols."
  :group 'vale
  :type 'alist)

(defun vale--repetitions-1 (v num)
  "Return a string containing [V] repeated [NUM] times."
  (if (= num 0) ""
    (concat v (vale--repetitions-1 v (- num 1)))))

(defun vale--repetitions (v num)
  "Return a list consisting of all repetitions of [V] upto [NUM] times."
  (if (= num 0) nil
    (cons (vale--repetitions-1 v num) (vale--repetitions v (- num 1)))))

(defun vale--also-suffix (l suffix)
  "Each element of [L] is returned also with the [SUFFIX]."
  (cond
   ((null l) nil)
   (t (cons (car l) (cons (concat (car l) suffix) (vale--also-suffix (cdr l) suffix))))))

(defun vale--get-path (fname suffix)
  "Get path related to the vaf filename (FNAME) such that it ends with [SUFFIX]."
  (let* ((base (file-name-base fname))
         (expected (concat base suffix)))
    (locate-file
     expected
     (vale--also-suffix (append (vale--repetitions "../" 10) '("./")) "obj/"))))

(defun vale--interactive-buffer (fname)
  "Get the interactive buffer related to FNAME."
  (if vale-interact-path
      (let* ((fstarfilepath (vale--get-path fname ".fst"))
	     (fstarcmd (vale--get-path fname ".fst.checked.cmd"))
             (valecmd (vale--get-path fname ".fst.cmd")))
	(if (and fstarfilepath fstarcmd valecmd)
	    (with-temp-buffer
	      (cd (string-remove-suffix "obj/" (file-name-directory fstarcmd)))
	      (make-comint (concat "vale-interact(" (file-name-base fname) ")")
			   "python3" nil
			   vale-interact-path
			   "--fstar-cmd" fstarcmd
			   "--vale-cmd" valecmd
			   "--fstar-file" fstarfilepath))
          (message (concat
                    "Unable to find files related to this .vaf file. "
                    "Are you sure they exist? "))
	  nil))
    (warn (concat
           "vale-interact-path not set. "
           "Run 'M-x customize-variable RET vale-interact-path' to set the path."))
    nil))

(defun vale-interact ()
  "Run the interactive vale tool.
If in a procedure, then start verification of that procedure."
  (interactive)
  (when-let ((buf (vale--interactive-buffer (buffer-file-name (current-buffer)))))
    (when (save-excursion
	    (search-backward-regexp "procedure \\_<\\([^ ]*?\\)\\_>" nil t nil))
      (let ((matched-string (match-string 1)))
        (with-current-buffer buf
          ;; insert and send, instead of comint-send-string to make sure
          ;; we get an echo back of what was sent
          (insert (concat "v " matched-string))
          (comint-send-input))))
    (switch-to-buffer-other-window buf)))

(defun vale-jump-to-fst ()
  "Jumps to .fst file corresponding to the .vaf."
  (interactive)
  (let* ((fname (buffer-file-name (current-buffer)))
         (fstarfilepath (vale--get-path fname ".fst")))
    (if fstarfilepath
        (progn
          (push-mark nil t)
          (find-file fstarfilepath))
      (message "Could not find corresponding .fst file"))))

(defun vale-create-tags (path)
  "Create a TAGS file at PATH using etags."
  (interactive "DPath to make TAGS file in: ")
  (let* ((args (append
                '("etags"
                  "--language=none"
                  "--regex=/[ \\t]*procedure[ \\t]+\\([^ \\t]*\\)[ \\t]*(/\\1/")
                (directory-files-recursively path ".*\\.vaf$")))
         (cmd (mapconcat 'shell-quote-argument args " ")))
    (with-temp-buffer
      (cd path)
      (shell-command-to-string cmd))
    (message "Finished making TAGS file")))

(define-derived-mode vale-mode prog-mode "Vale"
  "Vale mode is a major mode for editing Vale files"
  (setq-local font-lock-defaults vale-font-lock-defaults)
  (when vale-tab-width
    (setq-local tab-width vale-tab-width)
    (setq-local tab-stop-list (number-sequence
                               vale-tab-width
                               (* 30 vale-tab-width)
                               vale-tab-width))
    (setq-local indent-line-function 'tab-to-tab-stop)
    (local-set-key (kbd "<backtab>") 'vale-backtab-to-tab-stop)
    (electric-indent-local-mode -1))
  (local-set-key (kbd "C-c C-c") 'vale-interact)
  (local-set-key (kbd "C-c C-t") 'vale-create-tags)
  (local-set-key (kbd "C-.") 'xref-find-definitions)
  (local-set-key (kbd "C-'") 'xref-find-definitions-other-window)
  (local-set-key (kbd "C-,") 'pop-tag-mark)
  (local-set-key (kbd "C-c C-a") 'vale-jump-to-fst)
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")
  (setq-local comment-continue "  ")
  ;; (setq-local comment-style 'extra-line)
  (setq-local indent-tabs-mode nil)
  ;; strings won't be showing up in vale code except in directives
  (modify-syntax-entry ?\" "w" vale-mode-syntax-table)
  ;; comments /* */
  (modify-syntax-entry ?\/ ". 14a12b" vale-mode-syntax-table)
  (modify-syntax-entry ?* ". 23a" vale-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" vale-mode-syntax-table)
  (when (and (boundp 'prettify-symbols-alist)
	     (fboundp 'prettify-symbols-mode))
    (setq-local prettify-symbols-alist (append vale-symbols-alist
					       prettify-symbols-alist))
    (prettify-symbols-mode)))

(provide 'vale-mode)

;;; vale-mode.el ends here
