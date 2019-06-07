;;; vale-mode.el -- Major mode for writing Vale vaf files

;; Copyright (C) 2019 Jay Bosamiya
;; Author: Jay Bosamiya <jaybosamiya@gmail.com>
;; URL: TODO

;; Created: 7 June 2019
;; Version: 0.1
;; Keywords: convenience, languages

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

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

(defvar vale-tab-width 4 "Width of a tab for VALE mode")

(defvar vale-font-lock-defaults
  `((
     ;; NOTE: The order here matters. Once a color is set, it won't be
     ;; overwritten.

     ;; Comments
     ;; ("//.*$" . font-lock-comment-face)
     ;; ("/\\*[\0-\377[:nonascii:]]*?\\*/" . font-lock-comment-face)

     ;; Directives
     ( ,(regexp-opt vale-directives nil) . font-lock-preprocessor-face)
     ( "{:.*?}" . font-lock-preprocessor-face)

     ;; Keywords
     ( ,(regexp-opt vale-keywords 'symbols) . font-lock-keyword-face)

     ;; Constants
     ( ,(regexp-opt vale-constants 'symbols) . font-lock-constant-face)

     ;; Definitions/Declarations
     ("procedure \\_<\\(.*?\\)\\_>" . (1 font-lock-function-name-face))
     ("const \\_<\\(.*?\\)\\_>" . (1 font-lock-constant-face))
     ("let \\_<\\(.*?\\)\\_>" . (1 font-lock-variable-name-face))
     ("\\_<\\(.*?\\)\\_> *[:@]=" . (1 font-lock-variable-name-face))

     )))

(defun vale-backtab-to-tab-stop ()
  "Like `tab-to-tab-stop', but backwards."
  (interactive)
  (let ((nexttab (indent-next-tab-stop (current-column) t)))
    (delete-horizontal-space t)
    (indent-to nexttab)))

(define-derived-mode vale-mode fundamental-mode "VALE script"
  "VALE mode is a major mode for editing VALE files"
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
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")
  (setq-local comment-continue "  ")
  ;; (setq-local comment-style 'extra-line)
  (setq-local indent-tabs-mode nil)
  ;; words
  (modify-syntax-entry ?\" "w" vale-mode-syntax-table)
  ;; comments /* */
  (modify-syntax-entry ?\/ ". 14an12bn" vale-mode-syntax-table)
  (modify-syntax-entry ?* ". 23a" vale-mode-syntax-table)
  (modify-syntax-entry ?\n "> bn" vale-mode-syntax-table))

(provide 'vale-mode)
