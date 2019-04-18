;;; dharma-mode.el -- Major mode for writing Dharma grammars

;; Copyright (C) 2019 Jay Bosamiya
;; Author: Jay Bosamiya <jaybosamiya@gmail.com>
;; URL: TODO

;; Created: 17 April 2019
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

(defvar dharma-constants
  '(
    "URI_TABLE"
    "LEAF_TRIGGER"
    "MAX_REPEAT_POWER"
    "VARIANCE_TEMPLATE"
    "VARIANCE_MIN"
    "VARIANCE_MAX"
    "VARIABLE_MIN"
    "VARIABLE_MAX"))

(defvar dharma-keywords
  '("%const%"
    "%section%"
    "%range%"
    "%repeat%"
    "%uri%"
    "%block%"
    "%choice%"))

(defvar dharma-tab-width 4 "Width of a tab for DHARMA mode")

(defvar dharma-font-lock-defaults
  `((
     ;; stuff between +
     ("\\+.*?\\+" . font-lock-constant-face)
     ;; stuff between @
     ("\\@.*?\\@" . font-lock-string-face)
     ;; stuff between !
     ("\\!.*?\\!" . font-lock-function-name-face)
     ;; := is special element
     (":=" . font-lock-keyword-face)
     ;; Comments start with %%%
     ("%%%.*$" . font-lock-comment-face)
     ( ,(regexp-opt dharma-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt dharma-constants 'words) . font-lock-constant-face)
     )))

(define-derived-mode dharma-mode fundamental-mode "DHARMA script"
  "DHARMA mode is a major mode for editing DHARMA files"
  (setq font-lock-defaults dharma-font-lock-defaults)
  (when dharma-tab-width
    (setq tab-width dharma-tab-width))
  (setq comment-start "%%%")
  (setq comment-end "")
  (modify-syntax-entry ?\" "w" dharma-mode-syntax-table))

(provide 'dharma-mode)
