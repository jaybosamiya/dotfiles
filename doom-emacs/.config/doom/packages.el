;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Disable vertical IDO, since I don't like it. Personal pereference.
(package! ido-vertical-mode :disable t)

(package! amx)

(package! unfill)

(package! iedit)

(package! zygospore)

;; Disable the default built-in doom snippets
(package! doom-snippets :ignore t)

(package! framemove)
(package! buffer-move)

(package! default-text-scale)

(package! rg)

(package! just-mode)

;; Allow conveniently patching pre-existing things, when advices don't work too
;; well. This package keeps track of the old version too, so that we can run
;; `M-x el-patch-validate-all` (or `M-x el-patch-validate` which will let you
;; check any one interactively), to make sure that expectations are met. If
;; there is indeed a difference that shows up, you can visualize it using Ediff
;; with `M-x el-patch-ediff-conflict`.
(package! el-patch)

;; Use the "suggest" package to easily find elisp functions via pairs of inputs
;; and outputs.
(package! suggest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
