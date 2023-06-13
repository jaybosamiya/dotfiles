;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

; Temporarily pin transient to fix an error that shows up ; related: https://github.com/doomemacs/doomemacs/issues/7078
(package! transient :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440")

;; Disable vertical IDO, since I don't like it. Personal pereference.
(package! ido-vertical-mode :disable t)

;; I don't like the keybindings for the Doom `fold` module, nor do I like most
;; of the extra packages it pulls in. However, I do want to use the built-in
;; hideshow, so I'm disabling the Doom module and enabling the built-in package.
(package! hideshow :built-in t)

(package! amx)

(package! unfill)

(package! iedit)

(package! zygospore)

(package! framemove)

(package! buffer-move
  ;; Turns out
  ;; https://github.com/lukhas/buffer-move/commit/e7800b3ab1bd76ee475ef35507ec51ecd5a3f065
  ;; breaks things on recent enough Emacs, see
  ;; https://github.com/lukhas/buffer-move/issues/18 so until that is fixed, we
  ;; will stay on an older version.
  :pin "cb517ecf8409b5fdcda472d7190c6021f0c49751")

(package! default-text-scale)

(package! rainbow-blocks)
(package! rainbow-identifiers)

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

;; Add GitHub copilot
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; Add Verus support
(package! verus-mode
  :recipe (:host github :protocol ssh :repo "verus-lang/verus-mode.el" :branch "dev"))

;; Get ocp-indent to use with F*
(when (featurep! :lang fstar)
  (package! ocp-indent))

;; Add Dafny support
(package! boogie-friends)

;; Add Verifpal support
(package! verifpal-mode
  :recipe (:host github :protocol ssh :repo "jaybosamiya/verifpal-mode.el" :branch "main"))

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
