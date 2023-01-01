# Jay's Dotfiles

Requires GNU Stow `sudo apt install stow`/`nix-env -iA nixpkgs.stow`

Simply `stow $PKGNAME` to add all dotfiles for PKGNAME

## Emacs config

Hand-rolled custom Emacs config (my main daily driver) works simply
via `stow emacs`, while on the other hand, I am testing out Doom
Emacs, which requires a tiny bit more effort to quickly set up.

### Doom Emacs

```sh
stow doom-emacs
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install --no-hooks # Say no to hooks, but say yes to all else
~/.emacs.d/bin/doom doctor
```

Whenever changes are made to `~/.doom.d`, run `doom sync` and restart Emacs.

`doom upgrade` to update Doom itself.
