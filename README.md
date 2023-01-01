# Jay's Dotfiles

Requires GNU Stow `sudo apt install stow`/`nix-env -iA nixpkgs.stow`

Simply `stow $PKGNAME` to add all dotfiles for PKGNAME

## Emacs config

My custom hand-rolled config works with `stow emacs`, but I am currently in the
process of testing out (or indeed switching to) Doom Emacs. That takes a tiny
bit more effort to set up:

```sh
stow doom-emacs
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install --no-hooks # Say no to hooks, but say yes to all else
~/.emacs.d/bin/doom doctor
```

Whenever changes are made to `~/.doom.d`, run `doom sync` and restart Emacs
(`C-c q r`). Run `doom upgrade` to update Doom itself.
