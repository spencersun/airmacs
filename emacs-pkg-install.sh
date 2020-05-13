#!/bin/sh

emacs --batch --eval "(package-refresh-contents)"
for pkg in $*; do
    emacs --batch --eval "(setq pkgs-to-install '($pkg))" -l emacs-pkg-install.el
done;
