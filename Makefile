install: $(patsubst .elisp/%,$(HOME)/.elisp/%,$(wildcard .elisp/*)) $(patsubst .emacs.d/snippets/%,$(HOME)/.emacs.d/snippets/%,$(wildcard .emacs.d/snippets/*))
	/bin/cp -i .emacs $(HOME)/.emacs

all: external pkg-install install

$(HOME)/.elisp:
	-mkdir $(HOME)/.elisp

$(HOME)/.elisp/%: .elisp/% $(HOME)/.elisp
	-/bin/cp -rf $< $@

$(HOME)/.emacs.d/snippets/:
	-mkdir -p $(HOME)/.emacs.d/snippets/

$(HOME)/.emacs.d/snippets/%: .emacs.d/snippets/% $(HOME)/.emacs.d/snippets/
	-/bin/cp -rf $< $@

PACKAGES=\
	all-the-icons \
	auto-complete \
	auto-save-buffers-enhanced \
	csharp-mode \
	dockerfile-mode \
	expand-region \
	groovy-mode \
	helm \
	helm-git-grep \
	helm-ls-git \
	hide-lines \
	js-import \
	js2-mode \
	js2-refactor \
	js2-refactor \
	json-mode \
	less-css-mode \
	load-dir \
	markdown-mode \
	multiple-cursors \
	neotree \
	nlinum \
	org \
	org-bullets \
	python-mode \
	requirejs-mode \
	rjsx-mode \
	tern \
	tern-auto-complete \
	typescript-mode \
	web-mode

# check package-install before you add to the external target
pkg-install:
	./emacs-pkg-install.sh $(PACKAGES)

external:
# non-github links
	cd ~/.elisp; wget -Nq http://www.emacswiki.org/emacs/download/highlight-beyond-fill-column.el
