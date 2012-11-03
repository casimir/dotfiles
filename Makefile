# Based on cofi/dotfiles on github

.PHONY: all emacs-compile elisp-compile deploy

PWD := `pwd`
LINK_CMD := ln --symbolic --force -T
NORMAL_FILES := `ls -I xmonad.hs -I README -I Makefile`

refresh:
	 emacs --batch --no-site-file --eval '(byte-recompile-directory "elisp/")'

all: compile deploy

compile:
	 emacs --batch --no-site-file --eval '(byte-recompile-directory "elisp/" 0 t)'

deploy:
	mkdir -p ~/.xmonad
	for file in $(NORMAL_FILES); do $(LINK_CMD) $(PWD)/$$file ~/.$$file; done
	$(LINK_CMD) $(PWD)/xmonad.hs ~/.xmonad/xmonad.hs
	(cd $$HOME/.elisp/auto-complete; make install DIR=$$HOME/.elisp)
