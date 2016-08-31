# Originally based on cofi/dotfiles on github

# map utility function
map = $(foreach x,$(2),$(call $(1),$(x)))

PWD = $(shell pwd)
LINK_CMD = ln --symbolic --force -T
LINK_FILES = $(shell ls -I xmonad.hs -I README -I Makefile -I scripts -I config)

define link_rule
$(1): $(2)
	$(LINK_CMD) $(PWD)/$(2) ~/.$(2)
endef

mk_link = $(eval $(call link_rule,link_$(1),$(1))) link_$(1)

all_links := $(call map,mk_link,$(LINK_FILES))

links: $(all_links)

xmonad_link:
	mkdir -p ~/.xmonad
	$(LINK_CMD) $(PWD)/xmonad.hs ~/.xmonad/xmonad.hs
	$(LINK_CMD) $(PWD)/MyLayout.hs ~/.xmonad/MyLayout.hs

zathura_link:
	mkdir -p ~/config/zathura
	$(LINK_CMD) $(PWD)/config/zathura/zathurarc ~/config/zathura/zathurarc

scripts_link:
	$(LINK_CMD) $(PWD)/scripts ~/scripts

all: links xmonad_link scripts_link zathura_link

.PHONY: all links $(all_links) zathura_link xmonad_link scripts_link refresh compile deploy

# Emacs stuff

refresh:
	 emacs --batch --no-site-file --eval '(byte-recompile-directory "elisp/")'

compile:
	 emacs --batch --no-site-file --eval '(byte-recompile-directory "elisp/" 0 t)'

deploy:
	(cd $$HOME/.elisp/auto-complete; make install DIR=$$HOME/.elisp)
	mkdir -p ~/.emacs.d/backups

