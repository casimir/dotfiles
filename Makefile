
pwd = $(shell pwd)
ln = ln --symbolic --force -T

# ----------------------------------------------------------------------------

dotted_sources = $(filter-out config scripts Makefile README.md,$(wildcard *))
dotted_targets = $(addprefix ~/.,$(dotted_sources))

~/.%: %
	$(ln) $(pwd)/$< $@

# ----------------------------------------------------------------------------

config_targets = $(addprefix ~/,scripts $(wildcard config/*))

~/%: %
	$(ln) $(pwd)/$< $@

# ----------------------------------------------------------------------------

all: $(dotted_targets) $(config_targets) ~/.kakrc $(vim_plug)

# ----------------------------------------------------------------------------
# bonus symlink to kakrc

~/.kakrc:
	$(ln) $(pwd)/config/kak/kakrc $@

# ----------------------------------------------------------------------------
# install VimPlug
vim_plug=~/.vim/autoload/plug.vim

$(vim_plug):
	curl -fLo ~/$@ --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	vim -c PlugInstall -c qa

# ----------------------------------------------------------------------------

.PHONY: all

# ----------------------------------------------------------------------------
# debugging
print-%:
	@echo '$*=$($*)'

