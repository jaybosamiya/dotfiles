ALL_DCONFS := $(wildcard *.dconf)
INSTALL_ALL := $(patsubst %.dconf,install-%,$(ALL_DCONFS))

all:
	@echo "Usage:"
	@echo "  make install-all"
	@echo "  make install-{nameOfFile}"
	@echo "  make watch"
	@echo "  make dump"
	@echo ""
	@echo "Known names of files:"
	@echo "  $(INSTALL_ALL)"

install-all: $(INSTALL_ALL)

watch:
	@dconf watch / | tee watch

dump:
	@dconf dump / > dconf.dump
	@echo "Dumped to dconf.dump"

$(INSTALL_ALL): install-%: %.dconf install.py
	@python install.py $<

.PHONY: all install-all watch dump $(INSTALL_ALL)
