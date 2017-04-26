TAG=$(shell git describe)
TARBALL=$(TAG).tar.gz

prepare-tarball: $(TARBALL)

%.tar.gz:
	git archive -o $@ --prefix=$*/ $*
