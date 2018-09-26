TAG = $(shell git describe)
TARBALL = $(TAG).tar.gz
VERSION = $(subst obsmon-,,$(TAG))
VERSION_FILE := $(shell mktemp)

prepare-tarball: $(TARBALL)

%.tar.gz: %.tar
	gzip $^

%.tar:
	git archive -o $@ --prefix=$*/ $*
	echo $(VERSION)
	echo $(VERSION) >$(VERSION_FILE)
	tar --show-transformed-names --transform='s,$(VERSION_FILE),$*/VERSION,' \
	--owner=root --group=root --mode=664 \
	-rPf $@ $(VERSION_FILE)
	rm $(VERSION_FILE)
