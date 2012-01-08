PKGNAME = gh
VERSION = 0.4.3
SNAPDIR = $(PKGNAME)-$(VERSION)

PKGDEF    = $(PKGNAME)-pkg.el
AUTODEF   = $(PKGNAME)-auto.el
SPECIAL   = $(AUTODEF)
ALLSOURCE = $(wildcard *.el)

SOURCE  = $(filter-out $(SPECIAL), $(ALLSOURCE))
TARGET  = $(patsubst %.el,%.elc,$(SPECIAL) $(SOURCE))
MISC    = README

EMACS    = emacs
SITEFLAG = --no-site-file

PREFIX   = /usr/local
ELISPDIR = $(PREFIX)/share/emacs/site-lisp/$(PKGNAME)

TEXI2HTML = makeinfo --html --number-sections

# Location of Emacs Lisp Package Archive entries
ELPA=../../elpa

all: lisp docs

lisp: $(TARGET)

autoloads: $(AUTODEF)

$(AUTODEF): $(PKGNAME)-auto.in $(SOURCE)
	cp $(PKGNAME)-auto.in $(AUTODEF)
	rm -f $(AUTODEF)c
	@$(EMACS) -q $(SITEFLAG) -batch -L . \
		-l $(PKGNAME)-auto \
		-f gh-generate-autoloads \
		$(shell pwd | sed -e 's|^/cygdrive/\([a-z]\)|\1:|')/$(AUTODEF) .

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch -L . \
		-f batch-byte-compile $<

clean:
	rm -f *~ $(TARGET) $(PKGNAME).info $(PKGNAME).html

realclean: clean docsclean
	rm -f $(SPECIAL)

install-bin: lisp
	install -d $(ELISPDIR)
	install -m 0644 $(ALLSOURCE) $(TARGET) $(ELISPDIR)

install: install-bin

distclean: clean
	rm -Rf ../$(SNAPDIR)

release: autoloads distclean
	mkdir ../$(SNAPDIR) && chmod 0755 ../$(SNAPDIR)
	cp $(SPECIAL) $(SOURCE) ../$(SNAPDIR)
	(cd .. && tar cjf $(PKGNAME)-$(VERSION).tar.bz2 $(SNAPDIR)/*)

elpa:
	rm -fR $(ELPA)/$(SNAPDIR)
	rm -f $(ELPA)/$(PKGNAME)-$(VERSION).tar
	mkdir -p $(ELPA)/$(SNAPDIR) && chmod 0755 $(ELPA)/$(SNAPDIR)
	cp $(SOURCE) $(MISC) $(ELPA)/$(SNAPDIR)
	sed -r -e "s/%VERSION%/$(VERSION)/g" < $(PKGDEF) \
		> $(ELPA)/$(SNAPDIR)/$(PKGDEF)
	(cd $(ELPA) && tar cf $(PKGNAME)-$(VERSION).tar $(SNAPDIR))

info: doc/$(PKGNAME).info

html: doc/$(PKGNAME).texi
	$(TEXI2HTML) --no-split -o doc/$(PKGNAME).html doc/$(PKGNAME).texi

docs: info html

docsclean:
	rm -f doc/$(PKGNAME).info doc/$(PKGNAME).html
