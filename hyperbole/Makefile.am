# $Id: Makefile.am,v 1.5 2007/05/05 22:53:10 matsl Exp $

SUBDIRS = man kotl

EXTRA_DIST = ChangeLog ChangeLog.1 ChangeLog.2

HYPB_EXTRA = h-skip-bytec.lsp hsite.el MANIFEST DEMO .hypb MANIFEST \
	smart-clib-sym file-newer \
	man/hypb-mouse.txt

if COND_WITH_VM
HYPB_EL_VM = hvm.el
endif

HYPB_EL = \
        hact.el hactypes.el hargs.el hbdata.el hbmap.el hbut.el \
	hgnus.el hhist.el hib-doc-id.el hib-kbd.el hibtypes.el \
	hinit.el hlvar.el hmail.el hmh.el hmoccur.el hmous-info.el \
	hmouse-drv.el hmouse-key.el hmouse-mod.el hmouse-reg.el \
	hmouse-sh.el hmouse-tag.el hpath.el hrmail.el hsmail.el \
	hsys-w3.el hsys-wais.el \
	htz.el hui-menu.el hui-mini.el hui-mouse.el hui-window.el \
	hui-em19-b.el hui-xe-but.el \
	hui.el hvar.el hversion.el hypb.el hyperbole.el \
	set.el wconfig.el wrolo-logic.el wrolo-menu.el wrolo.el \
	$(HYPB_EL_VM)

ELCFILES = $(HYPB_EL:.el=.elc)

HYPB_FILES = \
	${HYPB_EXTRA} \
	${HYPB_EL}

noinst_LISP= h-skip-bytec.lsp ${HYPB_EL}

# Files to remove in 'make maintainer-clean' (+DISTCLEANFILES)
MAINTAINERCLEANFILES =				\
	*.tar.gz				\
	Makefile.in				\
	aclocal.m4				\
	configure

BUILT_SOURCES = hsite.el
hsite.el: hsite-ex.el
	\test -f hsite.el || cp hsite-ex.el hsite.el

INSTALL_DIR = $(DESTDIR)$(lispdir)/hyperbole

install-data-local: $(HYPB_FILES) ${ELCFILES}
	@$(NORMAL_INSTALL)
	@if test -n "$(INSTALL_DIR)"; then \
	  $(mkinstalldirs) $(INSTALL_DIR); \
	  list='$(HYPB_FILES)'; for p in $$list; do \
	    if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	    f="$$p"; \
	    echo " $(install_sh_DATA) $$d$$p $(INSTALL_DIR)/$$f"; \
	    $(install_sh_DATA) $$d$$p $(INSTALL_DIR)/$$f; \
	    if test -f $${p}c; then \
	      echo " $(install_sh_DATA) $${p}c $(INSTALL_DIR)/$${f}c"; \
	      $(install_sh_DATA) $${p}c $(INSTALL_DIR)/$${f}c; \
	    else : ; fi; \
	  done; \
	else : ; fi

install-data-hook:
	cd $(INSTALL_DIR); chmod 755 smart-clib-sym file-newer
