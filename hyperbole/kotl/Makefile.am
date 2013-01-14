# $Id: Makefile.am,v 1.2 2004/03/31 11:48:32 matsl Exp $

KOTL_EL = \
        kfile.el kfill.el kimport.el klabel.el \
	klink.el kmenu.el knode.el kotl-mode.el \
        kotl.el kproperty.el \
	kprop-xe.el kprop-em.el kview.el kvspec.el

ELCFILES = $(KOTL_EL:.el=.elc)

noinst_LISP = MANIFEST EXAMPLE.kotl $(KOTL_EL) 

INSTALL_DIR=$(DESTDIR)$(lispdir)/hyperbole/kotl

install-data-local: ${ELCFILES}
	@$(NORMAL_INSTALL)
	@if test -n "$(INSTALL_DIR)"; then \
	  $(mkinstalldirs) $(INSTALL_DIR); \
	  list='$(noinst_LISP)'; for p in $$list; do \
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

# Files to remove in 'make maintainer-clean' (+DISTCLEANFILES)
MAINTAINERCLEANFILES =				\
	Makefile.in

