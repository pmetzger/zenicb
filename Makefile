# Makefile for ZenICB.    -*- makefile -*-

# $Id: Makefile,v 1.2 1998/07/19 22:16:22 fn Exp $

###### configuration section

# where do we install?
ELISPDIR=	${HOME}/lib/elisp/zenicb

###### configuration section ends.

SHELL=	/bin/sh
CP=	cp

all:
	@echo "Type \"make install\" to install into ${ELISPDIR}."

install:	installdirs
	-cd ./src && $(CP) *.el ChangeLog $(ELISPDIR)

installdirs:
	-@$(SHELL) ./mkinstalldirs $(ELISPDIR)
