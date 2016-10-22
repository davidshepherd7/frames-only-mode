EMACS=emacs
CASK ?= cask

build :
	cask exec $(EMACS) -Q --batch --eval             \
	    "(progn                                \
	      (batch-byte-compile))" frames-only-mode.el

# TODO: add to progn above:
# (setq byte-compile-error-on-warn t)  \

clean :
	@rm -f *.elc

test: build
	cask exec ecukes

install:
	${CASK} install

.PHONY:	all test install clean build
