EMACS=emacs
CASK ?= cask

build :
	cask exec $(EMACS) -Q --batch --eval             \
	    "(progn                                \
          (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" frames-only-mode.el

clean :
	@rm -f *.elc

test: build test-unit test-integration test-load

test-unit:
	cask exec ert-runner

test-integration:
	cask exec ecukes

test-load:
	cask ${EMACS} -Q --script "load-test/loading-test.el"

install:
	${CASK} install

.PHONY:	all test test-unit test-integration install clean build
