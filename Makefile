EMACS=emacs
CASK ?= cask

build :
	cask exec $(EMACS) -Q --batch --eval             \
	    "(progn                                \
          (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" frames-only-mode.el

clean :
	@rm -f *.elc

test: build test-unit

test-unit:
	cask ${EMACS} --batch -L . -L test -l frames-only-mode-test.el -l revertable-set-test.el -f ert-run-tests-batch

test-load:
	cask ${EMACS} -Q --chdir . --script "load-test/loading-test.el"

install:
	${CASK} install

.PHONY:	all test test-unit install clean build
