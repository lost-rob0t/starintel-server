# [[file:source.org::*Makefile][Makefile:1]]
##
# Starintel Gserver
#
# @file
# @version 0.1

LISP ?= sbcl

all: test

run:
	$(LISP) --load run.lisp

reload:
	$(LISP)	--non-interactive \
		--load source/starintel-gserver.asd \
		--eval '(ql:quickload :starintel-gserver)' \
		--eval "(sb-ext:save-lisp-and-die \"star-server\" :toplevel 'star::main :executable t)"


build:
	$(LISP)	--non-interactive \
		--load source/starintel-gserver.asd \
		--eval '(ql:quickload :starintel-gserver)' \
		--eval "(sb-ext:save-lisp-and-die \"star-server\" :toplevel 'star::main :executable t :compression t)"
install:
	cp star-server /usr/local/bin

clean:
	rm -f ./star-server
# Makefile:1 ends here
