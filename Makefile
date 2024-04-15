##
# Starintel Gserver
#
# @file
# @version 0.1

LISP ?= sbcl

all: test

run:
	$(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load source/starintel-gserver.asd \
		--eval '(ql:quickload :starintel-gserver)' \
		--eval "(sb-ext:save-lisp-and-die \"star-server\" :toplevel 'starintel-gserver::main :executable t :compression t)"
install:
	cp star-server /usr/local/bin

clean:
	rm -f ./star-server
