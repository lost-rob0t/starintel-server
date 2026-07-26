# [[file:source.org::*Makefile][Makefile:1]]
##
# Starintel Gserver
#
# @file
# @version 0.1

LISP ?= sbcl

.PHONY: all test integration-test images load-images compose-config stack-test

all: test

test:
	nix run .#star-unit-tests

integration-test:
	nix run .#star-integration-tests

images:
	nix build .#star-server-image .#couchdb-image .#clouseau-image

load-images:
	nix run .#load-images

compose-config:
	docker compose config --quiet

stack-test:
	./scripts/stack-test.sh

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
	rm -v ./star-server

# Makefile:1 ends here
