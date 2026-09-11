# [[file:source.org::*Makefile][Makefile:1]]
##
# Starintel Gserver
#
# @file
# @version 0.1

LISP ?= sbcl
EMACS ?= emacs
HOT_RELOAD_DIR ?= .star-hot-reload

.PHONY: all test integration-test test-emacs images load-images compose-config stack-test docs-api doc-coverage reload live-patch

all: test

test:
	nix run .#star-unit-tests

docs-api:
	nix run .#gen-api-docs

doc-coverage:
	nix run .#doc-coverage-test

integration-test:
	nix run .#star-integration-tests

test-emacs:
	$(EMACS) -Q --batch -L . -l client-test.el -f ert-run-tests-batch-and-exit

images:
	nix build .#star-server-image .#couchdb-image .#clouseau-image .#rabbitmq-image

load-images:
	nix run .#load-images

compose-config:
	docker compose config --quiet

stack-test:
	./scripts/stack-test.sh

run:
	$(LISP) --load run.lisp

# Publish one source file as a live patch. The running service must have
# STAR_HOT_RELOAD=true and STAR_HOT_RELOAD_DIRECTORY pointed at this directory.
reload: live-patch

live-patch:
	@test -n "$(PATCH)" || { echo "usage: make reload PATCH=source/file.lisp [HOT_RELOAD_DIR=/path]" >&2; exit 2; }
	@test -f "$(PATCH)" || { echo "patch does not exist: $(PATCH)" >&2; exit 2; }
	@case "$(PATCH)" in *.lisp) ;; *) echo "hot patches must be .lisp files" >&2; exit 2 ;; esac
	@mkdir -p "$(HOT_RELOAD_DIR)"
	@name="$$(basename "$(PATCH)")"; \
	 tmp="$(HOT_RELOAD_DIR)/.$$name.tmp.$$$$"; \
	 cp -- "$(PATCH)" "$$tmp"; \
	 mv -f -- "$$tmp" "$(HOT_RELOAD_DIR)/$$name"; \
	 echo "published live patch: $(HOT_RELOAD_DIR)/$$name"

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
