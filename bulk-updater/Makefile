LISP ?= sbcl

.PHONY: all run build
all: build

run:
	rlwrap $(LISP) --load run.lisp

build:
	$(LISP) --non-interactive \
		--load bulk-updater.asd \
		--eval '(ql:quickload :bulk-updater)' \
		--eval '(asdf:make :bulk-updater)'

clean: -rm vsmod-updater vsmod-updater.exe