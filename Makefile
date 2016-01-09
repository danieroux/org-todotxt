.PHONY: test

all: test

test:
	cask exec ert-runner
