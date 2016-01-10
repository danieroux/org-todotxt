.PHONY: test clean

all: test

test:
	cask exec ert-runner

clean:
	rm -rf org-todotxt-pkg.el dist test/generated-files
