.PHONY: test clean

all: test

test:
	cask exec ert-runner

# While developing, you want this:
# (package-install-from-buffer)
package:
	cask package

clean:
	rm -rf org-todotxt-pkg.el dist test/generated-files
