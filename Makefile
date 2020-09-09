# inspired by:
#     https://github.com/yihui/knitr/blob/master/Makefile

# prepare the package for release
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: check clean

all-cran: check-cran clean

deps:
	tlmgr install pgf preview xcolor;\
	Rscript -e 'if (!is.element("devtools", installed.packages()[, 1])){ install.packages("devtools", repos="http://cran.rstudio.com") }';\
	Rscript -e 'if (!is.element("Rd2roxygen", installed.packages()[, 1])){ install.packages("Rd2roxygen", repos="http://cran.rstudio.com") }';

docs:
	$(RM) -r man/;\
	R -e 'devtools::document()'

build: docs
	cd ..;\
	R CMD build $(PKGSRC)

build-noman:
	cd ..;\
	R CMD build --no-manual $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz \
	| grep --color -E 'ERROR|WARNING|NOTE|'

check-cran: build
	cd ..;\
	R CMD check --as-cran $(PKGNAME)_$(PKGVERS).tar.gz \
	| grep --color -E 'ERROR|WARNING|NOTE|'

travis: build-noman
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --no-manual

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/