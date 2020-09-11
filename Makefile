# inspired by:
#     https://github.com/yihui/knitr/blob/master/Makefile

SHELL = /bin/sh

# prepare the package for release
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)


all: docs build check install

all-cran: docs build check-cran install


deps:
	tlmgr install pgf preview xcolor;\
	Rscript -e 'if (!is.element("devtools", installed.packages()[, 1])){ install.packages("devtools", repos="http://cran.rstudio.com") }';\
	Rscript -e 'if (!is.element("Rd2roxygen", installed.packages()[, 1])){ install.packages("Rd2roxygen", repos="http://cran.rstudio.com") }';

build:
    cd ..;\
	R CMD build $(PKGSRC)
	
build-noman:
    cd ..;\
	R CMD build --no-manual --no-build-vignettes $(PKGSRC)

docs:
	$(RM) -r man/;\
	R -e 'devtools::document()'

install:
    cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check:
    cd ..;\
	R CMD check --no-vignettes $(PKGNAME)_$(PKGVERS).tar.gz \
	| grep --color --extended-regexp 'ERROR|WARNING|NOTE|'

check-cran:
    cd ..;\
	R CMD check --as-cran $(PKGNAME)_$(PKGVERS).tar.gz \
	| grep --color --extended-regexp 'ERROR|WARNING|NOTE|'

travis: build-noman
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --no-manual

clean:
    cd ..;\
	$(RM) --recursive --force $(PKGNAME).Rcheck/;\
	$(RM) --recursive --force $(PKGNAME)_$(PKGVERS).tar.gz;\
	$(RM) --recursive --force $(PKGNAME)_$(PKGVERS)_R_x86_64-pc-linux-gnu.tar.gz


