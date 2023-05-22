# inspired by:
#     https://github.com/yihui/knitr/blob/master/Makefile

SHELL = /bin/sh

# prepare the package for release
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename "`pwd`")
TODAY   := $(shell date "+%F")

RM = rm --force

COLOR ?= TRUE

ifeq ($(COLOR),TRUE)
	COLOR_RESET :=$(shell tput sgr0)
	COLOR_ERROR :=$(shell tput setaf 1)
	COLOR_SUCCESS :=$(shell tput setaf 2)
	COLOR_MESSAGE :=$(shell tput setaf 3)
else
	COLOR_RESET :=
	COLOR_ERROR :=
	COLOR_SUCCESS :=
	COLOR_MESSAGE :=
endif


all: docs build check install


all-cran: docs build check-cran install


help:
	$(info --------------------------------------------------------------------------------)
	$(info Available arguments:)
	$(info - "$(COLOR_MESSAGE)make help$(COLOR_RESET)"         show this help)
	$(info - "$(COLOR_MESSAGE)make deps$(COLOR_RESET)"         to check if you have all dependencies installed)
	$(info - "$(COLOR_MESSAGE)make clean$(COLOR_RESET)"        clean all the files and folders caused by building the package)
	$(info - "$(COLOR_MESSAGE)make build-noman$(COLOR_RESET)"  build without building the manual)
	$(info - "$(COLOR_MESSAGE)make build$(COLOR_RESET)"        build everything)
	$(info - "$(COLOR_MESSAGE)make docs$(COLOR_RESET)"         generate documentations)
	$(info - "$(COLOR_MESSAGE)make check$(COLOR_RESET)"        check the built package quickly)
	$(info - "$(COLOR_MESSAGE)make check-cran$(COLOR_RESET)"   check the built package in accordance to CRAN standards)
	$(info - "$(COLOR_MESSAGE)make all$(COLOR_RESET)"          alias for running docs + build + check + install)
	$(info - "$(COLOR_MESSAGE)make all-cran$(COLOR_RESET)"     alias for running docs + build + check-cran + install)
	$(info )
	$(info You can turn off colorizing the make output by $(COLOR_MESSAGE)"COLOR=FALSE"$(COLOR_RESET))
	$(info --------------------------------------------------------------------------------)
#	to suppress the "make: 'help' is up to date." message
	@:


deps:
	command -v tlmgr &> /dev/null && tlmgr install pgf preview xcolor;\
	Rscript -e 'if (!is.element("xml2", installed.packages()[, 1])){ install.packages("xml2", repos="http://cran.rstudio.com") }' ;\
	Rscript -e 'if (!is.element("stringi", installed.packages()[, 1])){ install.packages("stringi", repos="http://cran.rstudio.com") }' ;\
	Rscript -e 'if (!is.element("devtools", installed.packages()[, 1])){ install.packages("devtools", repos="http://cran.rstudio.com") }' ;\
	Rscript -e 'if (!is.element("Rd2roxygen", installed.packages()[, 1])){ install.packages("Rd2roxygen", repos="http://cran.rstudio.com") }' ;\
	Rscript -e 'if (!is.element("pROC", installed.packages()[, 1])){ install.packages("pROC", repos="http://cran.rstudio.com") }' ;\
	Rscript -e 'if (!is.element("doParallel", installed.packages()[, 1])){ install.packages("doParallel", repos="http://cran.rstudio.com") }' ;\
	Rscript -e 'if (!is.element("foreach", installed.packages()[, 1])){ install.packages("foreach", repos="http://cran.rstudio.com") }' ;\
	Rscript -e 'if (!is.element("glmnet", installed.packages()[, 1])){ install.packages("glmnet", repos="http://cran.rstudio.com") }' ;\
	Rscript -e 'if (!is.element("varhandle", installed.packages()[, 1])){ install.packages("varhandle", repos="http://cran.rstudio.com") }' ;\
	Rscript -e 'if (!is.element("rmarkdown", installed.packages()[, 1])){ install.packages("rmarkdown", repos="http://cran.rstudio.com") }' ;\
	Rscript -e 'if (!is.element("knitr", installed.packages()[, 1])){ install.packages("knitr", repos="http://cran.rstudio.com") }' ;\
	Rscript -e 'if (!is.element("markdown", installed.packages()[, 1])){ install.packages("markdown", repos="http://cran.rstudio.com") }'


build:
	sed -i -E "s/^Date: [0-9]{4}-[0-9]{2}-[0-9]{2}/Date: $(TODAY)/m" DESCRIPTION ;\
	cd .. ;\
	R CMD build $(PKGSRC)


build-noman:
	cd .. ;\
	R CMD build --no-manual --no-build-vignettes $(PKGSRC)


docs:
	$(RM) -r man/ ;\
	R -e 'devtools::document()'


install:
	cd .. ;\
	R CMD REMOVE $(PKGNAME)_$(PKGVERS).tar.gz ;\
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
	$(RM) --recursive $(PKGNAME).Rcheck/ ;\
	$(RM) $(PKGNAME)_$(PKGVERS).tar.gz ;\
	$(RM) $(PKGNAME)_*_R_x86_64-pc-linux-gnu.tar.gz


#cleanall:
#	$(shell declare -a REMOVABLE_PATTERN=('../*.Rcheck/' \
#	                                       '$(PKGNAME)_$(PKGVERS).tar.gz' \
#	                                       '$(PKGNAME)_*_R_x86_64-pc-linux-gnu.tar.gz') && \
#	        for F in "$${REMOVABLE_PATTERN[@]}"; do
#	        	find ./ -maxdepth 1 -name $${F} -exec rm --interactive '{}' \;
#	        done)
#
