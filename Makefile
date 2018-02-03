PKG=${shell pwd}
PKGNAME=${shell basename ${PKG}}
R=R
RSCRIPT=@Rscript

all: clean doc install test

clean:
	rm -f ${PKG}/man/*.Rd
	rm -f ${PKG}/src/*.*o
	rm -f ${PKG}/src/*.gcda

doc:
	${RSCRIPT} -e "devtools::document(roclets = c('rd', 'collate', 'namespace'))" \
		--default-packages=methods,utils,stats

install:
	${RSCRIPT} -e "devtools::install()" \
		--default-packages=methods,utils,stats

test:
	${RSCRIPT} -e "devtools::test()" \
		--default-packages=methods,utils,stats

coverage:
	${RSCRIPT} -e "covr::package_coverage(type = c('tests'))" \
		--default-packages=methods,utils,stats

help:
	@echo -e "Usage:\n\
	\tmake                    clean package, create docs, install package and run tests\n\
	\tmake clean              clean package (remove shared libraries, objects files and documentation)\n\
	\tmake doc                create documentation with Roxygen\n\
	\tmake install            install package\n\
	\tmake coverage           run tests with coverage\n\
	\tmake test               run all tests"
