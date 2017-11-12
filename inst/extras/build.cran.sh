# For info on Travis R scripts, see
# http://jtleek.com/protocols/travis_bioc_devel/

# Roxygen tips:
# http://r-pkgs.had.co.nz/man.html

#/usr/bin/R CMD BATCH document.R
/usr/bin/R CMD build ../../ --no-build-vignettes #--no-tests #--no-examples 
#/usr/bin/R CMD check --as-cran bibliographica_0.1.22.tar.gz
/usr/bin/R CMD check bibliographica_0.2.36.tar.gz --no-build-vignettes --no-tests #--no-examples 
/usr/bin/R CMD INSTALL bibliographica_0.2.36.tar.gz
