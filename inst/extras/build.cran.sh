# For info on Travis R scripts, see
# http://jtleek.com/protocols/travis_bioc_devel/

# Roxygen tips:
# http://r-pkgs.had.co.nz/man.html

#/home/lei/bin/R CMD BATCH document.R
/home/lei/bin/R-3.4.0/bin/R CMD build ../../ --no-build-vignettes #--no-tests #--no-examples 
#/home/lei/bin/R-3.4.0/bin/R CMD check --as-cran bibliographica_0.1.22.tar.gz
/home/lei/bin/R-3.4.0/bin/R CMD check bibliographica_0.2.31.tar.gz --no-build-vignettes --no-tests #--no-examples 
/home/lei/bin/R-3.4.0/bin/R CMD INSTALL bibliographica_0.2.31.tar.gz
