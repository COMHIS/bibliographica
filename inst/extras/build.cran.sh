# For info on Travis R scripts, see
# http://jtleek.com/protocols/travis_bioc_devel/

# Roxygen tips:
# http://r-pkgs.had.co.nz/man.html

<<<<<<< HEAD
#/usr/bin/R CMD BATCH document.R
/usr/bin/R CMD build ../../ --no-build-vignettes #--no-tests #--no-examples 
#/usr/bin/R CMD check --as-cran bibliographica_0.1.22.tar.gz
/usr/bin/R CMD check bibliographica_0.2.31.tar.gz --no-build-vignettes --no-tests #--no-examples 
/usr/bin/R CMD INSTALL bibliographica_0.2.31.tar.gz
||||||| merged common ancestors
#/home/lei/bin/R CMD BATCH document.R
/home/lei/bin/R-3.4.0/bin/R CMD build ../../ --no-build-vignettes #--no-tests #--no-examples 
#/home/lei/bin/R-3.4.0/bin/R CMD check --as-cran bibliographica_0.1.22.tar.gz
/home/lei/bin/R-3.4.0/bin/R CMD check bibliographica_0.2.31.tar.gz --no-build-vignettes --no-tests #--no-examples 
/home/lei/bin/R-3.4.0/bin/R CMD INSTALL bibliographica_0.2.31.tar.gz
=======
#/home/lei/bin/R CMD BATCH document.R
/usr/bin/R CMD build ../../ --no-build-vignettes #--no-tests #--no-examples 
#/usr/bin/R CMD check --as-cran bibliographica_0.1.22.tar.gz
/usr/bin/R CMD check bibliographica_0.2.31.tar.gz --no-build-vignettes --no-tests #--no-examples 
/usr/bin/R CMD INSTALL bibliographica_0.2.31.tar.gz
>>>>>>> b376431c92d122c0ab642c28ba24a47b49f20509
