# For info on Travis R scripts, see
# http://jtleek.com/protocols/travis_bioc_devel/

# Roxygen tips:
# http://r-pkgs.had.co.nz/man.html

#~/bin/R-3.4.3/bin/R CMD BATCH document.R
~/bin/R-3.5.1/bin/R CMD build ../../ --no-build-vignettes #--no-tests #--no-examples 
#~/bin/R-3.4.3/bin/R CMD check --as-cran bibliographica_0.1.22.tar.gz
~/bin/R-3.5.1/bin/R CMD check bibliographica_0.2.46.tar.gz --no-build-vignettes --no-tests #--no-examples 
~/bin/R-3.5.1/bin/R CMD INSTALL bibliographica_0.2.46.tar.gz
