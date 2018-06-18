# For info on Travis R scripts, see
# http://jtleek.com/protocols/travis_bioc_devel/

# Roxygen tips:
# http://r-pkgs.had.co.nz/man.html

~/bin/R-3.4.4/bin/R CMD build ../../ --no-build-vignettes #--no-tests #--no-examples 
~/bin/R-3.4.4/bin/R CMD check bibliographica_0.2.43.tar.gz --no-build-vignettes --no-tests #--no-examples 
~/bin/R-3.4.4/bin/R CMD INSTALL bibliographica_0.2.43.tar.gz
