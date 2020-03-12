# For info on Travis R scripts, see
# http://jtleek.com/protocols/travis_bioc_devel/

# Roxygen tips:
# http://r-pkgs.had.co.nz/man.html

/usr/bin/R CMD build ../../ --no-build-vignettes #--no-tests #--no-examples 
<<<<<<< HEAD
/usr/bin/R CMD check bibliographica_0.2.54.tar.gz --no-build-vignettes --no-tests #--no-examples 
/usr/bin/R CMD INSTALL bibliographica_0.2.54y.tar.gz
||||||| merged common ancestors
/usr/bin/R CMD check bibliographica_0.2.53.tar.gz --no-build-vignettes --no-tests #--no-examples 
/usr/bin/R CMD INSTALL bibliographica_0.2.53y.tar.gz
=======
/usr/bin/R CMD check bibliographica_0.2.53.tar.gz --no-build-vignettes --no-tests #--no-examples 
/usr/bin/R CMD INSTALL bibliographica_0.2.53.tar.gz
>>>>>>> a62b35651b0ffdbf5bb96aad6e18f3bc4d2a7bec
