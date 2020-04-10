# For info on Travis R scripts, see
# http://jtleek.com/protocols/travis_bioc_devel/

# Roxygen tips:
# http://r-pkgs.had.co.nz/man.html

<<<<<<< HEAD
~/bin/R-patched/bin/R CMD build ../../ --no-build-vignettes #--no-tests #--no-examples 
~/bin/R-patched/bin/R CMD check bibliographica_0.2.55.tar.gz --no-build-vignettes --no-tests #--no-examples 
~/bin/R-patched/bin/R CMD INSTALL bibliographica_0.2.55.tar.gz
=======
~/bin/R-3.6.2/bin/R CMD build ../../ --no-build-vignettes #--no-tests #--no-examples 
~/bin/R-3.6.2/bin/R CMD check bibliographica_0.2.55.tar.gz --no-build-vignettes --no-tests #--no-examples 
~/bin/R-3.6.2/bin/R CMD INSTALL bibliographica_0.2.55.tar.gz
>>>>>>> 8dab4359a9817fca53abc13f00b802eb130c8ac7
