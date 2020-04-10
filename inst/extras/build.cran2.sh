# For info on Travis R scripts, see
# http://jtleek.com/protocols/travis_bioc_devel/

# Roxygen tips:
# http://r-pkgs.had.co.nz/man.html

~/bin/R-patched/bin/R CMD build ../../ --no-build-vignettes #--no-tests #--no-examples 
~/bin/R-patched/bin/R CMD check bibliographica_0.2.55.tar.gz --no-build-vignettes --no-tests #--no-examples 
~/bin/R-patched/bin/R CMD INSTALL bibliographica_0.2.55.tar.gz
