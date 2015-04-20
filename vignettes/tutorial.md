---
title: "bibliographica vignette"
author: "Leo Lahti, Niko Ilomaki, Mikko Tolonen"
date: "2015-04-20"
bibliography: 
- bibliography.bib
- references.bib
output: html_document
---
<!--
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteIndexEntry{bibliographica vignette}
  %\usepackage[utf8]{inputenc}
-->


R tools for bibliographic data analysis
===========

Your [contributions](http://ropengov.github.com/contact.html) and [bug
reports and other feedback](https://github.com/ropengov/bibliographica) are
welcome!

## Installation

We assume you have installed [R](http://www.r-project.org/). If you
use [RStudio](http://www.rstudio.com/ide/download/desktop), change the
default encoding to UTF-8. 

Install the stable release version in R:


```r
library(devtools)
install_github("ropengov/bibliographica")
```

Load tools and set UTF-8 encoding


```r
library(bibliographica)
```


```r
Sys.setlocale(locale="UTF-8") 
```


### Harmonizing manual fields data

The preprocessing functions approximate the [Bibliographic Processing Cataloging Rules](https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html).

Polish dimension information:


```r
#polish_dimensions("1/2to (37 x 11 cm)")
```

Polish page information:


```r
polish_pages("[6],viii,386p. ;")$estimated.pages
```

```
## [[1]]
## [1] 400
```

## Licensing and Citations

This work can be freely used, modified and distributed under the 
[Two-clause BSD license](http://en.wikipedia.org/wiki/BSD\_licenses).


```r
citation("bibliographica")
```

## Session info

This vignette was created with


```r
sessionInfo()
```

```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-pc-linux-gnu (64-bit)
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] rmarkdown_0.5.1       bibliographica_0.1.28 knitr_1.9            
## 
## loaded via a namespace (and not attached):
##  [1] assertthat_0.1   colorspace_1.2-6 DBI_0.3.1        digest_0.6.8    
##  [5] dplyr_0.4.1      evaluate_0.6     formatR_1.1      ggplot2_1.0.1   
##  [9] grid_3.1.2       gtable_0.1.2     htmltools_0.2.6  magrittr_1.5    
## [13] MASS_7.3-40      munsell_0.4.2    parallel_3.1.2   plyr_1.8.1      
## [17] proto_0.3-10     Rcpp_0.11.5      reshape2_1.4.1   scales_0.2.4    
## [21] stringr_0.6.2    tools_3.1.2      yaml_2.1.13
```
