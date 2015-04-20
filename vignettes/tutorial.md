---
title: "bibliographica vignette"
author: "Leo Lahti, Niko Ilomaki, Mikko Tolonen"
date: "2015-04-21"
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
Sys.setlocale(locale="UTF-8") 
```

```
## [1] ""
```


### Harmonizing manual fields data

The preprocessing functions approximate the [Bibliographic Processing Cataloging Rules](https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html).

Polish dimension information:


```r
polish_dimensions("1/2to (37 x 11 cm)")
```

```
##             original gatherings width height
## 1 1/2to (37 x 11 cm)       <NA>    11     37
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
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-unknown-linux-gnu (64-bit)
## Running under: Ubuntu 14.10
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
##  [1] Rcpp_0.11.5      digest_0.6.8     dplyr_0.4.1      assertthat_0.1  
##  [5] MASS_7.3-40      grid_3.2.0       plyr_1.8.1       gtable_0.1.2    
##  [9] DBI_0.3.1        formatR_1.1      magrittr_1.5     scales_0.2.4    
## [13] evaluate_0.6     ggplot2_1.0.1    reshape2_1.4.1   proto_0.3-10    
## [17] tools_3.2.0      stringr_0.6.2    munsell_0.4.2    yaml_2.1.13     
## [21] parallel_3.2.0   colorspace_1.2-6 htmltools_0.2.6
```
