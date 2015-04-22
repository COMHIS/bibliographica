---
title: "bibliographica vignette"
author: "Leo Lahti, Niko Ilomaki, Mikko Tolonen"
date: "2015-04-22"
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

The bibliographica R package provides tools for automated extraction and analysis of bibliographic metadata collections (library catalogues) such as the [ESTC](http://estc.bl.uk/F/?func=file&file_name=login-bl-estc). The toolkit includes functions to extract and clean up information from plain text library catalogue annotation fields following common standards such as the [Bibliographic Processing Cataloging Rules](https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html). This package can be used in combination with any library catalogue that follows these common standards.

Your [contributions](http://ropengov.github.com/contribute.html), [bug
reports and suggestions](https://github.com/ropengov/bibliographica) are
welcome!

## Installation

We assume you have installed [R](http://www.r-project.org/). If you
use [RStudio](http://www.rstudio.com/ide/download/desktop), change the
default encoding to UTF-8. 

Installing the stable release version in R:


```r
library(devtools)
install_github("ropengov/bibliographica")
```

Load the tools:


```r
library(bibliographica)
library(knitr)
```

## Harmonizing textual annotation fields in library catalogues

Below, you will find simple examples on the package functionality. In real studies the tools can be used to preprocess collections with millions of documents.


### Page information

Estimate the total page count for two documents:


```r
unlist(polish_pages(c("50 p.", "[6],viii,386p. ;"))$estimated.pages)
```

```
## [1]  50 400
```


### Dimension information

Extract and print document dimension information in a harmonized format:


```r
res <- polish_dimensions("1/2to (37 cm)")
knitr::kable(res)
```



|original      |gatherings | width| height|
|:-------------|:----------|-----:|------:|
|1/2to (37 cm) |2to        |    NA|     37|

The missing fields can be estimated with the 'fill' argument:


```r
res <- polish_dimensions("1/2to (37 cm)", fill = TRUE)
knitr::kable(res)
```



|original      |gatherings | width| height| area|
|:-------------|:----------|-----:|------:|----:|
|1/2to (37 cm) |2to        |    25|     37|  925|

Estimation of the missing information (gatherings, width, and/or height) is based on a ready-made [dimension mapping table](https://github.com/rOpenGov/bibliographica/blob/master/inst/extdata/documentdimensions.csv). This table can be changed by the user if necessary (see function arguments). The default table can be retrieved in R with:


```r
dtab <- dimension_table()
kable(head(dtab)) # just show the first rows
```



|height |NA |2long |2to |2small |4long |4to |4small |8to |12long |12to |16to |18to |24to |32to |48to |64to |1to |
|:------|:--|:-----|:---|:------|:-----|:---|:------|:---|:------|:----|:----|:----|:----|:----|:----|:----|:---|
|90     |60 |x     |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |60  |
|55     |34 |34    |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|54     |34 |34    |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|53     |33 |33    |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|52     |33 |33    |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|51     |32 |32    |32  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |

### Volume information

Pick information on the volume numbers:


```r
# Volume number 3 from multi-volume document
unname(polish_volumenumber("v.3, 50 p"))
```

```
## [1] 3
```

Pick information on the total volume count:


```r
# Document with 4 volumes and missing page information
unname(polish_volumecount("4v.")) 
```

```
## [1] 4
```

### Sheet size table

Check approximated sheet area for [folio](http://en.wikipedia.org/wiki/Folio). The area units are in cm2. Also other sheet types are available.


```r
sheet_area("folio")
```

```
## sheet.dimension.table not given, using sheet_sizes() mapping table by default
## The input folio corresponds to  paper with 30 cm width and 45 cm height and an area of 1350 cm2
```

```
## [1] 1350
```

The sheet sizes are calculated in the above example according to the [sheet size table](https://github.com/rOpenGov/bibliographica/blob/master/inst/extdata/sheetsizes.csv). The table can be changed by the user (see the function arguments) but by default the functions use this mapping:


```r
kable(sheet_area()) 
```



|format              |gatherings | width| height|    area|
|:-------------------|:----------|-----:|------:|-------:|
|sheet               |1to        |  60.0|   90.0| 5760.00|
|broadside           |bs         |  60.0|   64.0| 3840.00|
|folio-large         |2long      |  30.0|   53.0| 1749.00|
|folio               |2to        |  30.0|   45.0| 1350.00|
|small-folio         |2small     |  25.0|   38.0|  950.00|
|quarto-long         |4long      |  27.0|   35.0|  945.00|
|quarto              |4to        |  22.0|   28.0|  616.00|
|small-quarto        |4small     |  17.0|   20.0|  340.00|
|octavo              |8to        |  13.0|   19.0|  247.00|
|duodecimo           |12to       |  12.5|   19.0|  237.50|
|long-duodecimo      |12long     |  13.0|   20.0|  260.00|
|sextodecimo         |16to       |  12.0|   15.0|  180.00|
|octodecimo          |18to       |  10.0|   16.0|  160.00|
|vigesimo-quarto     |24to       |  12.5|    8.5|  106.25|
|trigesimo-secundo   |32to       |   7.0|   12.0|   84.00|
|quadragesimo-octavo |48to       |   6.5|   10.0|   65.00|
|sexagesimo-quarto   |64to       |   5.0|    7.0|   35.00|


### Stopwords

Removing [stopwords](http://en.wikipedia.org/wiki/Stop_words) is often necessary in text analysis. The stopwords form multiple categories, such as individual letters, conjugates, special characters, or particular expressions. The definition of a stopword may also depend on a context. The following example removes the term "well" and individual letters from the input vector:


```r
remove_stopwords(c("a", "well", "james", "30 year war"), terms = "well", remove.letters = TRUE)
```

```
## [1] NA            NA            "james"       "30 year war"
```

For a full list of stopword and related functions, see the [function documentation](https://github.com/rOpenGov/bibliographica/blob/master/man/). We also provide some [ready-made stopword lists](https://github.com/rOpenGov/bibliographica/tree/master/inst/extdata) that can be easily downloaded in R with the 'read.csv' function. 


## Licensing and Citations

This work can be freely used, modified and distributed under the 
[Two-clause BSD license](http://en.wikipedia.org/wiki/BSD\_licenses).


```r
citation("bibliographica")
```

```
## 
## Kindly cite this R package as follows:
## 
##   (C) Leo Lahti, Niko Ilomaki, Mikko Tolonen (rOpenGov 2015).
##   bibliographica R package URL:
##   http://github.com/ropengov/bibliographica
## 
## A BibTeX entry for LaTeX users is
## 
##   @Misc{,
##     title = {bibliographica: tools for bibliographic analysis},
##     author = {Leo Lahti and Niko Ilomaki and Mikko Tolonen},
##     year = {2015},
##   }
## 
## Many thanks for all contributors! See: http://ropengov.github.io
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
## [1] rmarkdown_0.5.1       bibliographica_0.1.30 knitr_1.9            
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.11.5      digest_0.6.8     dplyr_0.4.1      assertthat_0.1  
##  [5] MASS_7.3-40      grid_3.2.0       plyr_1.8.1       gtable_0.1.2    
##  [9] DBI_0.3.1        formatR_1.2      magrittr_1.5     scales_0.2.4    
## [13] evaluate_0.7     ggplot2_1.0.1    lazyeval_0.1.10  reshape2_1.4.1  
## [17] proto_0.3-10     tools_3.2.0      stringr_0.6.2    munsell_0.4.2   
## [21] yaml_2.1.13      parallel_3.2.0   colorspace_1.2-6 htmltools_0.2.6
```
