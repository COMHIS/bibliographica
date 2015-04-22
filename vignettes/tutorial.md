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

Your [contributions](http://ropengov.github.com/contribute.html), [bug
reports and other feedback](https://github.com/ropengov/bibliographica) are
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

Load tools and set UTF-8 encoding


```r
library(bibliographica)
Sys.setlocale(locale="UTF-8") 
```

```
## [1] ""
```

## Harmonizing textual annotation fields in library catalogues

The preprocessing functions aim to extract information from textual library catalogue annotation fields by approximating the [Bibliographic Processing Cataloging Rules](https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html). 


### Dimension information


```r
# Pick the dimension fields:
res <- polish_dimensions("1/2to (37 x 11 cm)")
library(knitr)
knitr::kable(res)
```



|original           |gatherings | width| height|
|:------------------|:----------|-----:|------:|
|1/2to (37 x 11 cm) |2to        |    11|     37|

```r
# Also estimate and fill in the missing fields where possible:
res <- polish_dimensions("1/2to (37 x 11 cm)", fill = TRUE)
knitr::kable(res)
```



|original           |gatherings | width| height| area|
|:------------------|:----------|-----:|------:|----:|
|1/2to (37 x 11 cm) |2to        |    11|     37|  407|


### Page information


```r
polish_pages("[6],viii,386p. ;")$estimated.pages
```

```
## [[1]]
## [1] 400
```

### Volume information

Pick information on volume numbers and counts:


```r
polish_volumenumber("v.4")
```

```
## v.4 
##   4
```


```r
polish_volumecount("4v.")
```

```
## 4v. 
##   4
```


### Stopwords

Removing stopwords (for a full list of stopword functions, see [here](https://github.com/rOpenGov/bibliographica/blob/master/R/stopwords.R)):


```r
remove_stopwords(c("a", "well", "james", "30 year war"), terms = "well", remove.letters = TRUE)
```

```
## [1] NA            NA            "james"       "30 year war"
```

### Dimension table

Document dimensions (gatherings, width, height) can be estimated when this information is only partially available. The estimation is made based on ready-made mapping tables. The table can be changed by the user but by default the functions use this mapping:


```r
kable(dimension_table())
```



|height |NA |2long |2to |2small |4long |4to |4small |8to |12long |12to |16to |18to |24to |32to |48to |64to |1to |
|:------|:--|:-----|:---|:------|:-----|:---|:------|:---|:------|:----|:----|:----|:----|:----|:----|:----|:---|
|90     |60 |x     |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |60  |
|55     |34 |34    |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|54     |34 |34    |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|53     |33 |33    |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|52     |33 |33    |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|51     |32 |32    |32  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|50     |32 |x     |32  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|49     |31 |x     |31  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|48     |31 |x     |31  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|47     |30 |x     |30  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|46     |30 |x     |30  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|45     |29 |x     |29  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|44     |29 |x     |29  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|43     |28 |x     |28  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|42     |27 |x     |27  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|41     |27 |x     |27  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|40     |26 |x     |26  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|39     |26 |x     |26  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|38     |35 |x     |25  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|37     |25 |x     |25  |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|36     |24 |x     |24  |x      |28    |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|35     |23 |x     |23  |x      |27    |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|34     |22 |x     |22  |x      |27    |x   |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|33     |22 |x     |22  |x      |26    |26  |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|32     |21 |x     |21  |x      |25    |25  |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|31     |21 |x     |20  |20     |24    |24  |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|30     |20 |x     |20  |20     |23    |23  |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|29     |20 |x     |19  |19     |x     |23  |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|28     |20 |x     |18  |18     |x     |22  |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|27     |20 |x     |17  |17     |x     |21  |x      |x   |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|26     |20 |x     |17  |17     |x     |20  |x      |16  |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|25     |19 |x     |x   |17     |x     |19  |x      |16  |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|24     |19 |x     |x   |16     |x     |19  |x      |15  |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|23     |18 |x     |x   |16     |x     |18  |x      |15  |15     |x    |x    |x    |x    |x    |x    |x    |x   |
|22     |15 |x     |x   |x      |x     |18  |x      |15  |14     |x    |x    |x    |x    |x    |x    |x    |x   |
|21     |14 |x     |x   |x      |x     |17  |x      |14  |14     |x    |x    |x    |x    |x    |x    |x    |x   |
|20     |13 |x     |x   |x      |x     |17  |x      |13  |13.5   |x    |x    |x    |x    |x    |x    |x    |x   |
|19     |13 |x     |x   |x      |x     |16  |x      |13  |13     |13   |x    |x    |x    |x    |x    |x    |x   |
|18     |12 |x     |x   |x      |x     |16  |16     |12  |12     |14   |x    |11   |x    |x    |x    |x    |x   |
|17     |12 |x     |x   |x      |x     |15  |15     |12  |11     |13   |x    |10.5 |x    |x    |x    |x    |x   |
|16     |11 |x     |x   |x      |x     |x   |14     |11  |x      |10.5 |13   |10   |x    |x    |x    |x    |x   |
|15     |12 |x     |x   |x      |x     |x   |14     |11  |x      |10   |12   |10   |12   |x    |x    |x    |x   |
|14     |11 |x     |x   |x      |x     |x   |x      |10  |x      |9.5  |11   |9    |11   |x    |x    |x    |x   |
|13     |10 |x     |x   |x      |x     |x   |x      |x   |x      |x    |10   |9    |10   |8    |x    |x    |x   |
|12     |9  |x     |x   |x      |x     |x   |x      |x   |x      |x    |9    |x    |9    |7    |x    |x    |x   |
|11     |8  |x     |x   |x      |x     |x   |x      |x   |x      |x    |8    |x    |8    |7    |x    |x    |x   |
|10     |7  |x     |x   |x      |x     |x   |x      |x   |x      |x    |7    |x    |x    |6    |x    |x    |x   |
|9      |6  |x     |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |6    |x    |x    |x   |
|8      |6  |x     |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |6    |x    |6    |x   |
|7      |5  |x     |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |10   |5    |x   |
|6      |4  |x     |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |10   |4    |x   |
|5      |4  |x     |x   |x      |x     |x   |x      |x   |x      |x    |x    |x    |x    |x    |9.5  |4    |x   |

### Sheet size table

Estimate sheet area in cm2:


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

Sheet sizes are calculated according to the sheet dimension table. The table can be changed by the user (see the function arguments) but by default the functions use this mapping:


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
## [1] rmarkdown_0.5.1       bibliographica_0.1.29 knitr_1.9            
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.11.5      digest_0.6.8     dplyr_0.4.1      assertthat_0.1  
##  [5] MASS_7.3-40      grid_3.2.0       plyr_1.8.1       gtable_0.1.2    
##  [9] DBI_0.3.1        formatR_1.2      magrittr_1.5     scales_0.2.4    
## [13] evaluate_0.7     ggplot2_1.0.1    lazyeval_0.1.10  reshape2_1.4.1  
## [17] proto_0.3-10     tools_3.2.0      stringr_0.6.2    munsell_0.4.2   
## [21] yaml_2.1.13      parallel_3.2.0   colorspace_1.2-6 htmltools_0.2.6
```
