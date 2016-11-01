---
title: "bibliographica vignette"
author: "Leo Lahti, Niko Ilomaki, Mikko Tolonen"
date: "2016-11-01"
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
Sys.setlocale(locale="UTF-8") 
```

```
## [1] ""
```

## Harmonizing textual annotation fields in library catalogues

Below, you will find simple examples on the package functionality. In real studies the tools can be used to preprocess collections with millions of documents.

### Page information

Estimate the total page count for two documents:


```r
# unlist(polish_physical_extent(c("50 p.", "[6],viii,386p. ;"))$estimated.pages)
```


### Dimension information

Extract and print document dimension information in a harmonized format:


```r
res <- polish_dimensions("1/2fo (37 cm)")
knitr::kable(res)
```



|gatherings.original | width.original| height.original|obl.original |original  |gatherings | width| height| obl| area|
|:-------------------|--------------:|---------------:|:------------|:---------|:----------|-----:|------:|---:|----:|
|2fo                 |             NA|              37|FALSE        |2fo 37 cm |2fo        |    25|     37|   0|  925|

The missing fields can be estimated with the 'fill' argument:


```r
res <- polish_dimensions("1/2fo (37 cm)", fill = TRUE)
knitr::kable(res)
```



|gatherings.original | width.original| height.original|obl.original |original  |gatherings | width| height| obl| area|
|:-------------------|--------------:|---------------:|:------------|:---------|:----------|-----:|------:|---:|----:|
|2fo                 |             NA|              37|FALSE        |2fo 37 cm |2fo        |    25|     37|   0|  925|


Estimate the total page count:


```r
unlist(polish_physical_extent(c("50 p.", "[6],viii,386p. ;"))$estimated.pages)
```

```
## NULL
```

Estimation of the missing information (gatherings, width, and/or height) is based on a ready-made [dimension mapping table](https://github.com/rOpenGov/bibliographica/blob/master/inst/extdata/documentdimensions.csv). This table can be changed by the user if necessary (see function arguments). The default table can be retrieved in R with:


```r
dtab <- dimension_table()
kable(head(dtab)) # just show the first rows
```



|height |NA |1to |2long |2fo |2small |4long |4to |4small |8long |8vo |8small |12long |12mo |16long |16mo |18mo |24long |24mo |32mo |40mo |48mo |64mo |80mo |84mo |1to |
|:------|:--|:---|:-----|:---|:------|:-----|:---|:------|:-----|:---|:------|:------|:----|:------|:----|:----|:------|:----|:----|:----|:----|:----|:----|:----|:---|
|90     |60 |60  |x     |x   |x      |x     |x   |x      |x     |x   |x      |x      |x    |x      |x    |x    |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|110    |66 |73  |x     |x   |x      |x     |x   |x      |x     |x   |x      |x      |x    |x      |x    |x    |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|109    |66 |72  |x     |x   |x      |x     |x   |x      |x     |x   |x      |x      |x    |x      |x    |x    |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|108    |65 |72  |x     |x   |x      |x     |x   |x      |x     |x   |x      |x      |x    |x      |x    |x    |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|107    |64 |71  |x     |x   |x      |x     |x   |x      |x     |x   |x      |x      |x    |x      |x    |x    |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|106    |64 |70  |x     |x   |x      |x     |x   |x      |x     |x   |x      |x      |x    |x      |x    |x    |x      |x    |x    |x    |x    |x    |x    |x    |x   |


Conversions between different versions of the gatherings names:


```r
res <- gatherings_table()
knitr::kable(res)
```



|Alternate |Standard |Symbol |Name                |
|:---------|:--------|:------|:-------------------|
|1to       |1to      |1°     |sheet               |
|bs        |bs       |bs     |broadside           |
|2long     |2long    |2°     |folio               |
|2to       |2fo      |2°     |folio               |
|2small    |2small   |2°     |folio               |
|4long     |4long    |4°     |quarto              |
|4to       |4to      |4°     |quarto              |
|4small    |4small   |4°     |quarto              |
|8long     |8long    |8°     |octavo              |
|8to       |8vo      |8°     |octavo              |
|8small    |8small   |8°     |octavo              |
|12long    |12long   |12°    |duodecimo           |
|12to      |12mo     |12°    |duodecimo           |
|16to      |16mo     |16°    |sextodecimo         |
|16long    |16long   |16°    |sextodecimo         |
|18to      |18mo     |18°    |octodecimo          |
|24to      |24mo     |24°    |vigesimo-quarto     |
|24long    |24long   |24°    |vigesimo-quarto     |
|32to      |32mo     |32°    |trigesimo-segundo   |
|40to      |40mo     |40°    |quadraquinto        |
|48to      |48mo     |48°    |quadragesimo-octavo |
|64to      |64mo     |64°    |sexagesimo-quarto   |
|80to      |80mo     |80°    |octogentesimo       |
|84to      |84mo     |84°    |octoginta-quarto    |



### Volume information

Pick information on the volume numbers:


```r
# Volume number 3 from multi-volume document
unname(polish_physical_extent("v.3, 50 p"))
```

```
##             
## 1 50 3 NA NA
```

Pick information on the total volume count:


```r
# Document with 4 volumes and missing page information
unlist(polish_physical_extent("4v.")) 
```

```
## pagecount volnumber  volcount     parts 
##        NA        NA         4        NA
```

### Dimension information

Extract and print dimension information:


```r
res <- polish_dimensions("1/2fo (37 cm)")
knitr::kable(res)
```



|gatherings.original | width.original| height.original|obl.original |original  |gatherings | width| height| obl| area|
|:-------------------|--------------:|---------------:|:------------|:---------|:----------|-----:|------:|---:|----:|
|2fo                 |             NA|              37|FALSE        |2fo 37 cm |2fo        |    25|     37|   0|  925|

Also the missing fields can be estimated:


```r
res <- polish_dimensions("1/2fo (37 cm)", fill = TRUE)
knitr::kable(res)
```



|gatherings.original | width.original| height.original|obl.original |original  |gatherings | width| height| obl| area|
|:-------------------|--------------:|---------------:|:------------|:---------|:----------|-----:|------:|---:|----:|
|2fo                 |             NA|              37|FALSE        |2fo 37 cm |2fo        |    25|     37|   0|  925|

Estimation of the missing information (gatherings, width, and/or height) is based on a ready-made [approximation table](https://github.com/rOpenGov/bibliographica/blob/master/inst/extdata/documentdimensions.csv). The table can be changed by the user (see function arguments). The default table can be retrieved in R with:


```r
dtab <- dimension_table()
kable(head(dtab)) # just print the first rows
```



|height |NA |1to |2long |2fo |2small |4long |4to |4small |8long |8vo |8small |12long |12mo |16long |16mo |18mo |24long |24mo |32mo |40mo |48mo |64mo |80mo |84mo |1to |
|:------|:--|:---|:-----|:---|:------|:-----|:---|:------|:-----|:---|:------|:------|:----|:------|:----|:----|:------|:----|:----|:----|:----|:----|:----|:----|:---|
|90     |60 |60  |x     |x   |x      |x     |x   |x      |x     |x   |x      |x      |x    |x      |x    |x    |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|110    |66 |73  |x     |x   |x      |x     |x   |x      |x     |x   |x      |x      |x    |x      |x    |x    |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|109    |66 |72  |x     |x   |x      |x     |x   |x      |x     |x   |x      |x      |x    |x      |x    |x    |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|108    |65 |72  |x     |x   |x      |x     |x   |x      |x     |x   |x      |x      |x    |x      |x    |x    |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|107    |64 |71  |x     |x   |x      |x     |x   |x      |x     |x   |x      |x      |x    |x      |x    |x    |x      |x    |x    |x    |x    |x    |x    |x    |x   |
|106    |64 |70  |x     |x   |x      |x     |x   |x      |x     |x   |x      |x      |x    |x      |x    |x    |x      |x    |x    |x    |x    |x    |x    |x    |x   |

### Sheet size table

Check approximated sheet area for [folio](http://en.wikipedia.org/wiki/Folio). The area units are in cm2. Also other sheet types are available.


```r
sheet_area("folio")
```

```
## [1] 1350
```

The sheet sizes are calculated in the above example according to the [sheet size table](https://github.com/rOpenGov/bibliographica/blob/master/inst/extdata/sheetsizes.csv). The table can be changed by the user (see the function arguments) but by default the functions use this mapping:


```r
kable(sheet_area()) 
```



|format               |gatherings | width| height|    area|
|:--------------------|:----------|-----:|------:|-------:|
|sheet                |1to        |  60.0|   90.0| 5760.00|
|broadside            |bs         |  60.0|   64.0| 3840.00|
|folio-large          |2long      |  30.0|   53.0| 1749.00|
|folio                |2fo        |  30.0|   45.0| 1350.00|
|folio-small          |2small     |  25.0|   38.0|  950.00|
|quarto-long          |4long      |  27.0|   35.0|  945.00|
|quarto               |4to        |  22.0|   28.0|  616.00|
|quarto-small         |4small     |  17.0|   20.0|  340.00|
|octavo-small         |8small     |  11.0|   17.0|  187.00|
|octavo               |8vo        |  13.0|   19.0|  247.00|
|octavo-long          |8long      |  13.0|   22.0|  286.00|
|duodecimo            |12mo       |  12.5|   19.0|  237.50|
|duodecimo-long       |12long     |  13.0|   20.0|  260.00|
|sextodecimo          |16mo       |  12.0|   15.0|  180.00|
|sextodecimo-long     |16long     |  14.0|   18.0|  252.00|
|octodecimo           |18mo       |  10.0|   16.0|  160.00|
|vigesimo-quarto      |24mo       |   8.5|   12.5|  106.25|
|vigesimo-quarto-long |24long     |   9.5|   14.0|  133.00|
|trigesimo-secundo    |32mo       |   7.0|   12.0|   84.00|
|quadraquinto         |40mo       |   6.0|   11.0|   66.00|
|quadragesimo-octavo  |48mo       |   6.5|   10.0|   65.00|
|sexagesimo-quarto    |64mo       |   5.0|    7.0|   35.00|
|octogentesimo        |80mo       |   4.0|    6.0|   24.00|
|octoginta-quarto     |84mo       |   3.5|    5.0|   17.50|


### Stopwords

Removing [stopwords](http://en.wikipedia.org/wiki/Stop_words) is often necessary in text analysis. The stopwords form multiple categories, such as individual letters, conjugates, special characters, or particular expressions. The definition of a stopword may also depend on a context. 

The following example removes the term "well" and individual letters from the input vector:


```r
remove_terms(c("a", "well", "james", "30 year war"), terms = "well")
```

```
## [1] "a"           NA            "james"       "30 year war"
```

For a full list of stopword and related functions, see the [function documentation](https://github.com/rOpenGov/bibliographica/blob/master/man/). We also provide some [ready-made stopword lists](https://github.com/rOpenGov/bibliographica/tree/master/inst/extdata) that can be easily downloaded in R with the 'read.csv' function. 


## Person names

Several person name lists are available for validation purposes, including also gender information. These can be read with the functions firstnames, lastnames, and notnames (pseudonymes also coming). The name lists are collected by combining manually constructed lists and openly available material from various sources listed in the [respective data folders](https://github.com/rOpenGov/bibliographica/tree/master/inst/extdata/names).


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
##   (C) Leo Lahti, Hege Roivainen, Niko Ilomaki, Mikko Tolonen
##   (rOpenGov 2015-2016).  bibliographica R package URL:
##   http://github.com/ropengov/bibliographica
## 
## A BibTeX entry for LaTeX users is
## 
##   @Misc{,
##     title = {bibliographica: tools for bibliographic analysis},
##     author = {Leo Lahti and Hege Roivainen and Niko Ilomaki and Mikko Tolonen},
##     year = {2015-2016},
##   }
## 
## Many thanks for all contributors! See: http://ropengov.github.io
```

## Related work

The generic tools of this package can be used in combination with packages that provide more specific tools for targeted data collections such as the [ESTC](https://github.com/rOpenGov/estc/) or [Fennica](https://github.com/rOpenGov/fennica/). 


## Session info

This vignette was created with


```r
sessionInfo()
```

```
## R version 3.3.1 (2016-06-21)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 16.04 LTS
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
## [1] rmarkdown_1.0.9016    bibliographica_0.2.30 knitr_1.14           
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.7        magrittr_1.5       munsell_0.4.3     
##  [4] tm_0.6-2           colorspace_1.2-7   R6_2.2.0          
##  [7] highr_0.6          stringr_1.1.0      plyr_1.8.4        
## [10] dplyr_0.5.0        tools_3.3.1        babynames_0.2.1   
## [13] parallel_3.3.1     grid_3.3.1         data.table_1.9.6  
## [16] gtable_0.2.0       genderdata_0.5.0   DBI_0.5-1         
## [19] htmltools_0.3.5    yaml_2.1.13        digest_0.6.10     
## [22] lazyeval_0.2.0     assertthat_0.1     tibble_1.2        
## [25] NLP_0.1-9          tidyr_0.6.0        reshape2_1.4.1    
## [28] ggplot2_2.1.0      formatR_1.4        stringdist_0.9.4.2
## [31] slam_0.1-38        evaluate_0.10      stringi_1.1.2     
## [34] sorvi_0.7.47       gender_0.5.1.9000  scales_0.4.0      
## [37] chron_2.3-47
```
