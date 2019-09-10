#' @title Theme function for COMHIS group
#' @description default theme for consistent plot style within the 
#' COMHIS group. 
#' @param type Type of the data to be plotted ("continuous"; "discrete")
#' @author Abhishekh Gupta \email{abishak.gupta@gmail.com}
#' Usage: source("theme_comhis.R") and append '+ theme_comhis(type)' 
#' to the desired plot

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,'Package'])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages<-c('ggplot2', 'ggthemes')
check.packages(packages)

theme_comhis <- function(type="invalid") {
  if(type %in% c("D","discrete","Discrete","d"))
    list(theme_light(base_size=12, base_family="Arial") +
           theme(axis.ticks.length=unit(0, "cm")),
         scale_fill_gdocs())
  else if(type %in% c("C","continuous","Continuous","c"))
    list(theme_light(base_size=12, base_family="Arial") + 
           theme(axis.ticks.length=unit(0, "cm")),
         scale_fill_viridis_d())
  else
    stop("Invalid input! Specify the type of data, either continuous or discrete ",
         call. = FALSE)
}
