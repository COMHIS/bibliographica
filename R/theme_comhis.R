#' @title Theme (ggplot) for Helsinki Computational History group
#' @description Default ggplot theme of Helsinki Computational History group
#' @param type Plot type ("continuous"; "discrete").
#' @param base_size Base size for fonts.
#' @param family Font family.
#' @author Abhishekh Gupta, Ville Vaara, Leo Lahti.
#' @export
#' @examples
#'   
#'   #library(ggplot2)
#'   #data(iris)
#'   #p <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
#'   #  geom_point()
#'   #p + theme_comhis("continuous")
#'
theme_comhis <- function(type="invalid", base_size=20, family="Helvetica") {

  packages <- c('ggplot2', 'ggthemes')
  check.packages(packages)

  if(type %in% c("D","discrete","Discrete","d")) {

    list(theme_light(base_size=base_size, base_family=family) +
           theme(axis.ticks.length=unit(0, "cm")),
         scale_fill_gdocs(),
         scale_color_gdocs()
	 )

  } else if(type %in% c("C","continuous","Continuous","c")) {
  
    list(theme_light(base_size=base_size, base_family=family) + 
           theme(#axis.ticks.length=unit(0, "cm"),
	         # panel.border = element_blank(),
		 panel.grid.major = element_blank(), 
            	 #panel.grid.minor = element_blank(),
		 axis.line = element_line(colour = "darkgray", size = rel(1)),
		 #legend.key = element_blank()
		 ),
         scale_fill_viridis_d()
	 )
	 
  } else {
    stop("Invalid input! Specify the type of data, 
          either continuous or discrete ",
         call. = FALSE)
  }
}

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,'Package'])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
