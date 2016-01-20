#' @importFrom dplyr count
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_set
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom sorvi condense_spaces
#' @importFrom stringr str_trim
.onAttach <- function(lib, pkg)
{

  packageStartupMessage("bibliographica - tools for bibliographic analysis\nCopyright (C) 2014-2016 Leo Lahti, Niko Ilomaki, Hege Roivainen and Mikko Tolonen\n\nhttp://github.com/ropengov/bibliographica\n\nHe who refuses to do arithmetic is doomed to talk nonsense.")

}



