#' @importFrom dplyr count
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr summarize
#' @importFrom dplyr %>%
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 scale_x_log10
#' @importFrom ggplot2 scale_y_log10
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_set
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom sorvi condense_spaces
#' @importFrom sorvi quickdf
#' @importFrom stringr str_trim
#' @importFrom tm stopwords
.onAttach <- function(lib, pkg)
{

  packageStartupMessage("bibliographica - tools for bibliographic analysis\nCopyright (C) 2014-2016 Leo Lahti, Niko Ilomaki, Hege Roivainen and Mikko Tolonen\n\nhttp://github.com/ropengov/bibliographica\n\nHe who refuses to do arithmetic is doomed to talk nonsense.")

}



