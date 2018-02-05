#' @import babynames
#' @import genderdata
#' @importFrom data.table fread
#' @importFrom dplyr count
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr rbind_all
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr summarize
#' @importFrom dplyr tbl_df
#' @importFrom dplyr %>%
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_x_log10
#' @importFrom ggplot2 scale_y_log10
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_set
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom parallel mclapply
#' @importFrom reshape2 melt
#' @importFrom stats median
#' @importFrom stats na.omit
#' @importFrom stringr str_trim
#' @importFrom stringr str_count
#' @importFrom stringr str_replace
#' @importFrom stringdist amatch
#' @importFrom tibble as_data_frame
#' @importFrom tibble data_frame
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom tidyr separate
#' @importFrom tm stopwords
#' @importFrom utils as.roman
#' @importFrom utils read.csv
#' @importFrom utils tail
#' @importFrom utils write.csv
#' @importFrom utils write.table
.onAttach <- function(lib, pkg)
{

  packageStartupMessage("bibliographica - tools for bibliographic analysis\nCopyright (C) 2014-2018 Computational History Group (COMHIS) - University of Helsinki, Finland \n\nhttp://github.com/COMHIS/bibliographica\n\nHe who refuses to do arithmetic is doomed to talk nonsense.")

}



