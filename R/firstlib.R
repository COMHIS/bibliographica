#' @import babynames
#' @importFrom data.table fread
#' @importFrom data.table rbindlist
#' @importFrom dplyr arrange
#' @importFrom dplyr count
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr summarize
#' @importFrom dplyr tally
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
#' @importFrom ggplot2 labs
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
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom stringi stri_match_all_regex
#' @importFrom stringi stri_trans_toupper
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_trim
#' @importFrom stringr str_count
#' @importFrom stringr str_replace
#' @importFrom stringr str_to_title
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

  packageStartupMessage("bibliographica - tools for bibliographic analysis\nCopyright (C) 2014-2020 Helsinki Computational History Group (COMHIS), Finland \n\nhttp://github.com/COMHIS/bibliographica\n\nHe who refuses to do arithmetic is doomed to talk nonsense.")

}



