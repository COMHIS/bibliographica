#' @title Timeline Plot
#' @description Timeline for selected variable (possibly across various groups).
#' @param x data frame
#' @param field Numeric field to summarize in the timeline. The number of entries (title count) per decade is shown by default, But if this argument is used, the sum of entries per decade for this field is given.
#' @param group Optional. Name for a data field that indicates groups to compare. If given, the different groups are indicated by lines.
#' @param nmin Include only entries with at least nmin absolute frequency
#' @param mode "absolute" or "relative" 
#' @return List:
#' \itemize{
#'   \item{plot}{ggplot object}
#'   \item{table}{summary table}
#' }
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{timeline(df, "gatherings")}
#' @keywords utilities
timeline <- function (x, field = "titlecount", group = NULL, nmin = 0, mode = "absolute") {

  publication_decade <- NULL

  if (!is.null(group)) {
    x <- x[, c("publication_decade", group)]
    x$group <- x[[group]]
  } else {
    x$group <- rep(1, nrow(x))
  }

  if (is.null(field)) {
    field <- "titlecount"
  }
  if (field == "titlecount" && !field %in% names(x)) {
    x[[field]] <- rep(1, nrow(x))
  }
  
  x$field <- x[[field]]
  
  df2 <- x %>% filter(!is.na(group)) %>%
               group_by(publication_decade, group) %>%
     	       summarise(absolute = sum(field, na.rm = TRUE))

  # Remove entries with too few occurrences
  df2 <- df2 %>% filter(!is.na(publication_decade) &
    group %in% setdiff(unique(as.character(unname(unlist(df2[which(df2$absolute >= nmin), "group"])))), "NA"))
  df2$group <- factor(df2$group)
  df2$group <- droplevels(df2$group)

  # Add relatives
  df3 <- spread(df2, "publication_decade", "absolute", fill = 0)
  df3[, -1] = 100 * apply(df3[, -1], 2, function (x) {x/sum(x, na.rm = TRUE)})
  df3 <- melt(as.data.frame(df3), "group")  
  colnames(df3) <- c("group", "publication_decade", "relative")
  df3$publication_decade <- as.numeric(as.character(df3$publication_decade))
  df3 <- df3[, c("publication_decade", "group", "relative")]

  # Combine counts and relatives
  dfs <- dplyr::full_join(df2, df3)
  dfs$mode <- dfs[[mode]]

  if (length(unique(dfs$group))>1) {
    p <- ggplot(dfs, aes(y = mode, x = publication_decade,
       		       shape = group, linetype = group)) +
     geom_point(size = 4) +
     geom_line(aes(color = group), size = 1) +               
     guides(linetype = guide_legend(keywidth = 5), shape = guide_legend(keywidth = 5)) 

  } else {
    p <- ggplot(dfs, aes(y = mode, x = publication_decade)) + geom_bar(stat = "identity")
  }

   p <- p + ylab(paste(field, " (", mode, ")", sep = "")) +
            xlab("Publication decade") +
	    ggtitle(paste("Timeline for ", field, sep = "")) 

   list(plot = p, table = dfs)

}