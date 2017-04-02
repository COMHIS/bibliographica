#' @title Retrieve Timeline 
#' @description Timeline data for selected variable (possibly across various groups).
#' @param x data frame
#' @param field Numeric field to summarize in the timeline. The number of entries (title count) per decade is used by default. If this argument is used, the sum of entries per decade for this field is given.
#' @param group Optional. Name for a data field that indicates groups to compare. 
#' @param nmin Include only entries with at least nmin absolute frequency
#' @param mode "absolute" or "relative"
#' @param time.window Time window for the timeline in years. Default: 10 (publication decade).
#' @param time.field Specify the field to be used for time. By default: "publication_year", or if time.window is 10, then "publication_decade" 
#' @return data.frame
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{timeline(df, "gatherings")}
#' @keywords utilities
timeline <- function (x, field = "titlecount", group = NULL, nmin = 0, mode = "absolute", time.window = 10, time.field = "publication_year") {

  publication_decade <- publication_time <- NULL

  x$publication_time <- x[[time.field]]

  # Set the desired time window (default one decade)
  x$publication_time <- time.window * floor(x$publication_time / time.window)

  if (time.field == "publication_decade" || (time.field == "publication_year" & time.window == 10 & "publication_decade" %in% names(x))) {
    x$publication_time <- x$publication_decade
  }

  if (!is.null(group)) {
    x <- x[, c("publication_time", group)]
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
               group_by(publication_time, group) %>%
     	       summarise(absolute = sum(field, na.rm = TRUE))

  # Remove entries with too few occurrences
  df2 <- df2 %>% filter(!is.na(publication_time) &
    group %in% setdiff(unique(as.character(unname(unlist(df2[which(df2$absolute >= nmin), "group"])))), "NA"))
  df2$group <- factor(df2$group)
  df2$group <- droplevels(df2$group)

  # Add relatives
  df3 <- spread(df2, "publication_time", "absolute", fill = 0)
  df3[, -1] = 100 * apply(df3[, -1], 2, function (x) {x/sum(x, na.rm = TRUE)})
  df3 <- melt(as.data.frame(df3), "group")  
  colnames(df3) <- c("group", "publication_time", "relative")
  df3$publication_time <- as.numeric(as.character(df3$publication_time))
  df3 <- df3[, c("publication_time", "group", "relative")]

  # Combine counts and relatives
  dfs <- dplyr::full_join(df2, df3)
  dfs$mode <- dfs[[mode]]

  return(dfs)

}