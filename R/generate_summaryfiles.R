#' @title Generate summary files
#' @description Generates statistical summaries for the preprocessed data
#' @param df data.frame to be summarized
#' @param author Author name (to printed on the generated documents)
#' @param output.folder Path to the folder with associated summary table
#' @param ntop The number of top findings to show
#' @param summaries A character vector specifying the summaries to be produced.
#' @return A vector of output file paths
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @importFrom knitr knit
#' @examples # generate_summaryfiles()
#' @keywords utilities
generate_summaryfiles <- function (df, author = "Author TBA", output.folder = "output.tables/", ntop = 20, summaries = c("overview", "author", "publicationplace", "publisher", "documents", "size", "gender", "topic")) {

  # Output file paths		      
  outputs <- c()

  # Generate the markdown summaries
  for (id in summaries) {
    outputs[[id]] <- knit(input = system.file(paste("extdata/", id, ".Rmd", sep = ""),
  	       	package = "bibliographica"),
       output = paste(id, ".md", sep = ""), envir = globalenv())
  }

  outputs

}
