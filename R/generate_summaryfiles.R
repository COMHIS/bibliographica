#' @title Generate Summary Pages
#' @description Generates statistical summaries for preprocessed data.
#' @param df data.frame to be summarized
#' @param df.orig Original unpolished data.frame 
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
generate_summaryfiles <- function (df, df.orig = NULL, author = "Author TBA", output.folder = "output.tables/", ntop = 20, summaries = c("overview", "author", "publicationplace", "publisher", "documents", "size", "gender", "topic", "language", "title", "publicationyear", "pagecount", "dimension")) {

  # Output file paths		      
  outputs <- c()

  # Generate the markdown summaries
  for (id in summaries) {
    this.folder <- getwd()
    outputs[[id]] <- knit(input = system.file(paste("extdata/", id, ".Rmd", sep = ""),
  	       	package = "bibliographica"),
       output = paste(id, ".md", sep = "")) # envir = globalenv()
       gc()
  }

  message("All markdown summaries generated.")

  outputs

}

