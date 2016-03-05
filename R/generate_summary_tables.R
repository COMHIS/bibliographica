#' @title Generate summary tables
#' @description Generate summary tables from the preprocessed data frame.
#' @param df.preprocessed Preprocessed data.frame to be summarized
#' @param df.orig Original data.frame for comparisons
#' @param output.folder Output folder path
#' @return NULL
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples # generate_summary_tables(df)
#' @keywords utilities
generate_summary_tables <- function (df.preprocessed, df.orig, output.folder = "output.tables") {
  f <- system.file("inst/extdata/summarize.R", package = "bibliographica")
  source(f)
  NULL
}

