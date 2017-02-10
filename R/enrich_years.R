#' @title Enrich Years
#' @description Enrich year info.
#' @param df.preprocessed Preprocessed data.frame
#' @param df.orig Original data.frame
#' @return Augmented data.frame
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df2 <- enrich_years(df)}
#' @keywords utilities
enrich_years <- function(df.preprocessed, df.orig) {

  # needs df.orig. Can we get rid of this? -vv
  
  # LL: this uses publication_interval field from df.orig; if that is
  # processed separately and stored in its own field in
  # df.preprocessed then this can be trivially handled. Note that
  # publication_interval is confusingly mixing (at least) to different
  # types of information: (1) actual years of the publication
  # interval, and (2) the number of publications during the years
  # given in the publication_time (df.orig). These should be
  # identified and separated. Below, df.orig take advantage of the
  # latter case to retrieve more information of the
  # publication_frequencies than would be otherwise available.

  message("Enriching publication years..")

    message("Add publication year")
    # Use from field; if from year not available, then use till year
    df.preprocessed$publication_year <- df.preprocessed$publication_year_from
    inds <- which(is.na(df.preprocessed$publication_year))
    df.preprocessed$publication_year[inds] <- df.preprocessed$publication_year_till[inds]

    message("Add publication decade")
    df.preprocessed$publication_decade <- floor(df.preprocessed$publication_year/10) * 10 # 1790: 1790-1799

    message("Mark potential first editions")
    df.preprocessed$first_edition <- is_first_edition(df.preprocessed)

  message("Enrich publication interval")
  # Based on analysis of a random sample of entries in Fennica,
  # it seems that when interval is of the form "1908" ie a single year
  # the publication interval is in all cases 1908-1908 ie a single year.
  # Hence let us augment the interval based on this if till year is missing.
  if ("publication_interval_from" %in% names(df.preprocessed)) {
    nas <- which(!is.na(df.preprocessed$publication_interval_from) & is.na(df.preprocessed$publication_interval_till))
    df.preprocessed$publication_interval_till[nas] <- df.preprocessed$publication_interval_from[nas]
  }

  message("Enrich publication frequency")
  # publication_interval "1-3" etc. does not refer to years but number of publications
  # within the given years. Augment the data based on this logic.
  if ("publication_frequency_text" %in% names(df.preprocessed)) {
    dfo <- df.orig[df.preprocessed$original_row, ]
    idx <- grep("^[0-9]{1}-[0-9]{1,2}$", gsub("\\.$", "", dfo$publication_interval))
    if (length(idx) > 0) {
      f <- sapply(strsplit(gsub("\\.$", "", dfo$publication_interval[idx]), "-"), function (x) {diff(sort(as.numeric(x)))+1})
      fa <- f/(df.preprocessed$publication_year_till[idx] - df.preprocessed$publication_year_from[idx] + 1)
      i <- is.na(df.preprocessed$publication_frequency_annual[idx])
      df.preprocessed$publication_frequency_annual[idx[i]] <- fa[i]
      df.preprocessed$publication_frequency_text <- publication_frequency_text(df.preprocessed$publication_frequency_text, df.preprocessed$publication_frequency_annual)
    }
  }

  return (df.preprocessed)
}
