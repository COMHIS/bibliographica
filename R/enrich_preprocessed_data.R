#' @title Enrich Data
#' @description Enrich data. 
#' @param data.validated Validated data.frame
#' @param df.orig Original data.frame
#' @return Augmented data.frame
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df2 <- enrich_preprocessed_data(df)}
#' @keywords utilities
enrich_preprocessed_data <- function(data.validated, df.orig) {

  pagecount <- width <- height <- NULL

  # This could be improved - varies in time !
  printrun <- 1000

  df.preprocessed <- data.validated$df.preprocessed
  update.fields   <- data.validated$update.fields
  conversions     <- data.validated$conversions

  # Note the source of page counts. Original MARC data by default.
  df.preprocessed$pagecount_from <- rep("original", nrow(df.preprocessed))

  if ("publication_place" %in% update.fields) {
    tmp <- enrich_geo(df.preprocessed[["publication_place"]])
    df.preprocessed$publication_place <- tmp$place
    df.preprocessed$publication_country <- tmp$country
  }

  if ("publication_geography" %in% update.fields) {
    tmp <- enrich_geo(df.preprocessed[["publication_geography"]])
    df.preprocessed$publication_geography_place <- tmp$place
    df.preprocessed$publication_geography_country <- tmp$country    
  }

  # Always do. New field "author" needed for first edition recognition.
  # This is fast.
  if (any(c("author_name", "author_date") %in% update.fields)) {
    # this seems to take long even with only
    # 100 entries in df.preprocessed? -vv 
    df.preprocessed <- enrich_author(df.preprocessed)
  }

  # Always do. This is fast.
  # Must be done after enrich_author
  df.preprocessed <- enrich_years(df.preprocessed, df.orig)

  if ("publisher" %in% update.fields) {

    message("Self-published docs where author is known but publisher not")
    # Note: also unknown authors are considered as self-publishers
    message("Add a separate self-published field")
    tmp <- self_published(df.preprocessed)
    df.preprocessed$self_published <- tmp$self_published
    df.preprocessed$publisher <- tmp$publisher

  }

  # -------------------------------------------------------------------

  if (any(c("physical_extent", "physical_dimension") %in% update.fields)) {

    # Enrich dimensions before pagecount (some dimensions reclassified)
    df.preprocessed <- enrich_dimensions(df.preprocessed)

    # Enrich pagecount after dimensions
    df.preprocessed <- enrich_pagecount(df.preprocessed)

    message("Add estimated paper consumption")
    # Estimated print run size for paper consumption estimates    
    # Paper consumption in sheets
    # (divide document area by standard sheet area
    sheet.area <- subset(sheet_sizes(), format == "sheet")$area
    df.preprocessed <- mutate(df.preprocessed,
            paper = printrun * (pagecount/2) * (width * height)/sheet.area)

    message("Add estimated print area")
    df.preprocessed <- mutate(df.preprocessed,
            print_area = (pagecount/2) * (width * height)/sheet.area)

  }

  message("Enrichment OK")

  enriched.data <- list(df.preprocessed = df.preprocessed,
                        update.fields = update.fields,
                        conversions = conversions) 

  return (enriched.data)
}
