#' @title Enrich Dimension Field
#' @description Augment dimensions.
#' @param df Preprocessed data.frame
#' @return Augmented data.frame
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df2 <- enrich_dimensions(df)}
#' @keywords utilities
enrich_dimensions <- function(df) {

  dim.info <- dimension_table()
  sheet.dim.tab <- sheet_area()

  message("Enriching dimensions..")
  gatherings <- obl <- width <- height <- NULL

  # ------------------------------------------------------------

  # Initialize
  df$gatherings <- df$gatherings.original
  df$width <- df$width.original
  df$height <- df$height.original
  df$obl <- df$obl.original      

  # ---------------------------------------------------------------

  # Reclassify some 1to documents to 2fo etc.
  df <- fix_gatherings(df, sheet.dim.tab)

  # ---------------------------------------------------------------

  # Estimating missing dimension info.. first pick dims as a separate table
  dim.orig <- df[, c("gatherings", "width", "height", "obl")]

  # Estimating missing dimension info based on averages from original data
  # TODO the dim.info provides ready-made estimates. We could
  # instead use dim.estimates that is calculated straight from the original data.
  # dim.estimates <- get_dimension_estimates(dim.orig)
  
  # -------------------------------------------------------------

  # Augment missing dimension info
  dim.estimated <- augment_dimension_table(dim.orig, verbose = FALSE, dim.info = dim.info, sheet.dim.tab = sheet.dim.tab)

  # -----------------------------------------------------------------------

  # Replace the current dimension information with the updated estimates

  # Remove earlier versions of these fields
  if (any(names(dim.estimated) %in% names(df))) {
    inds2 <- which(names(df) %in% names(dim.estimated))
    if (length(inds2)>0) {
      df <- df[, -inds2]
    }
  }
  # Merge
  df <- cbind(df, dim.estimated)  

  # -----------------------------------------------------------------------

  message("Dimensions enriched.")
  
  return (df)

}




get_dimension_estimates <- function (dim.orig) {

  width <- height <- obl <- gatherings <- NULL

  # Mean dimensions for each gatherings
  dim.estimates <- dim.orig %>%
     group_by(gatherings, obl) %>%
     summarize(
       mean.width = mean(width, na.rm = TRUE),
       mean.height = mean(height, na.rm = TRUE),
       n = n()
     )
  dim.estimates.orig <- dim.estimates

  # For long with NA, use the standard to (12long -> 12to)
  long <- unique(dim.estimates$gatherings[grep("long", dim.estimates$gatherings)])
  for (g in long) {

    wlong <- filter(dim.estimates, gatherings == g)$mean.width
    hlong <- filter(dim.estimates, gatherings == g)$mean.height 
    if (length(wlong) == 0) {wlong <- NA}
    if (length(hlong) == 0) {hlong <- NA}  

    gnum <- gsub("long", "", g)
    ind <- grep(paste("^", gnum, ".o$", sep = ""), unique(dim.estimates$gatherings))
    gstandard <- as.character(unique(dim.estimates$gatherings)[[ind]])
    ind2 <- which(dim.estimates$gatherings == g & dim.estimates$obl == FALSE)

    if (is.na(wlong)) {
      wlong <- filter(dim.estimates, gatherings == gstandard & obl == FALSE)$mean.width  
      dim.estimates[ind2, "mean.width"] <- wlong    
    }

    if (is.na(hlong)) {
      hlong <- filter(dim.estimates, gatherings == gstandard & obl == FALSE)$mean.height
      dim.estimates[ind2, "mean.height"] <- hlong    
    }

  }

  # For obl with NA, use the reverse of non-obl
  for (g in unique(dim.estimates$gatherings)) {

    wobl <- filter(dim.estimates, gatherings == g & obl == TRUE)$mean.width
    hobl <- filter(dim.estimates, gatherings == g & obl == TRUE)$mean.height 

    if (length(wobl) == 0) {wobl <- NA}
    if (length(hobl) == 0) {hobl <- NA}  

    h <- filter(dim.estimates, gatherings == g & obl == FALSE)$mean.height
    if (is.na(wobl) && length(h) > 0) {
      wobl <- h
      inds <- which(dim.estimates$gatherings == g & dim.estimates$obl == TRUE)
      dim.estimates[inds, "mean.width"] <- wobl    
    }

    w <- filter(dim.estimates, gatherings == g & obl == FALSE)$mean.width
    if (is.na(hobl) && length(w) > 0) {  
      hobl <- w
      inds <- which(dim.estimates$gatherings == g & dim.estimates$obl == TRUE)
      dim.estimates[inds, "mean.height"] <- hobl    
    }
  }

  dim.estimates

}



fix_gatherings <- function (df, sheet.dim.tab) {

  width <- height <- obl <- gatherings <- NULL

  # Change all 1to docs with >2 pages into 2fo category

  # 1to pitäisi aina olla tasan 2 sivua.
  # Eli yksi sheet, broadside tai 1to..,
  # mutta siinä on aina yksi lehti (ja siten kaksi sivua).
  # Näin ollen kaikki merkinnät joissa >2 sivua voisi siirtää 2fo kategoriaan.
  df[which(df$gatherings == "1to" & df$pagecount.orig > 2), "gatherings"] <- "2fo"

  # ----------------------------------------------------------------------------

  # Reclassify some 1to documents to 2fo according to dimension information

  # 1to are sometimes 2fo in reality and this could be inferred from
  # height. Therefore, reclassify all 1to documents that have 2fo
  # height (or width) into 2fo gatherings
  inds <- which(df$gatherings == "1to" & df$obl == 0)

  if (length(inds)) {

    # Check document height similarity 1to and 2fo formats
    dh <- cbind(
      dist.1to = abs(df[inds,"height"] - subset(sheet.dim.tab, gatherings == "1to")$height),
      dist.2fo = abs(df[inds,"height"] - subset(sheet.dim.tab, gatherings == "2fo")$height)
    )
    # Check document width similarity 1to and 2fo formats
    dw <- cbind(
      dist.1to = abs(df[inds,"width"] - subset(sheet.dim.tab, gatherings == "1to")$width),
      dist.2fo = abs(df[inds,"width"] - subset(sheet.dim.tab, gatherings == "2fo")$width)
    )
    # The height of these documents (indices) is closer to 2fo than 1to
    inds.2fo.height <- inds[c(which(apply(dh, 1, which.min) == 2), which(is.na(dh)))]
    # The width of these documents (indices) is closer to 2fo than 1to  
    inds.2fo.width <- inds[c(which(apply(dw, 1, which.min) == 2), which(is.na(dw)))]

    # Change the gatherings for those docs where width and height info 
    # consistently indicate 2fo format
    inds.2fo <- na.omit(intersect(inds.2fo.width, inds.2fo.height))

    if (length(inds.2fo)>0) {
      df[inds.2fo, "gatherings"] <- "2fo"
    }
 
    inds2 <- na.omit(setdiff(inds, inds.2fo))
    if (length(inds2)>0) {
      # Where height and width give inconsistent results, mark NA gatherings
      df[inds2, "gatherings"] <- NA
    }

  }


  # Same for obl documents (converted width and height)
  inds <- which(df$gatherings == "1to" & df$obl == 1)

  if (length(inds)) {
  # Check document height similarity 1to and 2fo formats
  dh <- cbind(
    dist.1to = abs(df[inds,"width"] - subset(sheet.dim.tab, gatherings == "1to")$height),
    dist.2fo = abs(df[inds,"width"] - subset(sheet.dim.tab, gatherings == "2fo")$height)
  )
  # Check document width similarity 1to and 2fo formats
  dw <- cbind(
    dist.1to = abs(df[inds,"height"] - subset(sheet.dim.tab, gatherings == "1to")$width),
    dist.2fo = abs(df[inds,"height"] - subset(sheet.dim.tab, gatherings == "2fo")$width)
  )
  # The height of these documents (indices) is closer to 2fo than 1to
  inds.2fo.height <- inds[c(which(apply(dh, 1, which.min) == 2), which(is.na(dh)))]
  # The width of these documents (indices) is closer to 2fo than 1to  
  inds.2fo.width <- inds[c(which(apply(dw, 1, which.min) == 2), which(is.na(dw)))]

  # Change the gatherings for those docs where width and height info 
  # consistently indicate 2fo format
  inds.2fo <- na.omit(intersect(inds.2fo.width, inds.2fo.height))

    if (length(inds.2fo)>0) {
      df[inds.2fo, "gatherings"] <- "2fo"
    }

    inds2 <- na.omit(setdiff(inds, inds.2fo))
    if (length(inds2)>0) {
      # Where height and width give inconsistent results, mark NA gatherings
      df[inds2, "gatherings"] <- NA
    }
  }

  # Return
  df

}