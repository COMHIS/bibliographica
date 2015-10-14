#' Pick gender based on first names
#'
#' @param x Vector of first names
#' @return Author gender information
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples get_gender("julius")
#' @keywords utilities
#' @import gender
get_gender <- function (x) {

  if (is.character(x) && length(x) == 1) {
    author.gender <- gender(x)
    return(author.gender$gender)
  }

  tmp <- strsplit(str_trim(sapply(x, function (x) {if (length(x) < 2) {x} else {x[[2]]}})), " ")
  tmp[sapply(tmp, length) == 0] <- NA
  first.names <- sapply(tmp, function (x) {x[[1]]})
  first.names.unique <- unique(na.omit(first.names))

  # gender package
  g <- gender(first.names.unique)

  # Convert to a named vector
  gen <- sapply(g, function (x) {x$gender})
  names(gen) <- sapply(g, function (x) {x$name})
  g1 <- gen

  # Gender name synonymes
  fn <- system.file("extdata/harmonize_gender.csv", package = "bibliographica")
  sn <- read.csv(fn, sep = ";")

  # name lists 
  f <- firstnames()
  gen <- f$gender
  gen <- as.character(suppressWarnings(harmonize_names(gen, synonymes = sn)$name))
  names(gen) <- tolower(f$name)
  g2 <- gen

  # Custom gender mappings for ambiguous cases
  custom <- read.csv(system.file("extdata/names/firstnames/gender.csv", 
  	    			package = "bibliographica"), sep = "\t")
  # Also add NA gender for individual letters
  custom <- rbind(custom, cbind(Name = letters, Gender = NA))
  g3 <- custom$Gender
  names(g3) <- custom$Name
  custom <- g3

  g <- data.frame(list(name = first.names.unique, 
       		       list1 = g1[first.names.unique],
       		       list2 = g2[first.names.unique]
		       ))
  rownames(g) <- g$name

  # Handle list1 as primary
  g$gender <- g$list1

  # Fill in remaining entries from list2
  inds <- is.na(g$gender)
  g$gender[inds] <- g$list2[inds]

  # Custom gender mappings for known ambiguous cases
  g[names(custom), "gender"] <- custom

  # Final mapping
  author.gender <- g[first.names, "gender"]
  names(author.gender) <- first.names

  # List unknown or ambiguous gender mapping
  u <- names(author.gender[!is.na(names(author.gender)) & is.na(author.gender)])
  unknown <- sort(table(u))
  unknown <- rev(unknown[!names(unknown) %in% rownames(custom)]  )
  unknown <- cbind(Name = names(unknown), Count = unknown)

  list(gender = author.gender, unknown = unknown)

}

