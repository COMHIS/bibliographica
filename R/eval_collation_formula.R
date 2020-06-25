# This module implements counting leaves in collation formulas.
# The implementation follows the description at:
#   https://manual.stcv.be/p/Collation_Formula

#' @title Build the signature table
#' @description This builds a list of possible gathering signatures in their
#' sequential ordering. It is necessary to represent this table explicitly when
#' unfolding sequences of gatherings, because the meaning of sequences is far
#' from obvious.
#' @return signature table
#' @author Maciej Janicki \email{maciej.janicki@@helsinki.fi}
#' @keywords internal
build_signature_table <- function() {
  alphabet <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', 'L', 'M',
                'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'V', 'X', 'Y', 'Z')
  symbols <- system.file("extdata/collation.csv", package = "bibliographica")
  prefixes <- c('', 2:50)
  sym_prefixes <- c('', 2:100)
  sig_table <- c(
    paste(rep(prefixes, each=length(alphabet)),
          rep(alphabet, length(prefixes)), sep=''),
    paste(rep(sym_prefixes, length(symbols)),
          rep(symbols, each=length(sym_prefixes)), sep=''))
  sig_table
}

#' @title Build the pattern for recognizing gathering sets
#' @return pattern (a regular expression)
#' @author Maciej Janicki \email{maciej.janicki@@helsinki.fi}
#' @keywords internal
build_pattern <- function() {
  # XXX use the signature table in `gathering`?
  gathering <- '([^`\\s-\\[\\]\\(\\)]+)'
  gathering_set <- paste0('\\[?', gathering, '(-', gathering, ')?', '\\]?')
  number <- '([0-9]+)'
  num_leaves_sup <- paste0(number, '(/', number, ')?')
  num_leaves_all <- paste0('(1|`SUP`', num_leaves_sup, '`LO`)')
  pattern <- paste0(gathering_set, num_leaves_all)
  pattern
}

#' @title Parse collation formula
#' @description internal
#' @param x input char
#' @return vector containing numbers of leaves
#' @author Maciej Janicki \email{maciej.janicki@@helsinki.fi}
#' @keywords internal
parse_collation_formula <- function(x) {
  pattern <- build_pattern()
  # )( and ):( are symbols, which interfere with the other use of brackets
  # - thus replace with different symbols
  x <- stri_replace_all_fixed(x, c(')(', '):('), c('#', ':'), vectorize_all=F)
  x <- gsub('\\([^\\(\\)]*\\)', '', x)   # remove text in brackets
  y <- stri_match_all_regex(x, pattern)
  # The start and end are uppercased, because:
  # - it is not important for them to be unique across sequences
  #   (e.g. A-K`SUP`8`LO` a-d`SUP`6`LO` can be converted to
  #   A-K`SUP`8`LO` A-D`SUP`6`LO` without a problem),
  # - in sequences where the case differs, like A-d`SUP`6`LO`, it seems to be
  #   an error and should be corrected to A-D`SUP`6`LO`,
  y <- lapply(y, function(z) data.frame(
    start = stri_trans_toupper(z[,2]),
    end = stri_trans_toupper(z[,4]),
    n1 = ifelse(z[,5] == "1", 1, z[,6]),
    n2 = z[,8],
    stringsAsFactors=FALSE
  ))
  y
}

#' @title Unfold a gathering set
#' @description internal
#' @param start the signature of first gathering in the set
#' @param end the signature of the last gathering (or NA if non-sequence)
#' @param n1 the number of leaves per gathering
#' @param n2 the second number of leaves in case of alternating numbers (or NA
#'        if not present)
#' @param sig_table the table of admissible gathering signatures and their
#'        sequential ordering (see \code{\link{build_signature_table}})
#' @return a data frame giving the signature and number of leaves of each
#'         gathering
#' @author Maciej Janicki \email{maciej.janicki@@helsinki.fi}
#' @keywords internal
unfold_set <- function(start, end, n1, n2, sig_table) {
  if (is.na(end)) { sig <- start }
  else if (start %in% sig_table && end %in% sig_table) {
    sig <- sig_table[seq(which(sig_table == start), which(sig_table == end))]
  } else {
    return(data.frame(sig = NA, nleaves = NA))
  }
  if (is.na(n2)) { leaves <- as.integer(n1) }
  else { leaves <- as.integer(c(n1, n2)) }
  nleaves <- rep(leaves, length(sig)/length(leaves))
  if (length(nleaves) != length(sig)) {
    return(data.frame(sig = NA, nleaves = NA))
  }
  data.frame(sig = sig, nleaves = nleaves, stringsAsFactors=FALSE)
}

#' @title Unfold gathering sets
#' @description This is a wrapper around \code{\link{unfold_set}} operating on
#'              data frames resulting from parsing multiple formulas.
#' @param x the list of dataframes containing recognized gathering sets (output
#'        of \code{\link{parse_collation_formula}})
#' @return a data frame giving the signatures and numbers of leaves of each
#'         gathering in each formula
#' @author Maciej Janicki \email{maciej.janicki@@helsinki.fi}
#' @keywords internal
unfold_sets <- function(x) {
  sig_table <- build_signature_table()
  lapply(
    x,
    function(z) rbindlist(mapply(unfold_set, z$start, z$end,
                                 z$n1, z$n2, list(sig_table), SIMPLIFY=F)))
}

#' @title Evaluate collation formula
#' @description Evaluates a collation formula to the number of leaves.
#' @param x input char
#' @return vector containing numbers of leaves
#' @author Maciej Janicki \email{maciej.janicki@@helsinki.fi}
#' @references See citation("bibliographica") and explanations in
#'             \url{https://manual.stcv.be/p/Collation_Formula}.
#' @export
#' @keywords utilities
#' @examples eval_collation_formula("n1 A`SUP`8`LO` B`SUP`6`LO`")
eval_collation_formula <- function(x) {
  y <- unlist(lapply(unfold_sets(parse_collation_formula(x)),
                     function(z) sum(z$nleaves)))
  y[y == 0] <- NA
  y
}

