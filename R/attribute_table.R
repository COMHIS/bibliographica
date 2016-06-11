attribute_table <- function (x) {

  # Identify the different page count types and their positions
  # along the page count sequence, including
  # arabics (3), romans ("xiv"), squarebracketed ([3], [3]-5), dashed
  # (3-5, [3]-5), sheets ("2 sheets"), plates ("plates")
  # NOTE: we allow these types to be overlapping and they are later
  # used to identify the sequence type
  # Initialize attributes vs. positions table
  attributes <- c("arabic", "roman", "squarebracket", "dash", "sheet", "plate")
  pagecount.attributes <- matrix(FALSE, nrow = length(attributes), ncol = length(x))
  rownames(pagecount.attributes) <- attributes

  # ARABIC POSITIONS
  pagecount.attributes["arabic", ] <- position_arabics(x)

  # ROMAN POSITIONS
  pagecount.attributes["roman", ] <- position_romans(x)

  # SQUARE BRACKET POSITIONS
  pagecount.attributes["squarebracket", ] <- position_squarebrackets(x)

  # DASH POSITIONS
  pagecount.attributes["dash", grep("-", x)] <- TRUE

  # SHEET POSITIONS
  pagecount.attributes["sheet", ] <- position_sheets(x)

  # PLATE POSITIONS  
  # Estimate pages for plates 
  # and indicate their positions along the page count sequence
  # Example: "127,[1]p.,plates" 
  pagecount.attributes["plate", ] <- position_plates(x) 

  pagecount.attributes

}
