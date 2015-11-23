attribute_table <- function (x) {

  # Identify the different page count types and their positions
  # along the page count sequence, including
  # arabics (3), romans ("xiv"), squarebracketed ([3], [3]-5), dashed
  #  (3-5, [3]-5), sheets ("2 sheets"), plates ("plates")
  # NOTE: we allow these types to be overlapping and they are later
  # used to identify the sequence type
  # Initialize attributes vs. positions table
  attributes <- c("arabic", "roman", "squarebracket", "dash", "sheet", "plate")

  pagecount.attributes <- matrix(FALSE, nrow = length(attributes), ncol = length(x))
  rownames(pagecount.attributes) <- attributes

  # ARABIC POSITIONS
  arabics <- position_arabics(x)
  pagecount.attributes["arabic", arabics$positions]<- TRUE

  # ROMAN POSITIONS
  romans <- position_romans(x)
  pagecount.attributes["roman", romans$positions]<- TRUE

  # SQUARE BRACKET POSITIONS
  sqb <- position_squarebrackets(x)
  pagecount.attributes["squarebracket", sqb$positions] <- TRUE

  # DASH POSITIONS
  pagecount.attributes["dash", grep("-", x)]<- TRUE

  # SHEET POSITIONS
  sheets <- position_sheets(x)
  pagecount.attributes["sheet", sheets$positions]<- TRUE

  # PLATE POSITIONS  
  # Estimate pages for plates 
  # and indicate their positions along the page count sequence
  # Example: "127,[1]p.,plates" 
  plates <- position_plates(x) #plates$pages; plates$positions; plates$total
  pagecount.attributes["plate", plates$positions] <- TRUE

  pagecount.attributes

}
