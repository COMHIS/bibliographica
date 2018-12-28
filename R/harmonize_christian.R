harmonize_christian <- function (x) {

  x <- str_trim(as.character(x))
  
  #x <- gsub("anno dom.", "A.D", x)
  #x <- gsub("an. dom.", "A.D", x)  
  #x <- gsub("anno domini", "A.D", x)    
  #x <- gsub("a.d.", "A.D", x)
  #x <- gsub("A\\.D", "", x) # redundant

  x <- gsub("anno dom\\.", " ", x)
  x <- gsub("an\\. dom\\.", " ", x)  
  x <- gsub("anno domini", " ", x)    
  x <- gsub("a\\.d\\.", " ", x)
  x <- gsub("ad", " ", x)  
  x <- gsub("A\\.D", " ", x) # redundant
  x <- gsub("anno d⁻ni", " ", x)
  x <- gsub("anno dñi", " ", x)
  x <- gsub("domini", " ", x)   

  x <- gsub("bc", "B.C", x)
  x <- gsub("b\\.c\\.", "B.C", x)  
  x <- gsub("b\\.c\\.", "before christian era", x)  
  x <- gsub("before christian era", "B.C", x)

  # Remove space
  x <- gsub(" B\\.C", "B.C", x)
  x <- condense_spaces(x)

  x
}