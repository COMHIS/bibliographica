harmonize_christian <- function (x) {

  x <- str_trim(as.character(x))
  x <- gsub("anno dom.", "A.D", x)
  x <- gsub("an. dom.", "A.D", x)  
  x <- gsub("anno domini", "A.D", x)    
  x <- gsub("a.d.", "A.D", x)
  x <- gsub("b.c.", "B.C", x)
  x <- gsub("b\\.c\\.", "before christian era", x)  
  x <- gsub("before christian era", "B.C", x)  

  x
}