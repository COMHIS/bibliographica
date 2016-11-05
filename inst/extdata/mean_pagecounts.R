# Calculate mean page counts for different document types

get_mean_pagecounts <- function(df) {

  # Clean up		    
  gc()		    

  message("get_mean_pagecounts single volume")
  # For single-volume documents, use only the ones with >=32 pages to estimate average page counts
  mean.pagecounts.singlevol <- mean_pagecounts(filter(df, singlevol & pagecount >= 32)) 
  colnames(mean.pagecounts.singlevol) <- paste(colnames(mean.pagecounts.singlevol), "singlevol", sep = ".")
  colnames(mean.pagecounts.singlevol) <- gsub("doc.dimension.singlevol", "doc.dimension", colnames(mean.pagecounts.singlevol))

  message("get_mean_pagecounts multivolume")
  mean.pagecounts.multivol <- mean_pagecounts(filter(df, multivol))
  colnames(mean.pagecounts.multivol) <- paste(colnames(mean.pagecounts.multivol), "multivol", sep = ".")
  colnames(mean.pagecounts.multivol) <- gsub("doc.dimension.multivol", "doc.dimension", colnames(mean.pagecounts.multivol))

  message("get_mean_pagecounts issue")
  mean.pagecounts.issue <- mean_pagecounts(filter(df, issue)) 
  colnames(mean.pagecounts.issue) <- paste(colnames(mean.pagecounts.issue), "issue", sep = ".")
  colnames(mean.pagecounts.issue) <- gsub("doc.dimension.issue", "doc.dimension", colnames(mean.pagecounts.issue))

  message("get_mean_pagecounts join")
  mean.pagecounts <- full_join(mean.pagecounts.singlevol, mean.pagecounts.multivol, by = "doc.dimension")
  mean.pagecounts <- full_join(mean.pagecounts, mean.pagecounts.issue, by = "doc.dimension")
  mean.pagecounts$doc.dimension <- factor(mean.pagecounts$doc.dimension,
  			                    levels = levels(mean.pagecounts.singlevol$doc.dimension))

  message("get_mean_pagecounts order")
  mean.pagecounts$doc.dimension <- order_gatherings(mean.pagecounts$doc.dimension)

  message("get_mean_pagecounts arrange into list")  
  mean.pagecounts.results <- list(multivol = mean.pagecounts.multivol,
                                  singlevol = mean.pagecounts.singlevol,
                                  issue = mean.pagecounts.issue)

  return (mean.pagecounts.results)

}
