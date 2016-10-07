
get_mean_pagecounts <- function(df.preprocessed) {

  mean.pagecounts.multivol <- mean_pagecounts(filter(df.preprocessed, multivol))
  colnames(mean.pagecounts.multivol) <- paste(colnames(mean.pagecounts.multivol), "multivol", sep = ".")
  colnames(mean.pagecounts.multivol) = gsub("doc.dimension.multivol", "doc.dimension", colnames(mean.pagecounts.multivol))

  mean.pagecounts.singlevol <- mean_pagecounts(filter(df.preprocessed, singlevol)) 
  colnames(mean.pagecounts.singlevol) <- paste(colnames(mean.pagecounts.singlevol), "singlevol", sep = ".")
  colnames(mean.pagecounts.singlevol) = gsub("doc.dimension.singlevol", "doc.dimension", colnames(mean.pagecounts.singlevol))

  mean.pagecounts.issue <- mean_pagecounts(filter(df.preprocessed, issue)) 
  colnames(mean.pagecounts.issue) <- paste(colnames(mean.pagecounts.issue), "issue", sep = ".")
  colnames(mean.pagecounts.issue) = gsub("doc.dimension.issue", "doc.dimension", colnames(mean.pagecounts.issue))

  mean.pagecounts <- full_join(mean.pagecounts.singlevol, mean.pagecounts.multivol, by = "doc.dimension")
  mean.pagecounts <- full_join(mean.pagecounts, mean.pagecounts.issue, by = "doc.dimension")
  mean.pagecounts$doc.dimension <- factor(mean.pagecounts$doc.dimension,
  			      levels = levels(mean.pagecounts.singlevol$doc.dimension))
  			      
  mean.pagecounts$doc.dimension <- order_gatherings(mean.pagecounts$doc.dimension)

  mean.pagecounts.results <- list(multivol = mean.pagecounts.multivol,
                                  singlevol = mean.pagecounts.singlevol,
                                  issue = mean.pagecounts.issue)

  return (mean.pagecounts.results)
}
