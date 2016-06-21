mean.pagecounts.multivol <- mean_pagecounts(filter(df.preprocessed, multivol))
mean.pagecounts.singlevol <- mean_pagecounts(filter(df.preprocessed, singlevol)) 
mean.pagecounts.issue <- mean_pagecounts(filter(df.preprocessed, issue)) 

print("..join..")
mean.pagecounts <- full_join(mean.pagecounts.singlevol, mean.pagecounts.multivol, by = "doc.dimension")
mean.pagecounts <- full_join(mean.pagecounts, mean.pagecounts.issue, by = "doc.dimension")
mean.pagecounts$doc.dimension <- factor(mean.pagecounts$doc.dimension,
			      levels = levels(mean.pagecounts.singlevol$doc.dimension))

# write.table(mean.pagecounts, file = paste(output.folder, "estimated_page_counts.csv", sep = ""),
#  quote = F, row.names = F, sep = ",")
