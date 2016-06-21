mean.pagecounts.multivol <- mean_pagecounts_multivol(df.preprocessed) 
mean.pagecounts.singlevol <- mean_pagecounts_singlevol(df.preprocessed) 
mean.pagecounts.issue <- mean_pagecounts_issue(df.preprocessed) 

print("..join..")
mean.pagecounts <- full_join(mean.pagecounts.singlevol, mean.pagecounts.multivol, by = "doc.dimension")
mean.pagecounts <- full_join(mean.pagecounts, mean.pagecounts.issue, by = "doc.dimension")
mean.pagecounts$doc.dimension <- factor(mean.pagecounts$doc.dimension,
			      levels = levels(mean.pagecounts.singlevol$doc.dimension))

# write.table(mean.pagecounts, file = paste(output.folder, "estimated_page_counts.csv", sep = ""),
#  quote = F, row.names = F, sep = ",")
