  message("Discarded first names")
  write.table(discarded.author.firstnames, file = paste(output.folder, "author_firstnames_discarded.csv", sep = ""), quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  write.table(discarded.author.lastnames, file = paste(output.folder, "author_lastnames_discarded.csv", sep = ""), quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)  

  message("Discarded author")
  tab <- cbind(name = names(discarded.author.table), count = unname(discarded.author.table))
  suppressWarnings(tab <- rbind(c("Total count:", sum(as.numeric(tab[, "count"]))), tab))
  write.table(tab, file = paste(output.folder, "author_discarded.csv", sep = ""), quote = FALSE, sep = "\t", row.names = FALSE)


[Discarded author last names](output.tables/author_lastnames_discarded.csv) The most common ones are listed first. Pseudonymes from here can be added to the list of [known pseudonymes](https://github.com/rOpenGov/bibliographica/tree/master/inst/extdata/names/pseudonymes). Otherwise, those last names that should be accepted can be added to the [custom list](https://github.com/rOpenGov/bibliographica/blob/master/inst/extdata/names/lastnames/custom.csv)

[Discarded author first names](output.tables/author_firstnames_discarded.csv). The most common ones are listed first. Pseudonymes from here can be added to the list of [known pseudonymes](https://github.com/rOpenGov/bibliographica/tree/master/inst/extdata/names/pseudonymes). Otherwise, those first names that should be accepted can be added to the [custom list](https://github.com/rOpenGov/bibliographica/blob/master/inst/extdata/names/firstnames/custom.csv)



Paper consumption

```{r summaryTop10publisherstimelinepaper, fig.height=30, fig.width=10, echo=FALSE, warning=FALSE}
# Calculate paper consumption for publishers
df2 <- df %>%
    group_by(publisher) %>%
    summarize(paper.consumption.km2 = sum(paper.consumption.km2, na.rm = TRUE), n = n()) 
# Pick top-10 paper publishers
o <- order(df2$paper.consumption.km2, decreasing = TRUE)
top10 <- na.omit(df2$publisher[o])[1:10]
dfs <- filter(df, publisher %in% top10)
dfs <- group_by(dfs, publisher, publication_year) %>%
    summarize(paper.consumption.km2 = sum(paper.consumption.km2, na.rm = TRUE), n = n()) 
p <- ggplot(dfs, aes(x = publication_year, y = paper.consumption.km2)) 
p <- p + geom_bar(stat = "identity") 
p <- p + facet_grid(publisher ~ .)
p <- p + ggtitle("Annual paper consumption by top publishers")
print(p)
```


# Identify romans that are not in square brackets _and_ start the 
  # page count sequence (ie. "iii, iv, 2-10, 13-16" -> inds = 1:2)
  roman <- pagecount.attributes["roman", ]
  sqb <- pagecount.attributes["squarebracket", ]
  inds <- which(roman & !sqb)
  if (length(inds) == 1 && inds == 1) {   
  } else if (length(inds) > 1 && inds[[1]] == 1) {   
    inds <- 1:(which.min(diff(inds) == 1) - 1)
  } else {
    inds <- NULL
  }

  pagecount.attributes <- rbind(pagecount.attributes, roman.start = rep(FALSE, ncol(pagecount.attributes)))
  pagecount.attributes["roman.start", inds] <- TRUE


  # Romans at the start are counted separately
  inds <- pagecount.attributes["roman.start",]
  pages$roman.start <- maxrule(x[inds])
  # Set remaining romans FALSE if they are already listed in roman.start
  pagecount.attributes["roman", pagecount.attributes["roman.start",]] <- FALSE

