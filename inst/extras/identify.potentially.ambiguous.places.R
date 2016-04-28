library(bibliographica)
s <- read_synonymes("PublicationPlaceSynonymes.csv", sep = ";", mode = "table", self.match = FALSE)

tab <- rbind(s[grep("ngland", s$synonyme),],
             s[grep("ngland", s$name),],
	     s[unlist(sapply(sapply(strsplit(s[grep("ngland", s$name),"name"], " "), function(x) {x[[1]]}), function (x) {grep(x, s$name)})),],
             s[grep("anglorum", s$synonyme),])

tab <- rbind(tab, s[s$name %in% tab$name,])

tab <- rbind(tab,
	     s[unlist(sapply(sapply(strsplit(tab$name, " "), function(x) {x[[1]]}), function (x) {grep(x, s$name)})),]
    )

tab <- tab[order(tab$name), c("name", "synonyme")]

tab <- unique(tab)

write.table(tab, file = "~/tmp/tmp.csv", sep = ";", quote = F, row.names = F)






