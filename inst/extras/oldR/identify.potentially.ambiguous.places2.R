library(bibliographica)

s <- read_synonymes("../extdata/PublicationPlaceSynonymes.csv", sep = ";", mode = "table", self.match = FALSE)

tab <- rbind(s[grep("ngland", s$synonyme),],
             s[grep("ngland", s$name),],
	     s[unlist(sapply(sapply(strsplit(s[grep("ngland", s$name),"name"], " "), function(x) {x[[1]]}), function (x) {grep(x, s$name)})),],
             s[grep("anglorum", s$synonyme),])

tab <- rbind(tab, s[s$name %in% tab$name,])

tab <- rbind(tab,
	     s[unlist(sapply(sapply(strsplit(tab$name, " "), function(x) {x[[1]]}), function (x) {grep(x, s$name)})),]
    )

tab1 <- unique(tab)

# ---------------------------------------------

# Pick the first part for all names
first <- sapply(strsplit(s$name, " "), function (x) {x[[1]]})
first.accepted <- unique(na.omit(first))
first.accepted <- setdiff(first.accepted, "New")
first.accepted <- first.accepted[nchar(first.accepted) > 2]
first[!first %in% first.accepted] <- NA

# Check where the first part matches to full name ambiguously
spl <- split(s$name, first)
inds <- sapply(spl, function (x) {length(unique(x))}) > 1
spl <- lapply(spl[inds], function (x) {unique(x)})

tab <- s[s$name %in% unname(unlist(spl)),]
tab <- tab[order(tab$name), c("name", "synonyme")]

tab2 <- tab

# -----------------------------------------------------

# Pick the first part for all names that start with "New "
snam <- gsub("New ", "New_", s$name)
first <- sapply(strsplit(snam, " "), function (x) {x[[1]]})
first.accepted <- unique(na.omit(first))
first.accepted <- first.accepted[grep("^New_", first.accepted)]
first.accepted <- first.accepted[nchar(first.accepted) > 2]
first[!first %in% first.accepted] <- NA

# Check where the first part matches to full name ambiguously
spl <- split(snam, first)
inds <- sapply(spl, function (x) {length(unique(x))}) > 1
spl <- lapply(spl[inds], function (x) {unique(x)})

tab <- s[s$name %in% gsub("New_", "New ", unname(unlist(spl))),]
tab <- tab[order(tab$name), c("name", "synonyme")]

tab3 <- tab

# --------------------------------------------------

tab <- unique(rbind(tab1, tab2, tab3))
tab <- tab[order(tab$name), c("name", "synonyme")]

write.table(tab, file = "~/tmp/tmp4.csv", sep = ";", quote = F, row.names = F)






