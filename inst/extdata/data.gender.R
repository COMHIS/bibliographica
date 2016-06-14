# This script combines name-gender mappings from multiple sources

# TODO add later dictionary information
library(dplyr)
library(bibliographica)
source("gender_map.R")

gendermap <- gender_map(dictionaries = "English")
write.table(gendermap, file = "gendermap_english.csv", quote = F, row.names = F, sep = "\t", fileEncoding = "UTF-8")

gendermap <- gender_map(dictionaries = c("Finnish", "Swedish"))
write.table(gendermap, file = "gendermap_finnish_swedish.csv", quote = F, row.names = F, sep = "\t", fileEncoding = "UTF-8")

gendermap <- gender_map()
write.table(gendermap, file = "gendermap.csv", quote = F, row.names = F, sep = "\t", fileEncoding = "UTF-8")





