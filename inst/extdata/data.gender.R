# TODO add later dictionary information
library(dplyr)
source("gender_map.R")
gendermap <- gender_map()
gendermap$name <- iconv(as.character(gendermap$name), from = "latin1", to = "UTF-8")
save(gendermap, file = "../../data/gendermap.rda", compress = "xz")


