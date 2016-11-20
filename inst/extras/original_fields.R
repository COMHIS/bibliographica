

catalogs <- list()
catalogs$ESTC <- c("100a","100d","240n","245a","260a","260b","260c","300a","300c","310a","362a","650a","650y,651y","650z,651a,651z")
catalogs$Fennica <- c("041a","041h","100a","100d","240a","245a","245b","260a","260b","260c","300a","300b","300c","300e","310a","362a","500a","502a","502c","502d","510a","510c","650a","651a","710a","720a","785t","852a")
catalogs$Kungliga <- c("008lang","100a","100d","110a","240a","245a","245b","245c","260a","260b","260c","260e","260f","300a","300b","300c","300e","310a","362a","440a","440v","500a","502a","502c","502d","510a","510c","650a","650x","650y","650z","651a","700a","700d","710a","720a","740a","772c","772d","772t","785t","852a","852j","852z","866x","900a","900d","900u","976a","976b")


rets <- list()
for (cat in names(catalogs)) {

  x <- catalogs[[cat]]

  # Get printing terms from a table
  f <- system.file("extdata/fieldnames.csv", package = "bibliographica")
  map <- read.csv(f, sep = "\t")

  ret <- map[match(x, map$field), ]
  rets[[cat]] <- ret

  write.table(ret, file = paste("~/tmp/", cat, "_fields.csv", sep = ""),
  		   quote = F, row.names = F, sep = "\t")

}