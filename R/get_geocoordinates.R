#' @title Get Geocoordinates
#' @description Map geographic places to geocoordinates.
#' @param x A vector of publication place names
#' @param geonames geonames
#' @param places.geonames places.geonames
#' @return data.frame with latitude and longitude
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- get_geocoordinates("Berlin")}
#' @details Experimental.
#' @keywords utilities
get_geocoordinates <- function (x, geonames, places.geonames) {

  places <- asciiname <- country.code <- admin1 <- NULL

  pubplace.orig <- as.character(x)
  pubplace <- pubplace.uniq <- unique(pubplace.orig)

  # Match publication places to geonames
  # First match all based on asciiname, then name, only then synonymes.
  # Otherwise too many mismatches with identical city names from different
  # continents
  # Download geonames data (city coordinates etc)
  # geonames <- get_geonames("cities1000", tempdir())
  # save(geonames, file = "geonames.RData")
  # places <- sort(as.character(unique(pubplace)))
  # @importFrom estc match_geonames
  # if (!length(places.geonames) == length(places)) {
  #   places.geonames <- estc::match_geonames(places, geonames)
  #   save(places.geonames, file = "places.geonames.RData")
  # }

  # print("Match to geonames")
  geocoordinates <- geonames[match(places.geonames, geonames$asciiname), ]
  geocoordinates$latitude <- as.numeric(as.character(geocoordinates$latitude))
  geocoordinates$longitude <- as.numeric(as.character(geocoordinates$longitude))
  rownames(geocoordinates) <- places

  # TODO investigate opportunities to have identifiers for all major cities
  # Fill in manually the largest publication places that could not be uniquely matched to a location
  # and are hence still missing the coordinates
  #top <- rev(sort(table(subset(df, publication_place %in% names(which(is.na(places.geonames)))$publication_place)))

  message("Manual matching")
  place <- "London"
  f <- filter(geonames, asciiname == place & country.code == "GB")
  geocoordinates[place,] <- f

  place <- "Edinburgh"
  f <- geonames[intersect(grep("Edinburgh", geonames$alternatenames), which(geonames$country.code == "GB")),]
  geocoordinates[place,] <- f

  place <- "Dublin"
  f <- filter(geonames, asciiname == place & country.code == "IE")	
  geocoordinates[place,] <- f

  place <- "Philadelphia Pa"
  f <- filter(geonames, asciiname == "Philadelphia" & admin1 == "PA")
  geocoordinates[place,] <- f

  place <- "Boston"
  f <- filter(geonames, asciiname == place & country.code == "GB")
  geocoordinates[place,] <- f

  # TODO geonames matching might be improved if country code would be included
  # in the matching in the first place and not afterwards manually - to be implemented?
  place <- "Boston Ma"
  f <- filter(geonames, asciiname == "Boston" & country.code == "US")
  geocoordinates[place,] <- f

  # TODO Many options - in general check all with Mikko
  place <- "Oxford"
  f <- filter(geonames, asciiname == place & admin1 == "NY")
  geocoordinates[place,] <- f

  place <- "New York N.Y"
  f <- geonames[intersect(grep("New York", geonames$alternatenames), which(geonames$feature.code == "PPL")),]
  geocoordinates[place,] <- f

  place <- "York"
  f <- geonames[intersect(grep("York", geonames$alternatenames), which(geonames$country.code == "GB" & geonames$admin2 == "H3")),]
  geocoordinates[place, ] <- f

  place <- "Glasgow"
  f <- geonames[intersect(grep("Glasgow", geonames$alternatenames), which(geonames$country.code == "GB" & geonames$admin1 == "ENG")),]
  geocoordinates[place,] <- f

  place <- "York"
  f <- geonames[intersect(grep("York", geonames$alternatenames), which(geonames$country.code == "GB" & geonames$admin2 == "H3")),]
  geocoordinates[place,] <- f

  place <- "Cambridge"
  f <- filter(geonames, asciiname == place & country.code == "GB")
  geocoordinates[place,] <- f

  # Checked manually
  place <- "Providence R.I"
  geocoordinates[place, ] <- rep(NA, ncol(geocoordinates))
  geocoordinates[place, c("latitude", "longitude")] <- c(41.8384163, -71.4256989)

  place <- "Hartford Ct"
  f <- geonames[intersect(grep("Hartford", geonames$alternatenames), which(geonames$admin1 == "MA")),]
  geocoordinates[place,] <- f

  place <- "Amsterdam"
  f <- filter(geonames, asciiname == place & country.code == "NL")
  geocoordinates[place,] <- f

  place <- "Bristol"
  f <- filter(geonames, asciiname == place & country.code == "GB")
  geocoordinates[place,] <- f

  place <- "Norwich"
  f <- filter(geonames, asciiname == place & country.code == "GB")
  geocoordinates[place,] <- f

  place <- "Newcastle"
  f <- filter(geonames, asciiname == place & country.code == "GB")
  geocoordinates[place,] <- f

  place <- "Aberdeen"
  f <- filter(geonames, asciiname == place & country.code == "GB")
  geocoordinates[place,] <- f

  place <- "Watertown Ma"
  f <- filter(geonames, asciiname == "Watertown" & admin1 == "MA")
  geocoordinates[place,] <- f

  place <- "Paris"	 
  f <- filter(geonames, asciiname == place & country.code == "FR")
  geocoordinates[place,] <- f

  place <- "Baltimore Md" 
  f <- filter(geonames, asciiname == "Baltimore")
  geocoordinates[place,] <- f

  message("Read custom mappings from file")
  # FIXME integrate all into a single place - country - geocoordinates file
  # that will be used in place - country and place - coordinate mappings
  # systematically
  f <- system.file("extdata/geocoordinates.csv", package = "bibliographica")
  geotab <- read.csv(f, sep = "\t")
  rownames(geotab) <- geotab$place
  coms <- intersect(geotab$place, rownames(geocoordinates))
  geocoordinates[coms, c("latitude", "longitude")] <- geotab[coms, c("latitude", "longitude")]

  # print("FIXME move to tidy data principles ie. geographic info are in their own data frames..")
  latitude <- as.numeric(as.character(geocoordinates[as.character(pubplace), "latitude"]))
  longitude <- as.numeric(as.character(geocoordinates[as.character(pubplace), "longitude"]))

  skip <- T
  if (!skip) {

  # Places with missing geocoordinates
  absent <- rownames(geocoordinates[is.na(geocoordinates$latitude), ])
  missing <- sort(unique(rownames(geocoordinates[is.na(geocoordinates$latitude), ])))

  # print("List all potential hits in geonames")
  hits <- list()
  if (length(missing) > 0) {
    for (place in missing) {
      # print(place)
      inds <- unique(c(grep(place, geonames$name), grep(place, geonames$asiiname), grep(place, geonames$alternatenames)))

      # Cambridge Ma -> Cambridge
      spl <- unlist(strsplit(place, " "))
      spl <- spl[-length(spl)]
      place2 <- paste(spl, collapse = " ")
      inds2 <- unique(c(grep(place2, geonames$name), grep(place2, geonames$asiiname), grep(place2, geonames$alternatenames)))
      inds <- unique(c(inds, inds2))

      hits[[place]] <- geonames[inds,]
    }
  }

  message("Places with no hit whatsoever in geonames")
  absent <- NULL 
  if (length(hits) > 0) {
    absent <- names(which(sapply(hits, function (x) {nrow(x) == 0})))
  }

  }

  tmpdf <- quickdf(list(latitude = latitude, longitude = longitude))

  # Now for missing geocoordinates try further custom data
  nainds <- is.na(tmpdf$latitude) | is.na(tmpdf$longitude)
  missing.geoc <- pubplace.uniq[nainds]

  # Unpolished code showing how the missing coords were retrieved from osm
  #gctmp <- NULL
  #library(gisfin)
  #for (place in missing.geoc) {
  #  print(place)
  #  a <- try(get_geocode(paste("&city=", place, sep = ""), service="openstreetmap", raw_query=T))
  # if (class(a) == "try-error") {a <- list(lat = NA, lon = NA)}; 
  #  gctmp <- rbind(gctmp, c(lat = a$lat, lon = a$lon))
  #}
  #gctmp <- as.data.frame(gctmp)
  #gctmp$publication_place <- missing.geoc
  #saveRDS(gctmp, file = "geoc_Kungliga.Rds")
  #else {
  f2 <- system.file("extdata/geoc_Kungliga.Rds", package = "bibliographica")
  f3 <- system.file("extdata/geoc_Finland.Rds", package = "bibliographica")    
  f2r <- readRDS(f2)
  f3r <- readRDS(f2)
  gctmp <- unique(bind_rows(f2r, f3r))
  tmpdf$latitude[nainds]  <- gctmp$lat[match(missing.geoc, gctmp$publication_place)]
  tmpdf$longitude[nainds] <- gctmp$lon[match(missing.geoc, gctmp$publication_place)]

  # Map back to the original domain
  return(tmpdf[match(pubplace.orig, pubplace.uniq),])

}

