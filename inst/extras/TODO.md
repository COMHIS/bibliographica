### Technical

 * Specific data format for harmonized bibliographic data?

 * languages: in CERL at least we have "Scottish Gaelic" and "Scottish
   Gaelix" - have to add harmonization for languages. Are these the
   same or not. They come from the official catalog! Check first!
   Mention in documentation that only unique and accepted languages
   are listed and discareded are not shown in the final listing. See
   the mark_languages function. Now the counting of languages does not
   work properly since Untermined ones are ignored.

 * Use tm pkg: removeNumbers; removePuncuations; removeWords;
   replaceWords; stripWhiteSpace; tmTolower; and tau:remove_stopwords;
   stopwords(language = ...)

 * Travis & automated analysis updates on a public (non-github)
   server, for instance Pouta.

 * read_mapping -> Use the fast = TRUE more - at least for slower
   polishing function

 * Move all unit tests to tables that can be linked from the overview
   page. As already done with publicationplaces.Rmd



### Author info

 * [gutenbergr R package](https://cran.rstudio.com/web/packages/gutenbergr/vignettes/intro.html) contains metadata for ~50,000 Gutenberg documents and ~ 16,000 authors (life years, aliases etc) 


### Geography

Test the [tmap](https://cran.r-project.org/web/packages/tmap/vignettes/tmap-nutshell.html) R package

1) [open geocode
   database](http://www.opengeocode.org/download.php#cities) Cities of
   the World näyttää tosi kattavalta, laajempi kuin se aikaisemmin
   lähettämäni ja on puhtaasti open source. Lisäksi noita muita
   sovelluksia on mukavasti.

3) geonames: Geonamesista löysin seuraavan aika hyvin asettuvan johon
   listattuna aika kattavan oloisesti kaikki paikat joissa yli 15k
   asukasta. Tämä voisi olla aika hyvä? Samalla sieltä löytyisi
   kaikenlaista lisätietoa paikkoihin liittyen, maakoodit standardeina
   jne. Eli varmaan sitten myöhemmin jos tehdään myös karttoja joissa
   eri elementtejä niin tästä voisi olla hyötyä?

4) World Cities Database https://www.maxmind.com/en/worldcities
  Includes city, region, country, latitude and longitude and
  Population. A listing of all the cities in the world. This database
  contains duplicate and incorrect entries. It is provided as-is, and
  we are unable to provide support for it. For a cleaner database, try
  GeoNames, but they may lack some cities included in our data. This
  could be useful for initial city-county mappings and place name
  validation however.

Muita Usein mainittuja mäppäysresursseja:
- Open Street Map
- Yahoo's GeoPlanet provides a dataset of all named places on earth,
  including oceans, countries, cities and villages. You can download
  it at http://developer.yahoo.com/geo/geoplanet/data/
- GeoDataSource http://www.geodatasource.com/world-cities-database/free


### Utilities

Data Table: http://rstudio.github.io/DT/

Poudan R-serverille interaktiivisia yhteenvetoja.
- rGoogleViz
- Shiny
- muut R:n interaktiiviset paketit...
- Animaatiot


