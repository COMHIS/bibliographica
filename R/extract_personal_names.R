
extract_personal_names  <- function(x) {

  # To avoid warnings in cran build/check these variables must be named within
  # this function before referring to them
  str_extract <- str_count <- str_replace <- NULL


  #if (length(grep("[[:upper:]][[:lower:]]+ [[:upper:]][[:lower:]]+", x)) > 0) {
    full_name = str_extract(x, "((([[:upper:]][[:lower:]]+) |([[:upper:]][.] ?)))+[[:upper:]][[:lower:]]+")
    number_of_given_names = (str_count(full_name, "[[:upper:]]") - 1)
    given_names_pattern <- paste("((([[:upper:]][[:lower:]]+) |([[:upper:]][.] ?))){", (number_of_given_names), "}", sep = "")
    given_names = str_extract(full_name, given_names_pattern)
    initials = gsub("[[:lower:]]+ ", ".", given_names)
    family_name = str_replace(full_name, given_names, "")
    guessed <- (given_names != initials)
    
    initials = gsub(" ", "", initials)
    family_name = gsub(" ", "", family_name)
    
    init_name = as.character(paste(initials, family_name, sep=" "))
    full_name_with_initials <- str_replace(x, full_name, init_name)
    
    for (i in nrow(x)) {
      if (is.na(initials[i])) {guessed[i] <- NA}
    }
  #}
    return (data.frame(initials=initials, family=family_name, full_name=full_name, init_name=full_name_with_initials, guessed=guessed))
    
  
}

# Case: "Hege Raivoinen" vs. c("H. Vaivainen", "H. Taivainen", "H.H.M. Raivoinen", "H. Raivoinen itse")
# 1) "Hege Raivoinen" -> "H. Raivoinen"
# 2) Karsitaan mahdolliset täsmättävät: ainoastaan "H. Raivoinen itse" sisältää samat alkukirjaimet ja saman sukunimen (=alkukirjainten jälkeinen kapiteelisana), joten ainoastaan sitä vasten verrataan
# 3) tehdään sumea täsmäys vasta nyt soveliaalla stringedit-systeemillä "H. Raivoinen" vs. "H. Raivoinen itse"
# 4) Huomataan ettei mitään voi yhdistää, ellei 'itseä' ole poistettu tarkasteltavasta arvosta



# Automatisointiehdotus aika huomioonottaen:
#  Jos halutaan varmistua siitä, ettei ajankohdat mene pahasti päällekkäin, voidaan ottaa esim. yleisimmän kirjoitusasun mukaisista julkaisuista (Kaikki 400 julkaisua nimellä "H. Raivoinen") painovuodet ja lisätä maksimitoiminta-aika vaikka 40-60 vuodeksi (kustantajien keskimääräinen elinikä?).

# Case "H. Raivoinen"
# 1) julkaisuvuodet 1830-1850
# 2) varhaisin julkaisuvuosi 1810 (1850-40)
# 3) myöhäisin julkaisuvuosi 1870 (1830+40)

# Lisäkriteeriksi voi ottaa vaikka tiedetyn maksimitoiminta-ajan plus/miinus 5-10 vuotta, jolloin väärät osumat vähentynevät huomattavasti.


