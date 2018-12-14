#' Cleans the offers descriptions by removing the unwanted characters and getting rid of the ponctuation
#'
#' @param df character
#'
#' @return character
#' @export
#'

clean <- function(df) {

  annoncesmini<- tolower(df$Description)
  cleanannonces <- gsub("-", " ", anoncesmini, fixed=TRUE)
  return(cleanannonces)}


#' extracts the roadnames or public transportation stations from the apartment descriptions
#'
#' @param places dataframe with the names of Paris' streets or stations
#' @param castorus dataframe
#'
#' @return dataframe with the appartments and their streets or stations
#' @export
#'
match_environnement <- function(places,castorus){

  match <- sapply(places, function(x) grepl(x, clean(castorus) ,ignore.case = F ,perl = F,
                                            fixed = TRUE, useBytes = FALSE))
  rue_du_bien <- apply(match, 1, function(i) paste0(names(i)[i], collapse = ","))
  rue_du_bien<- data.frame(rue_du_bien)
  return(rue_du_bien)
}


#' Extracts the street numbers from the description of the apartment
#'
#' @param castorus dataframe
#'
#' @return dataframe with the apartments and their street numbers
#' @export
#' @importFrom stringr str_extract
#' @importFrom tm removeWords
number <- function(castorus) {

  numero_de_rue <- str_extract(castorus$Description, "([\\d]+) rue ")
  numero_de_rue <- removeWords(numero_de_rue , "rue")
  numero_de_rue <- data.frame(numero_de_rue)
  numero_de_rue <- numero_de_rue %>%
    mutate(numero_de_rue = as.numeric(levels(numero_de_rue))[numero_de_rue])
  numero_de_rue[numero_de_rue>500]=NA
  return(numero_de_rue)
}


#' Combines the results form the datascrapping and the extraction from the descriptions
#'
#' @param castorus dataframe
#' @param roadnames dataframe with street names
#' @param stations dataframe with station names
#'
#' @return dataframe with the apartments and their locations coming form the scraping and the extraction from the descriptions
#' @export
combine_roads <- function(castorus,roadnames,stations) {
  ruepriority <- castorus$rue
  fullextrait <- data.frame(number(castorus),match_environnement(roadnames,castorus),
                            match_environnement(stations,castorus))

  lextrait <- data.frame(ruepriority,fullextrait)
  priority <- data.frame(lextrait$ruepriority,lextrait$rue_du_bien)
  priority <- priority %>%
    mutate_all(as.character())

  ind <- is.na(priority$lextrait.ruepriority)
  priority$lextrait.ruepriority[ind] <- priority$lextrait.rue_du_bien[ind]
  priority <- data.frame(priority)
  priority <- priority$lextrait.ruepriority
  lextrait <- data.frame(lextrait$numero_de_rue,priority,lextrait$station_a_proximite)
  lextrait <- lextrait[-c(974,4582,6429,6982,12170,114,1055,1536,2542,3147,7933,8341,12158), ]
  castorus_final<- castorus[-c(974,4582,6429,6982,12170,114,1055,1536,2542,3147,7933,8341,12158), ]
  castorus_final<- data.frame(castorus_final, lextrait)
  return(castorus_final)
}




