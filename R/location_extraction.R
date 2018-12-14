#' Cleans the offers descriptions by removing the unwanted characters and getting rid of the ponctuation
#'
#' @param descriptions character 
#'
#' @return character
#' @export
#'
#'@examples 
#' annonces<-castorus$Description
#' clean(annonces)

clean <- function(df) {
  
  annoncesmini<- tolower(df$Description)
  unwanted_array = list(     'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                             'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                             'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss','á'='a',"à"="a", 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c', 
                             'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o', 'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
  #this is a list of all unwanted characters in our text. I will combine later with other function in R to remove all   of them from our text.
  cleanannonces <- chartr(paste(names(unwanted_array), collapse=''),
                          paste(unwanted_array, collapse=''),
                          annoncesmini) 
  cleanannonces <- gsub("-", " ", cleanannonces, fixed=TRUE)
  return(cleanannonces)}


#' extracts the roadnames or public transportation stations from the apartment descriptions
#'
#' @param places dataframe with the names of Paris' streets or stations 
#' @param castorus dataframe
#'
#' @return dataframe with the appartments and their streets or stations
#' @export
#'
#' @examples
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
#'
#' @examples number(castorus)
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
#'
#' @examples
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




