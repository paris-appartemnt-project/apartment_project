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

clean <- function(descriptions) {

  annoncesmini<- tolower(annonces)
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