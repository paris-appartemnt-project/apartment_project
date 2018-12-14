#' Extract the coordinates of the apartments with characteristics asked from the client
#'
#' @param min_price minimum price of the apartment
#' @param max_price maximum price of the apartment
#' @param min_surface minimum surface of the apartment
#' @param N arrondissement in Paris
#' @param max_surface maximum surface of the apartment
#' @param min_room minimum number of rooms
#' @param max_room maximum number of rooms
#' @param castorus_table dataframe with the informations
#'
#' @return dataframe with two columns longitude and latitude
#' @export
#' @import dplyr
get_minimalfeatures <- function(min_price,max_price,min_surface,max_surface,N,min_room,max_room, castorus_table){
  castorus_table <- castorus_table %>%
    filter(prix %in% c(min_price:max_price)) %>%
    filter(m2 %in% c(min_surface:max_surface)) %>%
    filter(piec.%in% c(min_room:max_room)) %>%
    filter(arrondissement %in% N) %>%
    dplyr::select(-titre,-vue.le,-Type) %>%
  filter(latitude != is.na(latitude)) %>%
  filter(longitude != is.na(longitude)) %>%
  select(longitude, latitude)
  }
