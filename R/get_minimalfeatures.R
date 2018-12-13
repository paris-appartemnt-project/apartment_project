#' Extract the coordinates of the apartments with characteristics asked from the client
#'
#' @param price range of price of the apartment
#' @param surface range of surface of the apartment
#' @param rooms number of rooms in the apartment
#' @param N arrondissement.s in Paris
#'
#' @return dataframe with two columns longitude and latitude
#' @export
#' @import dplyr
#'
#' @examples
#' price <- c(300000,600000)
#' surface <- c(20,60)
#' N <- c(10,11)
#' rooms <- c(1,4,3)
#' get_minimalfeatures(price, surface, rooms, N)
get_minimalfeatures <- function(price, surface, rooms, N){
  N <- as.character(N)
  castorus_data <- castorus_data %>%
  filter(prix %in% c(price[1]:price[2])) %>%
  filter(m2 %in% c(surface[1]:surface[2])) %>%
  filter(piec.%in% rooms[1:length(rooms)]) %>%
  filter(arrondissement %in% N) %>%
  filter(latitude != is.na(latitude)) %>%
  filter(longitude != is.na(longitude)) %>%
  select(longitude, latitude)
  }
