#' Find the longitude and latitude of an apartment according to the nearest metro station and street
#'
#' @param metro name of the nearest metro station
#' @param street name of the street of the apartment
#'
#' @return dataframe with two columns : latitude and longitude
#' @export
#' @import dplyr
#' @importFrom purrr map
#'
#' @examples
location_metro_street <- function(metro, street){
  stations <- strsplit(metro,",")
  stations <- paste0(stations,", metro, Paris, France")
  streets <- strsplit(street,",")
  streets <- paste0(streets, ", Paris, France")
  stations_streets <- c(streets,stations)
  stations_streets <- stations_streets %>%
    map(google_geocode,key = my.env$google_key)
  coord <-  map(stations_streets, function(i) i$results$geometry$location) %>%
    bind_rows() %>%
    summarise(lat=mean(lat),lng=mean(lng))
  return(coord)
}
