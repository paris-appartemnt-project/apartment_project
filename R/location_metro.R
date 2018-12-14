#' Find the longitude and latitude of an apartment according to the nearest metro station
#'
#' @param metro name of the nearest metro station
#'
#' @return dataframe with two columns : latitude and longitude
#' @export
#' @import dplyr
#' @importFrom purrr map
#' @importFromd googleway google_geocode
#'
#' @examples
location_metro <- function(metro){
  stations <- strsplit(metro,",")
  stations <- paste0(stations,", metro, Paris, France")
  stations <- stations %>%
    map(google_geocode,key = my.env$google_key)
  coord <-  map(stations, function(i) i$results$geometry$location) %>%
    bind_rows() %>%
    summarise(lat=mean(lat),lng=mean(lng))
  return(coord)
}
