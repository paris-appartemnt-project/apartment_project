#' Find the longitude and latitude of an apartment according to its street
#'
#' @param street street of the apartment
#'
#' @return dataframe with two columns : latitude and longitude
#' @export
#' @import dplyr
#' @importFrom googleway google_geocode
#' @importFrom purrr map
#'
#' @examples
#' location_street("rue de Courcelles")
location_street <- function(street){
  streets <- strsplit(street,",")
  streets <- paste0(streets,", Paris, France")
  streets <- streets %>%
    map(google_geocode, key = my.env$google_key)
  coord <- map(streets, function(i) i$results$geometry$location) %>%
    bind_rows() %>%
    summarise(lat=mean(lat),lng=mean(lng))
  return(coord)
}
