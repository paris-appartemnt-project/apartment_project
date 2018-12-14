#' Find the longitude and latitude of an apartment according to its complete address
#'
#' @param num number of the street of the apartment
#' @param street name of the street of the apartment
#'
#' @return dataframe with two columns : latitude and longitude
#' @export
#'
#' @importFrom googleway google_geocode
location_comp <- function(num,street){
  loc <- google_geocode(paste0(num,street,", Paris, France"), key = my.env$google_key)
  lat <- loc$results$geometry$location$lat
  lon <- loc$results$geometry$location$lng
  coord <- as.data.frame(list(lat,lon))
  colnames(coord) <- c("lat","lng")
  return(coord)
}
