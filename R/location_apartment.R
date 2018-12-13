#' Choose the right function to locate the apartment according to the available information
#'
#' @param num number of the street of the apartment
#' @param street name of the street of the apartment
#' @param metro name of the nearest metro station
#'
#' @return dataframe with two columns : latitude and longitude
#' @export
#'
#' @examples
#' location_apartment(num= "12", street ="rue de Courcelles", metro = "Saint-Philippe-du-Roule")
#' location_apartment(num= "12", street ="rue de Courcelles", metro = NA)
#' location_apartment(num= NA, street ="rue de Courcelles", metro = "Saint-Philippe-du-Roule")
#' location_apartment(num= NA, street ="rue de Courcelles", metro = NA)
#' location_apartment(num= NA, street = NA, metro = "Saint-Philippe-du-Roule")
location_apartment <- function(num, street, metro){
  if( !is.na(num) & !is.na(street)){
    location_comp(num,street)
  } else if(is.na(num) & !is.na(street) & is.na(metro)){
    location_street(street)
  } else if(is.na(num) & is.na(street) & !is.na(metro)){
    location_metro(metro)
  } else if(is.na(num) & !is.na(street) & !is.na(metro)){
    location_metro_street(metro, street)
  } else {
    tibble(lat = NA, lng = NA)
  }
}
