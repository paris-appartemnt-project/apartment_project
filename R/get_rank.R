#' Rank the apartments according to their proximity to a place
#'
#' @param dataframe dataframe
#' @param meters proximity in meters to the apartment
#'
#' @return dataframe with two columns: the index of the apartment and its rank
#' @export
#' @import dplyr
#' @importFrom geosphere distm
#' @importFrom geosphere distGeo
#'
#' @examples
get_rank <- function(dataframe, meters){
  dataframe <- dataframe[!(dataframe$longitude == ""), ]
  matrix_coord <- cbind(dataframe$longitude,dataframe$latitude)
  distance_apartment <- distm(matrix_coord, castorus_data, fun=distGeo)
  distance_apartment <- data.frame(apartment = distance_apartment)
  distance_apartment <- distance_apartment < meters
  count_criterion <- colSums(distance_apartment)
  ranking <- data.frame(apartment_index = 1:nrow(castorus_data), rank = count_criterion)
  ranking <-  ranking %>%
    arrange(desc(rank)) %>%
    mutate(rank = 1:nrow(castorus_data))
  return(ranking)
}
