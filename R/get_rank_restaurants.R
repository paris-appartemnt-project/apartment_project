#' Rank the apartments according to their proximity to restaurants and bars
#'
#' @param dataframe restaurants dataframe
#' @param meters proximity in meters to the apartment
#'
#' @return dataframe with two columns: the index of the apartment and its rank
#' @export
#' @import dplyr
#' @importFrom geosphere distm
#' @importFrom geosphere distGeo
#' @importFrom stats kmeans
#'
#' @examples
get_rank_restaurants <- function(dataframe, meters){
  dataframe <- dataframe[!(dataframe$longitude == ""), ]
  coord_rest <- data.frame(longitude = restaurants$longitude, latitude = restaurants$latitude)
  clusters <- kmeans(coord_rest, centers = 100, nstart = 100)
  distance_apartment <- distm(clusters$centers, castorus_data, fun=distGeo)
  distance_apartment <- data.frame(nb_restaurants = clusters$size, apartment = distance_apartment)
  distance_apartment[,-1] <- (distance_apartment[,-1] < meters)*distance_apartment$nb_restaurants
  count_criterion <- colSums(distance_apartment[,-1])
  ranking <- data.frame(apartment_index = 1:nrow(castorus_data), rank = count_criterion)
  ranking <-  ranking %>%
    arrange(desc(rank)) %>%
    mutate(rank = 1:nrow(castorus_data))
}
