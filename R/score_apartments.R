#' Score the apartments according to the densities of restaurants,commerce,cinema,schools and stations
#'
#' @param omit dataframe
#'
#' @return dataframe with 1 column for the index of the good, and columns scoring the apartments among categories
#' @export
#' @import dplyr
#' @importFrom plyr join_all
#'
#' @examples
score_apartments <- function(omit){

  df_commerce <- get_rank(commerce,200)
  df_restaurant <- get_rank(restaurants,500)
  df_cinema <- get_rank(cinema,500)
  df_schools <- get_rank(schools,500)
  df_stations <- get_rank(stations,500)
  df <- list(df_restaurant,df_commerce,df_cinema,df_schools,df_stations)

  df_scoring <- join_all(df[1:5], type = "inner")


  df_scoring <- df_scoring %>%
    mutate(anime_resto_cine = restaurant + cinema) %>%
    select(-cinema,-restaurant) %>%
    mutate(apartment_index = as.character(apartment_index)) %>%
    mutate_if(is.integer,funs(qt = decile)) %>%
    mutate(score = (omit$anim)*anime_resto_cine_qt + (omit$com)*commerce_qt + (omit$school)*schools_qt +
             (omit$metro)*stations_qt) %>%
    arrange(score)

  return(df_scoring)
}

