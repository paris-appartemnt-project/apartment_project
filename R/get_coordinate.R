#' Add coordinates of the apartment to the dataframe containing the ads information
#'
#' @param df dataframe
#'
#' @return dataframe
#' @export
#' @import dplyr
#' @importFrom purrr pmap
#' @importFrom tidyr unnest
#'
#' @examples
get_coordinate <- function(df){
  df <- df %>% mutate(coord =
                        pmap(list(numero = lextrait.n, rue = priority, metro = lextrait.stationextrait), .f = location_apartment)
  ) %>%
    unnest(coord)
  return(df)
}
