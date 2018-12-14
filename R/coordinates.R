#' The function takes a dataframe and inputs a new column for the marker image, and also selects the longitude and lattitude column.
#'
#' @param castorus_table work on this data frame to obtain the longitude, the lattitude and icon shape colum.
#' @param iconUrl allows to get the flag on the map
#' @return a data frame with longitude lattitude and the icon shape column.
#' @export
#' @import dplyr
coordinates <- function(castorus_table, iconUrl){
  a <- castorus_data %>%
    dplyr::select("latitude","longitude") %>%
    dplyr::mutate(icon = iconUrl)
  return(a)
}
