#' The function takes a dataframe and inputs a new column for the marker image, and also selects the longitude and lattitude column.
#'
#' @param castorus_table work on this data frame to obtain the longitude, the lattitude and icon shape colum.
#'
#' @return a data frame with longitude lattitude and the icon shape column.
#' @export
coordinates <- function(castorus_table){
  a <- castorus_table %>% dplyr::select("latitude","longitude") %>% dplyr::mutate(icon = iconUrl)
  return(a)
}
