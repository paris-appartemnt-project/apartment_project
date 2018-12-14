#' The Function inputs a dataframe and filters out the quantiles for a specific category that the user wants or do not want.
#'
#' @param castorus_table a data frame we are going to modify
#' @param tri The dataframe with the scores
#' @param min The miminum quantile we want.
#' @param max The highest quantile we want.
#' @param value Select the column in tri which is going to be filtered.
#'
#' @return data frame
#' @export
#'
filter_slider <- function(castorus_table, tri, min, max, value){
  temp<- tri %>% dplyr::filter(tri_categories[,value] %in% c(min:max)) %>% select(X)
  a <- castorus_table %>% dplyr::filter(X %in% temp$X)
  return(a)
}
