#' This function is used to be able to obtain the indexes of the selected rows in the datatable.
#'
#' @param c the indexes of the selected rows
#' @param df1 the dataframe which is going to be used.
#'
#' @return data frame.
#' @export
column_index <- function(c, df1){
  df1 <- df1[c,]
}
