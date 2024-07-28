#' @noRd
clean_table = function(df){
  df[, colSums(is.na(df)) != nrow(df)]
}
