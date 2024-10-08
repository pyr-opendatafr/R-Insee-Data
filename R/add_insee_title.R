#' Add a title column to the idbank list dataset
#'
#' @details this function uses extensively the get_insee_title function.
#' Then, it should be used on an already filtered dataset, not on the full idbank dataset (cf. get_insee_title).
#' The number of separators in the official INSEE title can vary and is not normalized. Beware all title columns created may not be a cleaned dimension label.
#' @param df a dataframe containing an idbank column called "idbank" or "IDBANK"
#' @param split split the title column in several columns, by default is TRUE
#' @param n_split number of new columns, by default the maximum is chosen
#' @param clean remove the columns filled with NA (missing value), by default is TRUE
#' @param lang returns an English title, by default is "en", any other value returns a French title
#' @return the same dataframe but with one or several title columns
#' @examples
#' \donttest{
#'library(magrittr)
#'library(dplyr)
#'
#'idbank_empl =
#'  get_idbank_list("EMPLOI-SALARIE-TRIM-NATIONAL") %>% #employment
#'  slice(1:15) %>%
#'  add_insee_title()
#' }
#' @export
add_insee_title = function(df, n_split, lang = "en", split = TRUE, clean = TRUE){

  if(!is.null(df)){

    col_idbank = which(names(df) %in% c("idbank", "IDBANK"))

    if(length(col_idbank) > 0 & nrow(df) > 0){

      col_idbank = min(col_idbank)

      col_idbank_name = names(df)[col_idbank]

      df = dplyr::mutate(.data = df,
                         title = insee::get_insee_title(.data[[!!col_idbank_name]],
                                                        lang = lang))

      if(missing(n_split)){
        n_split = "max"
      }

      if(split){
        if("title" %in% names(df)){
          df = insee::split_title(df = df, n_split = n_split, title_col_name = "title", lang = lang)
        }
      }
      if(clean){
        df = clean_table(df)
      }
    }
  }

  return(df)
}


