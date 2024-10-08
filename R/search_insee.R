
#' Search a pattern among insee datasets and idbanks
#'
#' @details The data related to idbanks is stored internally in the package and might the most up to date.
#' The function ignores accents and cases.
#' @param pattern string used to filter the dataset and idbank list
#' @return the dataset and idbank table filtered with the pattern
#' @examples
#' \donttest{
#' # example 1 : search one pattern, the accents do not matter
#' writeLines("the word 'enqu\U00EAte' (meaning survey in French) will match with 'enquete'")
#' dataset_enquete = search_insee("enquete")
#'
#' # example 2 : search multiple patterns
#' dataset_survey_gdp = search_insee("Survey|gdp")
#'
#' # example 3 : data about paris
#' data_paris = search_insee('paris')
#'
#' # example 4 : all data
#' data_all = search_insee()
#' }
#' @export
search_insee = function(pattern = '.*'){

  if(is.null(pattern)){pattern = '.*'}
  if(pattern == ''){pattern = '.*'}

  insee_download_verbose = if(Sys.getenv("INSEE_download_verbose") == "TRUE"){TRUE}else{FALSE}

  file_cache = file.path(tempdir(), paste0(openssl::md5(paste0("search", pattern)), ".rds"))

  if(!file.exists(file_cache)){
    dataset_list = suppressMessages(get_dataset_list())

    # create new french name column withtout accent
    dataset_list = dplyr::mutate(.data = dataset_list,
                                 nomflow = .data$id,
                                 Name.fr_accent = iconv(.data$Name.fr,
                                                        from = "UTF-8", to = 'ASCII//TRANSLIT')
    )

    # filter the dataset list no matter the cases
    dataset_selected = dplyr::filter_at(
      .tbl = dataset_list,
      .vars = dplyr::vars("Name.en", "Name.fr", "Name.fr_accent"),
      .vars_predicate = dplyr::any_vars(stringr::str_detect(.data$.,
                                                            stringr::regex(pattern, ignore_case = TRUE))))

    dataset_selected = dplyr::select(.data = dataset_selected, -"Name.fr_accent")


    idbank_list_search = dplyr::select(.data = idbank_list_internal,
                                       "nomflow", "idbank", "title_fr", "title_en")

    idbank_list_search = dplyr::mutate(.data = idbank_list_search,
                                       stop_var = dplyr::case_when(stringr::str_detect(.data$title_en,
                                                                                       stringr::regex('stopped series', ignore_case = TRUE)) ~ 1,
                                                                   TRUE ~ 0))

    idbank_list_search = dplyr::arrange(.data = idbank_list_search, "stop_var")

    idbank_list_search = dplyr::mutate(.data = idbank_list_search,
                                       title_fr_accent = iconv(.data$title_fr,
                                                               from = "UTF-8", to = 'ASCII//TRANSLIT'))

    idbank_list_search = dplyr::filter_at(
      .tbl = idbank_list_search,
      .vars = dplyr::vars("title_en", "title_fr", "title_fr_accent"),
      .vars_predicate = dplyr::any_vars(stringr::str_detect(.data$.,
                                                            stringr::regex(pattern, ignore_case = TRUE))))

    idbank_list_search = dplyr::select(.data = idbank_list_search,
                                       -"title_fr_accent", -"stop_var")

    idbank_list_search = dplyr::rename(.data = idbank_list_search,
                                       id = "idbank")

    dataset_selected = dplyr::rename(.data = dataset_selected,
                                     title_fr = "Name.fr",
                                     title_en = "Name.en")

    search_results = dplyr::bind_rows(dataset_selected, idbank_list_search)

    search_results = dplyr::select(.data = search_results,
                                   "nomflow", "id", "title_fr", "title_en")

    file_warning_search_data = file.path(tempdir(), paste0(openssl::md5("search_data"), ".rds"))

    if(!file.exists(file_warning_search_data)){
      msg1 = "\nInternal package data has been used, it may not be the most up-to-date data"
      msg2 = "This message is displayed once per R session"
      msg = sprintf("%s\n%s\n", msg1, msg2)

      message(crayon::style(msg, "red"))
      save(msg, file = file_warning_search_data)
    }


    saveRDS(search_results, file = file_cache)
    if(insee_download_verbose){
      msg = sprintf("Data cached : %s\n", file_cache)
      message(crayon::style(msg, "green"))
    }

  }else{
    if(insee_download_verbose){
      msg = "Cached data has been used"
      message(crayon::style(msg, "green"))
    }
    search_results = readRDS(file_cache)
  }

  return(search_results)
}


