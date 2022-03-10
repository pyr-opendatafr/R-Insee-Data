
#' @noRd
dwn_idbank_file = function(){

  temp_file = tempfile()
  insee_data_dir = tempdir()

  file_to_dwn = Sys.getenv("INSEE_idbank_dataset_path")
  mapping_file_pattern = Sys.getenv("INSEE_idbank_dataset_file")
  mapping_file_sep = Sys.getenv("INSEE_idbank_sep")

  option_mode = Sys.getenv("INSEE_download_option_mode")
  option_method = Sys.getenv("INSEE_download_option_method")
  option_port = Sys.getenv("INSEE_download_option_port")
  option_extra = Sys.getenv("INSEE_download_option_extra")
  option_proxy = Sys.getenv("INSEE_download_option_proxy")
  option_auth = Sys.getenv("INSEE_download_option_auth")

  if (option_extra == ""){
    dwn = utils::download.file(file_to_dwn, temp_file,
                               mode = option_mode, quiet = TRUE)
  }else{
    dwn = utils::download.file(file_to_dwn, temp_file,
                               method = option_method,
                               mode = option_mode,
                               extra = option_extra,
                               quiet = TRUE)
  }


  uzp = utils::unzip(temp_file, exdir = insee_data_dir)

  mapping_file = file.path(insee_data_dir, list.files(insee_data_dir, pattern = mapping_file_pattern)[1])

  mapping = utils::read.delim(mapping_file, sep = mapping_file_sep,
                              stringsAsFactors = F)
  return(mapping)
}

#' @noRd
dwn_idbank_files = function(){

  data = suppressWarnings(try(dwn_idbank_file(), silent = TRUE))

  if ("try-error" %in% class(data)){

    curr_year = as.numeric(substr(Sys.Date(), 1, 4))
    last_year = curr_year - 1

    months_char = c()
    for(i in as.character(1:12)){
      if (nchar(i) == 1){
        months_char = c(months_char, paste0("0", i))
      }else{
        months_char = c(months_char, i)
      }
    }

    dates_pattern_list = c(paste0(curr_year, months_char), paste0(last_year, months_char))
    files_pattern = paste0(dates_pattern_list, "_correspondance_idbank_dimension")
    files_dwn = paste0("https://www.insee.fr/en/statistiques/fichier/2868055/" , files_pattern, '.zip')

    i = 1

    while(("try-error" %in% class(data)) & (i <= length(files_pattern))){

      Sys.setenv(INSEE_idbank_dataset_path = files_dwn[i])
      Sys.setenv(INSEE_idbank_dataset_file = files_pattern[i])

      data = suppressWarnings(try(dwn_idbank_file(), silent = TRUE))

      i = i + 1
    }

  }

  return(data)

}



