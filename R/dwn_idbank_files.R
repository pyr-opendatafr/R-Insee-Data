
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

  # if (option_extra == ""){
  #   dwn = utils::download.file(file_to_dwn, temp_file,
  #                              mode = option_mode, quiet = TRUE)
  # }else{
  #   dwn = utils::download.file(file_to_dwn, temp_file,
  #                              method = option_method,
  #                              mode = option_mode,
  #                              extra = option_extra,
  #                              quiet = TRUE)
  # }
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  # file_to_dwn = "https://www.insee.fr/en/statistiques/fichier/2868055/202209_correspondance_idbank_dimension.zip"

  if(option_extra == ""){
    response = try(httr::GET(file_to_dwn), silent = TRUE)
  }else{

    proxy = httr::use_proxy(url = option_proxy,
                            port = as.numeric(option_port),
                            auth = option_auth)

    response = httr::GET(url = file_to_dwn,
                         config = proxy)
  }

  # trigger error
  if (response$status_code != 200){
    trigger_error = "1" + "1"
  }

  #create temporary directory for storing and unzipping file
  td <- tempdir()

  #open connection to write contents
  zipF <- paste0(td, "/idbankfile.zip")

  filecon <- file(zipF, "wb")

  #write data contents to the temporary file
  writeBin(response$content, filecon)

  #close the connection
  close(filecon)

  uzp = utils::unzip(zipF, exdir = insee_data_dir)
  potential_file = list.files(insee_data_dir, pattern = "correspondance_idbank_dimension")[1]

  if (is.na(potential_file)){
    potential_file = list.files(insee_data_dir)[1]
  }

  mapping_file = file.path(insee_data_dir, potential_file)

  mapping = utils::read.delim(mapping_file, sep = mapping_file_sep,
                              stringsAsFactors = F)
  return(mapping)
}

#' @noRd
dwn_idbank_files = function(){

  data = suppressWarnings(try(dwn_idbank_file(), silent = TRUE))

  if ("try-error" %in% class(data)){

    curr_year = as.numeric(substr(Sys.Date(), 1, 4))
    curr_month = as.character(substr(Sys.Date(), 6, 7))
    last_year = curr_year - 1

    months_char = c()
    for(i in as.character(1:12)){
      if (nchar(i) == 1){
        months_char = c(months_char, paste0("0", i))
      }else{
        months_char = c(months_char, i)
      }
    }

    idbank_file_env = Sys.getenv("INSEE_idbank_dataset_path")
    pattern_file_env = Sys.getenv("INSEE_idbank_dataset_file")

    dates_pattern_list = c(paste0(curr_year, curr_month), paste0(curr_year, months_char), paste0(last_year, months_char))
    files_pattern = paste0(dates_pattern_list, "_correspondance_idbank_dimension")
    files_pattern = c(pattern_file_env, files_pattern)
    files_dwn = paste0("https://www.insee.fr/en/statistiques/fichier/2868055/" , files_pattern, '.zip')
    files_dwn = c(idbank_file_env, files_dwn)

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



