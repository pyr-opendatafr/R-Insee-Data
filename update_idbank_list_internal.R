
library(tidyverse)

# 
# UPDATE IDBANK LIST USED AS A BACKUP AND USED INTERNALLY IN THE PACKAGE
# 

# idbank_list_internal = update_idbank_list_internal()
# usethis::use_data(idbank_list_internal, internal = TRUE, overwrite = TRUE)


update_idbank_list_internal = function(){
  
  all_idbank_bdm = insee::get_idbank_list(update=TRUE)
  
  idbank_list_internal_old = insee:::idbank_list_internal
  
  idbank_missing_internally = 
    all_idbank_bdm %>% 
    filter(!idbank %in% idbank_list_internal_old$idbank) %>% 
    dplyr::rename(IDBANK = idbank) %>% 
    insee::add_insee_title(lang='fr',split=FALSE) %>% 
    dplyr::rename(title_fr = title) %>% 
    insee::add_insee_title(lang='en',split=FALSE) %>% 
    dplyr::rename(title_en = title) %>% 
    insee::add_insee_metadata() %>% 
    dplyr::rename(idbank = IDBANK)
  
  idbank_list_internal_new = 
    idbank_list_internal_old %>% 
    filter(idbank %in% all_idbank_bdm$idbank) %>% 
    bind_rows(idbank_missing_internally)
  
  return(idbank_list_internal_new)
  
}