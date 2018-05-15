
mr_cleaner <- function (metadata_stores = NULL,
                        dir_response = NULL,
                        date_execution = NULL ) {
  
  ## Get store info
  list_stores <- metadata_stores %>%
    dplyr::pull(store_id) %>%
    as.numeric()
  
  ## Create directory for each store
  for (i in list_stores) {
    print(i)
    
    file_dir <- paste("./../../04 Output/", dir_response, "/", "Model result for store - ", 
                                 i, sep = "")
    
    file_remove <- dir(path = paste("./../../04 Output/", dir_response, 
                                    "/", "Model result for store - ", i, sep = ""), 
                       pattern = date_execution,
                       full.names = T)
    
    ## Create directory
    if ( dir.exists(file_dir) ) {
      if ( any(!is.na(file_remove)) ) {
        file.remove(file_remove)
      }
    }
  }
}
