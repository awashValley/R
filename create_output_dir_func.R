
create_output_dir <- function (metadata_stores = NULL,
                               dir_response = NULL) {
  
  ## Get store info
  list_stores <- metadata_stores %>%
    dplyr::pull(store_id) %>%
    as.numeric()
  
  ## Create directory for each store
  for (i in list_stores) {
    
    # new_dir <- paste("./../../04 Output/Models - ARIMA/", "Model result for store - ", i, sep = "")
    new_dir <- paste("./../../04 Output/", dir_response, "/", "Model result for store - ", i, sep = "")
    
    ## Create directory
    if ( !dir.exists(new_dir) ) {
      dir.create(path = new_dir)
    }
  }
}
