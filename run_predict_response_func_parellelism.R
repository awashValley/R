
run_predict_response <- function (loop_storeID = NULL,
                                  data = NULL,
                                  response_type = NULL,
                                  df_bestModel_candidates = NULL,
                                  metadata_stores = NULL,
                                  window_forTestDataset = 30,
                                  dir_response = NULL) {
  
  ## Set environment for parallel computing
  n_cores <- parallel::detectCores()    ## 4
  cl <- parallel::makeCluster(n_cores - 1) # create a cluste  
  # parallel::clusterExport(cl, c("predict_turnover", "create_model_matrix",
  parallel::clusterExport(cl, c("predict_response", "create_model_matrix",
                                "create_data_future", "calculate_coverage"))    ## Export custom functions
  doParallel::registerDoParallel(cl) # register the cluster
  
  ## Start looping
  start_time <- Sys.time()
  
  ## create log
  filename_out <- file.path(paste("./../../", 
                                  "/log/run_predict_response_pickup_", 
                                  Sys.Date(), ".log", sep = ""))
  
  writeLines(text = c(""), con = filename_out)
  
  ## Run foreach function
  predict_turnover_all <- foreach::foreach(x = loop_storeID,
                                           .combine = 'rbind',
                                           .packages = 'magrittr'
                                           # .packages = c('magrittr', 'grDevices')
                                           ) %dopar% {
    
    sink(file = filename_out, append=TRUE)
                                             
     predict_response(data = data,
                      response_type = response_type,
                      store_sel = x,
                      df_bestModel_candidates = df_bestModel_candidates,
                      window_forTestDataset = window_forTestDataset,
                      metadata_stores = metadata_stores,
                      dir_response = dir_response)
                                             
  }
  
  # shut down the cluster
  parallel::stopCluster(cl)
  
  ## Calculate program execution time
  end_time <- Sys.time()
  execution_time <- end_time - start_time
  print(paste("Total execution time: ", execution_time, sep = ""))
}
