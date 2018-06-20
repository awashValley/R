
visualize_prediction <- function (data = NULL, 
                                  response_type = NULL,
                                  response_actual = NULL,
                                  response_predicted = NULL,
                                  list_storeID = c(1:4)) {
  
  ## Filter dataset by store ID
  dsin <- as.data.frame(subset(data, store_id %in% list_storeID))
  
  ## Get actual and predicted response values
  response_actual <- as.numeric(dsin[, response_actual]) 
  response_predicted <- as.numeric(dsin[, response_predicted]) 
  
  ## Get date variable
  date <- dsin[, "date"]
  
  ## Get min/max for y-axis limit
  min_amount <- min(min(response_actual), min(response_predicted))
  max_amount <- max(max(response_actual), max(response_predicted))
  
  ## Define plot characterstics
  if ( response_type == "turnover" ) {
    
    y_lab <- "Turnover"
    title_lab <- "Comparison of actual and predicted turnover per store"
    
    ## Get lower/upper limits
    response_predicted_lowerLimit <- as.numeric(dsin[, "turnover_predicted_lowerLimit"]) 
    response_predicted_upperLimit <- as.numeric(dsin[, "turnover_predicted_upperLimit"]) 
  }
  else if ( response_type == "notesfifty" ) {
    
    y_lab <- "Number of Notes (50+) in Deposite cassete"
    title_lab <- "Comparison of actual and predicted number of notes in Deposite cassette per store"
    
    ## Get lower/upper limits
    response_predicted_lowerLimit <- as.numeric(dsin[, "safe50_predicted_lowerLimit"]) 
    response_predicted_upperLimit <- as.numeric(dsin[, "safe50_predicted_upperLimit"]) 
  }
  
  ## Plot TS data
  plot_prediction <- ggplot2::ggplot(data = dsin, ggplot2::aes(x=date, y=response_predicted)) +
    ggplot2::geom_line(ggplot2::aes(x=date, y=response_actual, linetype="Actual", color="Actual"), 
                       size=1,
                       data = dsin) +
    ggplot2::geom_line(ggplot2::aes(x=date, y=response_predicted, linetype="Predicted", color="Predicted"), 
                       size=1,
                       data=dsin) +
    ggplot2::geom_ribbon(ggplot2::aes(x=date, ymin=response_predicted_lowerLimit, 
                                      ymax=response_predicted_upperLimit), alpha=0.25, position='identity', 
                         fill = "blue",
                         data=dsin) +
    scale_linetype_manual("Response type", values = c(1,2)) +
    scale_color_manual("Response type", values = c("black", "blue")) +
    ggplot2::labs(x="Date", y=y_lab, title=title_lab) +
    ggplot2::theme_bw() +
    theme(legend.position = "bottom", 
          legend.background = element_rect(color = "black", 
                                           fill = "grey90", size = 1, linetype = "solid"), 
          legend.direction = "horizontal") + 
    facet_wrap(~ store)
  
  plot_prediction
}
