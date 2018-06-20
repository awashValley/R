
visualize_triggerLimit <- function (data = NULL, 
                                    response_type = NULL,
                                    response_actual = NULL,
                                    response_predicted = NULL,
                                    list_storeID = c(1:4),
                                    data_type = NULL) {
  
  ## Define plot characterstics
  if ( response_type == "turnover" ) {
    y_lab <- "Insurance limit amount"
    title_lab <- "Trend for Insurance limit amount - actual versus predicted"
    h_line <- 100000
  }
  else if ( response_type == "notesfifty" ) {
    y1_lab <- "Deposit cassette amount"
    title_lab <- "Trend for Deposit cassette (50+ notes) - actual versus predicted"
    h_line <- 4000
  }
  
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
  
  ## Plot TS data
  plt1 <- dsin %>% 
    ggplot2::ggplot(aes(x = date, y = response_predicted)) +
    geom_line(aes(x = date, y = response_predicted, linetype="Predicted"), color="blue", size = 0.75) +
    geom_line(aes(x = date, y = response_actual, linetype="Actual"), color="black", size = 0.75) +
    geom_hline(yintercept = h_line, color="red", size = 0.75) + 
    scale_y_continuous(limits = c(min_amount, max_amount)) + 
    scale_linetype_manual("Response type", values = c(1,2)) +
    labs(x="Date", y=y_lab, 
         title=title_lab) + 
    theme_bw() +
    theme(legend.position = "bottom", 
          legend.background = element_rect(color = "black", 
                                           fill = "grey90", size = 1, linetype = "solid"), 
          legend.direction = "horizontal") + 
    facet_wrap(~ store)
  
  plt1
}
