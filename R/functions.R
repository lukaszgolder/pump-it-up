featureEngineering <- function(data) {
  # Make installer lowercase, take first 3 letters as a sub string
  data$install_3 <- substr(tolower(data$installer),1,3)
  data$install_3[data$install_3 %in% c(" ", "", "0", "_", "-")] <- "other"
  
  # Take the top 15 substrings from above by occurance frequency
  install_top_15 <- names(summary(as.factor(data$install_3)))[1:15]
  
  data$install_3[!(data$install_3 %in% install_top_15)] <- "other"
  data$install_3 <- as.factor(data$install_3)
  
  return(data)
}