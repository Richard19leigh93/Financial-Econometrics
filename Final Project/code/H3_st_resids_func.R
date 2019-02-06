# Calculating Standardized residuals for fitted Analytics Ci Moderate Fund of Funds (H2) GARCH model
H3_st_resids_func <-  function(x){
  
  H3 <- x %>%
    group_by(FundName) %>% 
    filter(FundName == "Momentum International Balanced Feeder Fund A") %>% 
    filter(Date <= as.Date("2018-09-13")) %>% 
    select(Date, FundName, Price, Return, Cumulative_Return)
  
  
  # make time series
  H3 <- H3 %>% 
    select(Date, FundName, Return) %>% 
    ungroup() %>% 
    tbl_xts()
  
  
  st_resids <- xts(H3_fit@fit$residuals/H3_fit@fit$sigma, order.by = index(H3))
  
  # autocorrelation functions
  Acf(st_resids, main = "Standardized Residuals")
  
  Acf(st_resids^2, main = "Squared Standardized Residuals")
  
}
