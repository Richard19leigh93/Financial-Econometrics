# Calculating Standardized residuals
H4_st_resids_func <-  function(x){
  
  H4 <- x %>%
    group_by(FundName) %>% 
    filter(FundName == "Oasis Crescent Balanced High Equity Fund of Funds D") %>% 
    filter(Date <= as.Date("2018-09-13")) %>% 
    select(Date, FundName, Price, Return, Cumulative_Return)
  
  
  # make time series
  H4 <- H4 %>% 
    select(Date, FundName, Return) %>% 
    ungroup() %>% 
    tbl_xts()
  
  
  st_resids <- xts(H4_fit@fit$residuals/H4_fit@fit$sigma, order.by = index(H4))
  
  # autocorrelation functions
  Acf(st_resids, main = "Standardized Residuals")
  
  Acf(st_resids^2, main = "Squared Standardized Residuals")
  
}
