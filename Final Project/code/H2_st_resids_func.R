# Calculating Standardized residuals for fitted Analytics Ci Moderate Fund of Funds (H2) GARCH model
H2_st_resids_func <-  function(x){
  
  H2 <- x %>%
    group_by(FundName) %>% 
    filter(FundName == "Analytics Ci Moderate Fund of Funds") %>% 
    filter(Date <= as.Date("2018-09-13")) %>% 
    select(Date, FundName, Price, Return, Cumulative_Return)
  
  
  # make time series
  H2 <- H2 %>% 
    select(Date, FundName, Return) %>% 
    ungroup() %>% 
    tbl_xts()
  
  
  st_resids <- xts(H2_fit@fit$residuals/H2_fit@fit$sigma, order.by = index(H2))
  
  # autocorrelation functions
  Acf(st_resids, main = "Standardized Residuals")
  
  Acf(st_resids^2, main = "Squared Standardized Residuals")
  
}
