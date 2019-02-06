# Calculating Standardized residuals for Old Mutual Balanced Fund A GARCH model
M4_st_resids_func <-  function(x){
  
  M4 <- x %>%
    group_by(FundName) %>% 
    filter(FundName == "Old Mutual Balanced Fund A") %>% 
    filter(Date <= as.Date("2018-09-13")) %>% 
    select(Date, FundName, Price, Return, Cumulative_Return)
  
  
  # make time series
  M4 <- M4 %>% 
    select(Date, FundName, Return) %>% 
    ungroup() %>% 
    tbl_xts()
  
  
  st_resids <- xts(M4_fit@fit$residuals/M4_fit@fit$sigma, order.by = index(M4))
  
  # autocorrelation functions
  Acf(st_resids, main = "Standardized Residuals")
  
  Acf(st_resids^2, main = "Squared Standardized Residuals")
  
}