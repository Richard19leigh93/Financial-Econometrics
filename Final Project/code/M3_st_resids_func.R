# Calculating Standardized residuals for Kagiso Islamic Balanced Fund A GARCH model
M3_st_resids_func <-  function(x){
  
  M3 <- x %>%
    group_by(FundName) %>% 
    filter(FundName == "Kagiso Islamic Balanced Fund A") %>% 
    filter(Date <= as.Date("2018-09-13")) %>% 
    select(Date, FundName, Price, Return, Cumulative_Return)
  
  
  # make time series
  M3 <- M3 %>% 
    select(Date, FundName, Return) %>% 
    ungroup() %>% 
    tbl_xts()
  
  
  st_resids <- xts(M3_fit@fit$residuals/M3_fit@fit$sigma, order.by = index(M3))
  
  # autocorrelation functions
  Acf(st_resids, main = "Standardized Residuals")
  
  Acf(st_resids^2, main = "Squared Standardized Residuals")
  
}