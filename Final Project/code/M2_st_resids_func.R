# Calculating Standardized residuals for fitted Coronation Balanced Plus Fund A GARCH model
M2_st_resids_func <-  function(x){
  
  M2 <- x %>%
    group_by(FundName) %>% 
    filter(FundName == "Coronation Balanced Plus Fund A") %>% 
    filter(Date <= as.Date("2018-09-13")) %>% 
    select(Date, FundName, Price, Return, Cumulative_Return)
  
  
  # make time series
  M2 <- M2 %>% 
    select(Date, FundName, Return) %>% 
    ungroup() %>% 
    tbl_xts()
  
  
  st_resids <- xts(M2_fit@fit$residuals/M2_fit@fit$sigma, order.by = index(M2))
  
  # autocorrelation functions
  Acf(st_resids, main = "Standardized Residuals")
  
  Acf(st_resids^2, main = "Squared Standardized Residuals")
  
}