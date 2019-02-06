# Calculating Standardized residuals for fitted Allan Gray Tax-Free Balanced Fund A GARCH model
M1_st_resids_func <-  function(x){
  
  M1 <- x %>%
    group_by(FundName) %>% 
    filter(FundName == "Allan Gray Tax-Free Balanced Fund A") %>% 
    filter(Date <= as.Date("2018-09-13")) %>% 
    select(Date, FundName, Price, Return, Cumulative_Return)
  
  
  # make time series
  M1 <- M1 %>% 
    select(Date, FundName, Return) %>% 
    ungroup() %>% 
    tbl_xts()
  
  
  st_resids <- xts(M1_fit@fit$residuals/M1_fit@fit$sigma, order.by = index(M1))
  
  # autocorrelation functions
  Acf(st_resids, main = "Standardized Residuals")
  
  Acf(st_resids^2, main = "Squared Standardized Residuals")
  
}