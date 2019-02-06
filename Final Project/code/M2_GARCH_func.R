# Fitting appropriate GARCH model to Coronation Balanced Plus Fund A
M2_GARCH_func <- function(x){
  
  
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
  
  garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[3]
                                              , garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(1, 2), include.mean = TRUE), 
                        distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                               "sged", "nig", "ghyp", "jsu")[3])
  
  M2_fit = ugarchfit(spec = garch11, data = as.numeric(M2), solver = "solnp")
  
  M2_fit
}
