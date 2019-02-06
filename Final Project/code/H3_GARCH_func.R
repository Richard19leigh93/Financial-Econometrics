# Fitting appropriate GARCH model to Momentum International Balanced Feeder Fund A (H3)
H3_GARCH_func <- function(x){
  
  
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
  
  garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[3]
                                              , garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(2, 2), include.mean = TRUE), 
                        distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                               "sged", "nig", "ghyp", "jsu")[3])
  
  H3_fit = ugarchfit(spec = garch11, data = as.numeric(H3), solver = "solnp")
  
  H3_fit
}
