# Fitting appropriate GARCH model to Old Mutual Balanced Fund A
M4_GARCH_func <- function(x){
  
  
  M4 <- Data %>%
    group_by(FundName) %>% 
    filter(FundName == "Old Mutual Balanced Fund A") %>% 
    filter(Date <= as.Date("2018-09-13")) %>% 
    select(Date, FundName, Price, Return, Cumulative_Return)
  
  # make time series
  M4 <- M4 %>% 
    select(Date, FundName, Return) %>% 
    ungroup() %>% 
    tbl_xts()
  
  garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[3]
                                              , garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(0,2), include.mean = FALSE), 
                        distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                               "sged", "nig", "ghyp", "jsu")[3])
  
  M4_fit = ugarchfit(spec = garch11, data = as.numeric(M4), solver = "solnp")
  
  M4_fit
}