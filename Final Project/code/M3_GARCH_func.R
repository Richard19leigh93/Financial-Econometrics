# Fitting appropriate GARCH model to Kagiso Islamic Balanced Fund A
M3_GARCH_func <- function(x){
  
  
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
  
  garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[3]
                                              , garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(0, 1), include.mean = TRUE), 
                        distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                               "sged", "nig", "ghyp", "jsu")[3])
  
  M3_fit = ugarchfit(spec = garch11, data = as.numeric(M3), solver = "solnp")
  
  M3_fit
}