# Fitting appropriate GARCH model to Oasis Crescent Balanced High Equity Fund of Funds D
H4_GARCH_func <- function(x){
  
  
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

  garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[3]
                                              , garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(2, 2), include.mean = FALSE), 
                        distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                               "sged", "nig", "ghyp", "jsu")[3])
  
  H4_fit = ugarchfit(spec = garch11, data = as.numeric(H4), solver = "solnp")
  
  H4_fit
}
