# Fitting appropriate GARCH model to Analytics Ci Moderate Fund of Funds (H2)
H2_GARCH_func <- function(x){
  
  
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

  garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[3]
                                              , garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(2, 2), include.mean = FALSE), 
                        distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                               "sged", "nig", "ghyp", "jsu")[3])
  
  H2_fit = ugarchfit(spec = garch11, data = as.numeric(H2), solver = "solnp")
  
  H2_fit
}
