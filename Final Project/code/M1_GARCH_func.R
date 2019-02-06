# Fitting appropriate GARCH model to Allan Gray Tax-Free Balanced Fund A
M1_GARCH_func <- function(x){
  
  
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
  
  garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[3]
                                              , garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(2, 2), include.mean = FALSE), 
                        distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                               "sged", "nig", "ghyp", "jsu")[3])
  
  M1_fit = ugarchfit(spec = garch11, data = as.numeric(M1), solver = "solnp")
  
  M1_fit
}
