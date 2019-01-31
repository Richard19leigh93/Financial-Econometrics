# Fitting appropriate GARCH model to STANLIB Global Balanced Feeder Fund B (H1)
H1_GARCH_func <- function(x){
  

H1 <- x %>%
  group_by(FundName) %>% 
  filter(FundName == "STANLIB Global Balanced Feeder Fund B") %>% 
  filter(Date <= as.Date("2018-10-15")) %>% 
  select(Date, FundName, Price, Return, Cumulative_Return)
  
# make time series
H1 <- H1 %>% 
    select(Date, FundName, Return) %>% 
    ungroup() %>% 
    tbl_xts()
  
  
garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[1]
                                              , garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(1, 1), include.mean = FALSE), 
                        distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                               "sged", "nig", "ghyp", "jsu")[3])
  
H1_fit = ugarchfit(spec = garch11, data = as.numeric(H1), solver = "solnp")
  
H1_fit
}

