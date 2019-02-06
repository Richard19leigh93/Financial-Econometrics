# Calculating Standardized residuals for fitted STANLIB Global Balanced Feeder Fund B (H1) GARCH model
H1_st_resids_func <-  function(x){

H1 <- x %>%
  group_by(FundName) %>% 
  filter(FundName == "STANLIB Global Balanced Feeder Fund B") %>% 
  filter(Date <= as.Date("2018-09-13")) %>% 
  select(Date, FundName, Price, Return, Cumulative_Return)


# make time series
H1 <- H1 %>% 
  select(Date, FundName, Return) %>% 
  ungroup() %>% 
  tbl_xts()


st_resids <- xts(H1_fit@fit$residuals/H1_fit@fit$sigma, order.by = index(H1))

# autocorrelation functions
Acf(st_resids, main = "Standardized Residuals")

Acf(st_resids^2, main = "Squared Standardized Residuals")

}
