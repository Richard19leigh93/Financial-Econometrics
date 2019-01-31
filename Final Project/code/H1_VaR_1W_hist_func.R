# Historical VaR (1 week 2018-10-16 to 2018-10-22 )
H1_VaR_1W_hist_func <- function(x){x %>% 
  group_by(FundName) %>% 
  filter(FundName == "STANLIB Global Balanced Feeder Fund B") %>% 
  filter(Date >= as.Date("2018-10-16")) %>% 
  filter(Date <= as.Date("2018-10-22")) %>% 
  select(Date, FundName, Return) %>%
  ungroup() %>% 
  tbl_xts %>% 
  VaR(p = 0.95, method = "historical")
}