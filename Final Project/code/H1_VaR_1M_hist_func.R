# Historical VaR (1 month 2018-10-16 to 2018-11-15 )
H1_VaR_1M_hist_func <- function(x){x %>% 
    group_by(FundName) %>% 
    filter(FundName == "STANLIB Global Balanced Feeder Fund B") %>% 
    filter(Date >= as.Date("2018-10-16")) %>% 
    filter(Date <= as.Date("2018-11-15")) %>% 
    select(Date, FundName, Return) %>%
    ungroup() %>% 
    tbl_xts %>% 
    VaR(p = 0.95, method = "historical")
}