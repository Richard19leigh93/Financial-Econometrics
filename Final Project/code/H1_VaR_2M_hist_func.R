# Historical VaR (2 month 2018-09-14 to 2018-11-15 )
H1_VaR_2M_hist_func <- function(x){x %>% 
    group_by(FundName) %>% 
    filter(FundName == "STANLIB Global Balanced Feeder Fund B") %>% 
    filter(Date >= as.Date("2018-09-14")) %>% 
    filter(Date <= as.Date("2018-11-15")) %>% 
    select(Date, FundName, Return) %>%
    ungroup() %>% 
    tbl_xts %>% 
    VaR(p = 0.95, method = "historical")
}