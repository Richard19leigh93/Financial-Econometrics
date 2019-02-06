# Historical VaR (1 week 2018-09-14 to 2018-09-20 )
M4_VaR_1W_hist_func <- function(x){x %>% 
    group_by(FundName) %>% 
    filter(FundName == "Old Mutual Balanced Fund A") %>% 
    filter(Date >= as.Date("2018-09-14")) %>% 
    filter(Date <= as.Date("2018-09-20")) %>% 
    select(Date, FundName, Return) %>%
    ungroup() %>% 
    tbl_xts %>% 
    VaR(p = 0.95, method = "historical")
}