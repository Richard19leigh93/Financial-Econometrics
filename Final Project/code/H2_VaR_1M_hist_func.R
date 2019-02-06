# Historical VaR (1 week 2018-09-14 to 2018-09-20 )
H2_VaR_1M_hist_func <- function(x){x %>% 
    group_by(FundName) %>% 
    filter(FundName == "Analytics Ci Moderate Fund of Funds") %>% 
    filter(Date >= as.Date("2018-09-14")) %>% 
    filter(Date <= as.Date("2018-10-15")) %>% 
    select(Date, FundName, Return) %>%
    ungroup() %>% 
    tbl_xts %>% 
    VaR(p = 0.95, method = "historical")
}