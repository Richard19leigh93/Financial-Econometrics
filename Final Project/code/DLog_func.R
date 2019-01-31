# DLog Returns in decimals

DLog_func <- function(x) {x %>% 
    group_by(FundName) %>% 
    arrange(Date) %>% 
    mutate(Return = (log(Price)-log(lag(Price)))) %>%
    mutate(Return = coalesce(Return, 0)) %>% 
    ungroup()}
