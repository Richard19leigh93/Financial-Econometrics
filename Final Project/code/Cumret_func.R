# Cumulative Returns

Cumret_func <- function(x){x %>% group_by(FundName) %>% 
    mutate(Cumulative_Return = cumsum(Return)) %>% 
    ungroup()}
