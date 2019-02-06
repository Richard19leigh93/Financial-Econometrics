# VaR for simulations (1 Week from 2018-09-14 to 2018-09-20)
VaR_1W_sim_func <- function(x){x %>% xts_tbl() %>% 
    gather(Sim, Return, -date) %>% 
    tbl_xts() %>% 
    VaR(p = 0.95, method = "modified")
  
}