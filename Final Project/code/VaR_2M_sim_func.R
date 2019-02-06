# VaR for simulations (1 Month from 2018-09-14 to 2018-11-15)
VaR_2M_sim_func <- function(x){x %>% xts_tbl() %>% 
    gather(Sim, Return, -date) %>% 
    tbl_xts() %>% 
    VaR(p = 0.95, method = "modified")
  
}