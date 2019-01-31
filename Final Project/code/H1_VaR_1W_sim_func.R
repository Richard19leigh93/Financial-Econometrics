# VaR for simulations (1 Week from 2018-10-16 to 2018-10-22)
H1_VaR_1W_sim_func <- function(x){x %>% xts_tbl() %>% 
  gather(Sim, Return, -date) %>% 
  tbl_xts() %>% 
  VaR(p = 0.95, method = "modified")
  
  }