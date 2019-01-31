# Histogram of 1 month simulations
H1_sim_1M_histogram <- function(x){x %>% xts_tbl() %>% 
  gather(Simulation, Return, -date) %>% 
  group_by(Simulation) %>% 
  ggplot() + geom_histogram(aes(x = Return), position = "stack", binwidth = 0.001, alpha=0.9) + theme(legend.position = "none") + 
  geom_vline(aes(xintercept=mean(Return)), color="blue", linetype="dashed", size=1) + 
  
  labs(title = "One Month Simulated Returns for STANLIB", 
       x = " Simulated Returns", y = "Count")}