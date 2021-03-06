# Histogram of 1 month simulations
H4_sim_1M_histogram <- function(x){x %>% xts_tbl() %>% 
    gather(Simulation, Return, -date) %>% 
    group_by(Simulation) %>% 
    ggplot() + geom_histogram(aes(x = Return, color = Simulation, fill = Simulation), position = "stack", binwidth = 0.001, alpha=0.7) + 
    theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = "grey50")) +  
    geom_vline(aes(xintercept=mean(Return)), color="blue", linetype="dashed", size=1) + 
    
    labs(title = "One Month Simulated Returns for Oasis", 
         x = " Simulated Returns", y = "Count")}