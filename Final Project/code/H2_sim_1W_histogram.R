# Histogram of 1 week simulations
H2_sim_1W_histogram <- function(x){x %>% xts_tbl() %>% 
    gather(Simulation, Return, -date) %>% 
    group_by(Simulation) %>% 
    ggplot() + geom_histogram(aes(x = Return, color = Simulation, fill = Simulation), position = "stack", binwidth = 0.0005, alpha=0.7) + 
    theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = "grey50")) +  
    geom_vline(aes(xintercept=mean(Return)), color="blue", linetype="dashed", size=1) + 
    
    labs(title = "One Week Simulated Returns for Analytics", 
         x = " Simulated Returns", y = "Count")}