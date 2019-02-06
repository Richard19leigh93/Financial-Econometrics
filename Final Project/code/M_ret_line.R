# Plot of Returns for different RiskRatings
M_ret_line <- function(x){x %>% 
    group_by(FundName) %>% 
    filter(RiskRating == "M") %>% 
    ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + 
    theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = "grey50"), panel.grid.major = element_line(colour = "grey80"), panel.grid.minor = element_line(colour = "grey80")) +
    facet_wrap(~FundName, scales = "free") + 
    
    labs(title = "Log-Returns for Medium Risk Funds", 
         x = "Year", y = "Returns")
}