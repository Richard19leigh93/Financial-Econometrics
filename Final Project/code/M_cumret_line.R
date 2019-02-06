M_cumret_line <- function(x){x %>%
    group_by(FundName) %>% 
    filter(RiskRating == "M") %>% 
    ggplot() + geom_line(aes(x = Date, y = Cumulative_Return, colour = FundName)) + theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = "grey50"), panel.grid.major = element_line(colour = "grey80"), panel.grid.minor = element_line(colour = "grey80")) + 
    
    labs(title = "Cumulative Log_Returns for Medium Risk Funds", 
         x = "Year", y = "Cumulative Returns")
}