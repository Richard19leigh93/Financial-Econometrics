# Table of VaR estimates for historical and simulated data

H4_VaR_table <- function(){
  
  # Create matrix with the relative VaR estimates then name rows and cols
  H4_VaR <- matrix(c(H4_VaR_1W_sim, H4_VaR_1M_sim, H4_VaR_2M_sim, H4_VaR_1W_hist, H4_VaR_1M_hist, H4_VaR_2M_hist, abs(H4_VaR_1W_hist - H4_VaR_1W_sim), abs(H4_VaR_1M_hist - H4_VaR_1M_sim), abs(H4_VaR_2M_hist - H4_VaR_2M_sim) ), nrow=3, ncol=3,byrow=TRUE)
  H4_VaR <- H4_VaR %>% as.data.frame()
  colnames(H4_VaR) <- c("One-Week", "One-Month", "Two-Months")
  rownames(H4_VaR) <- c("FHS","Historical", "Difference")
  
  # convert matrix into table
  xtable(H4_VaR, caption = NULL, label = NULL, align = NULL, digits = 5,
         display = NULL, auto = FALSE)
}