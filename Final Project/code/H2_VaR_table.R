# Table of VaR estimates for historical and simulated data

H2_VaR_table <- function(){
  
  # Create matrix with the relative VaR estimates then name rows and cols
  H2_VaR <- matrix(c(H2_VaR_1W_sim, H2_VaR_1M_sim, H2_VaR_2M_sim, H2_VaR_1W_hist, H2_VaR_1M_hist, H2_VaR_2M_hist, abs(H2_VaR_1W_hist - H2_VaR_1W_sim), abs(H2_VaR_1M_hist - H2_VaR_1M_sim), abs(H2_VaR_2M_hist - H2_VaR_2M_sim) ), nrow=3, ncol=3,byrow=TRUE)
  H2_VaR <- H2_VaR %>% as.data.frame()
  colnames(H2_VaR) <- c("One-Week", "One-Month", "Two-Months")
  rownames(H2_VaR) <- c("FHS","Historical", "Difference")
  
  # convert matrix into table
  xtable(H2_VaR, caption = NULL, label = NULL, align = NULL, digits = 5,
         display = NULL, auto = FALSE)
}