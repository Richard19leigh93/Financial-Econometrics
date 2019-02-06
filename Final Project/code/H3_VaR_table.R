# Table of VaR estimates for historical and simulated data

H3_VaR_table <- function(){
  
  # Create matrix with the relative VaR estimates then name rows and cols
  H3_VaR <- matrix(c(H3_VaR_1W_sim, H3_VaR_1M_sim, H3_VaR_2M_sim, H3_VaR_1W_hist, H3_VaR_1M_hist, H3_VaR_2M_hist, abs(H3_VaR_1W_hist - H3_VaR_1W_sim), abs(H3_VaR_1M_hist - H3_VaR_1M_sim), abs(H3_VaR_2M_hist - H3_VaR_2M_sim) ), nrow=3, ncol=3,byrow=TRUE)
  H3_VaR <- H3_VaR %>% as.data.frame()
  colnames(H3_VaR) <- c("One-Week", "One-Month", "Two-Months")
  rownames(H3_VaR) <- c("FHS","Historical", "Difference")
  
  # convert matrix into table
  xtable(H3_VaR, caption = NULL, label = NULL, align = NULL, digits = 5,
         display = NULL, auto = FALSE)
}