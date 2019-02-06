# Table of VaR estimates for historical and simulated data

M2_VaR_table <- function(){
  
  # Create matrix with the relative VaR estimates then name rows and cols
  M2_VaR <- matrix(c(M2_VaR_1W_sim, M2_VaR_1M_sim, M2_VaR_2M_sim, M2_VaR_1W_hist, M2_VaR_1M_hist, M2_VaR_2M_hist, abs(M2_VaR_1W_sim - M2_VaR_1W_hist), abs(M2_VaR_1M_sim - M2_VaR_1M_hist), abs(M2_VaR_2M_sim - M2_VaR_2M_hist) ), nrow=3, ncol=3,byrow=TRUE)
  M2_VaR <- M2_VaR %>% as.data.frame()
  colnames(M2_VaR) <- c("One-Week", "One-Month", "Two-Months")
  rownames(M2_VaR) <- c("FHS","Historical", "Difference")
  
  # convert matrix into table
  xtable(M2_VaR, caption = NULL, label = NULL, align = NULL, digits = 5,
         display = NULL, auto = FALSE)
}