# Table of VaR estimates for historical and simulated data

M3_VaR_table <- function(){
  
  # Create matrix with the relative VaR estimates then name rows and cols
  M3_VaR <- matrix(c(M3_VaR_1W_sim, M3_VaR_1M_sim, M3_VaR_2M_sim, M3_VaR_1W_hist, M3_VaR_1M_hist, M3_VaR_2M_hist, abs(M3_VaR_1W_sim - M3_VaR_1W_hist), abs(M3_VaR_1M_sim - M3_VaR_1M_hist), abs(M3_VaR_2M_sim - M3_VaR_2M_hist) ), nrow=3, ncol=3,byrow=TRUE)
  M3_VaR <- M3_VaR %>% as.data.frame()
  colnames(M3_VaR) <- c("One-Week", "One-Month", "Two-Months")
  rownames(M3_VaR) <- c("FHS","Historical", "Difference")
  
  # convert matrix into table
  xtable(M3_VaR, caption = NULL, label = NULL, align = NULL, digits = 5,
         display = NULL, auto = FALSE)
}