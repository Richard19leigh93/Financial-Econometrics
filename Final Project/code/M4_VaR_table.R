# Table of VaR estimates for historical and simulated data

M4_VaR_table <- function(){
  
  # Create matrix with the relative VaR estimates then name rows and cols
  M4_VaR <- matrix(c(M4_VaR_1W_sim, M4_VaR_1M_sim, M4_VaR_2M_sim, M4_VaR_1W_hist, M4_VaR_1M_hist, M4_VaR_2M_hist, abs(M4_VaR_1W_sim - M4_VaR_1W_hist), abs(M4_VaR_1M_sim - M4_VaR_1M_hist), abs(M4_VaR_2M_sim - M4_VaR_2M_hist) ), nrow=3, ncol=3,byrow=TRUE)
  M4_VaR <- M4_VaR %>% as.data.frame()
  colnames(M4_VaR) <- c("One-Week", "One-Month", "Two-Months")
  rownames(M4_VaR) <- c("FHS","Historical", "Difference")
  
  # convert matrix into table
  xtable(M4_VaR, caption = NULL, label = NULL, align = NULL, digits = 5,
         display = NULL, auto = FALSE)
}