# Table of VaR estimates for historical and simulated data

M1_VaR_table <- function(){
  
  # Create matrix with the relative VaR estimates then name rows and cols
  M1_VaR <- matrix(c(M1_VaR_1W_sim, M1_VaR_1M_sim, M1_VaR_2M_sim, M1_VaR_1W_hist, M1_VaR_1M_hist, M1_VaR_2M_hist, abs(M1_VaR_1W_sim - M1_VaR_1W_hist), abs(M1_VaR_1M_sim - M1_VaR_1M_hist), abs(M1_VaR_2M_sim - M1_VaR_2M_hist) ), nrow=3, ncol=3,byrow=TRUE)
  M1_VaR <- M1_VaR %>% as.data.frame()
  colnames(M1_VaR) <- c("One-Week", "One-Month", "Two-Months")
  rownames(M1_VaR) <- c("FHS","Historical", "Difference")
  
  # convert matrix into table
  table <- xtable(M1_VaR, caption = "Comparrison of VaR estimates between FHS and Historical for the three time periods \\label{tabH1}", digits = 6)
  print.xtable(table, 
               # tabular.environment = "longtable",
               floating = TRUE,
               table.placement = 'H', 
               # scalebox = 0.3, 
               comment = FALSE,
               caption.placement = 'bottom'
               )
  

}