# Table of VaR estimates for historical and simulated data

H1_VaR_table <- function(){

# Create matrix with the relative VaR estimates then name rows and cols
  H1_VaR <- matrix(c(H1_VaR_1W_sim, H1_VaR_1M_sim, H1_VaR_2M_sim, H1_VaR_1W_hist, H1_VaR_1M_hist, H1_VaR_2M_hist, abs(H1_VaR_1W_hist - H1_VaR_1W_sim), abs(H1_VaR_1M_hist - H1_VaR_1M_sim), abs(H1_VaR_2M_hist - H1_VaR_2M_sim)), nrow=3, ncol=3,byrow=TRUE)
  H1_VaR <- H1_VaR %>% as.data.frame()
  colnames(H1_VaR) <- c("One-Week", "One-Month", "Two-Months")
  rownames(H1_VaR) <- c("FHS","Historical", "Difference")

# convert matrix into table
  xtable(H1_VaR, caption = NULL, label = NULL, align = NULL, digits = 6,
       display = NULL, auto = FALSE)
}