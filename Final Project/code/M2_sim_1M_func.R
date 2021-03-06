# Simulated process for 1 Month ahead

M2_sim_1M_func <- function(x){
  mysim_M2 <- ugarchsim(x, n.sim = 22, m.sim =100, startMethod = "sample")
  
  M2_sim <- fitted(mysim_M2) %>% # extract simulated values
    as.data.frame()       #covert to data frame
  
  # create simulation path dates and bind them with simulations
  date_col <- dateconverter(as.Date("2018-09-14"), as.Date("2018-10-15"), 
                            "weekdays")
  
  date_col <- data.frame(Date = date_col)
  
  M2_sim <- cbind(date_col, M2_sim) %>% 
    as.tbl() %>% tbl_xts() # make xts to work with performance analytics package
  
  # NB!! Data is in untidy format to create portfolio in performance analytics package
  
}