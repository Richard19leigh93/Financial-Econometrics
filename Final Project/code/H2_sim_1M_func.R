# Simulated process for 1 Month ahead

H2_sim_1M_func <- function(x){
  mysim_H2 <- ugarchsim(x, n.sim = 22, m.sim =100, startMethod = "sample")
  
  H2_sim <- fitted(mysim_H2) %>% # extract simulated values
    as.data.frame()       #covert to data frame
  
  # create simulation path dates and bind them with simulations
  date_col <- dateconverter(as.Date("2018-09-14"), as.Date("2018-10-15"), 
                            "weekdays")
  
  date_col <- data.frame(Date = date_col)
  
  H2_sim <- cbind(date_col, H2_sim) %>% 
    as.tbl() %>% tbl_xts() # make xts to work with performance analytics package
  
  # NB!! Data is in untidy format to create portfolio in performance analytics package
  
}