# Simulated process for 2 Month ahead

H3_sim_2M_func <- function(x){
  mysim_H3 <- ugarchsim(x, n.sim = 45, m.sim =100, startMethod = "sample")
  
  H3_sim <- fitted(mysim_H3) %>% # extract simulated values
    as.data.frame()       #covert to data frame
  
  # create simulation path dates and bind them with simulations
  date_col <- dateconverter(as.Date("2018-09-14"), as.Date("2018-11-15"), 
                            "weekdays")
  
  date_col <- data.frame(Date = date_col)
  
  H3_sim <- cbind(date_col, H3_sim) %>% 
    as.tbl() %>% tbl_xts() # make xts to work with performance analytics package
  
  # NB!! Data is in untidy format to create portfolio in performance analytics package
  
}