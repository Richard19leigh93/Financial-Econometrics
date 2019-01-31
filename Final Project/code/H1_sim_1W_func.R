# Simulated process for 1 Week ahead 

H1_sim_1W_func <- function(x){
  mysim_H1 <- ugarchsim(x, n.sim = 5, m.sim =100, startMethod = "sample")
  
  H1_sim <- fitted(mysim_H1) %>% # extract simulated values
    as.data.frame()       #covert to data frame
  
  # create simulation path dates and bind them with simulations
  date_col <- dateconverter(as.Date("2018-10-16"), as.Date("2018-10-22"), 
                            "weekdays")
  
  date_col <- data.frame(Date = date_col)
  
  H1_sim <- cbind(date_col, H1_sim) %>% 
    as.tbl() %>% tbl_xts() # make xts to work with performance analytics package
  
  # NB!! Data is in untidy format to create portfolio in performance analytics package
  
}