# Simulated process for 1 Week ahead 

M4_sim_1W_func <- function(x){
  mysim_M4 <- ugarchsim(x, n.sim = 5, m.sim =100, startMethod = "sample")
  
  M4_sim <- fitted(mysim_M4) %>% # extract simulated values
    as.data.frame()       #covert to data frame
  
  # create simulation path dates and bind them with simulations
  date_col <- dateconverter(as.Date("2018-09-14"), as.Date("2018-09-20"), 
                            "weekdays")
  
  date_col <- data.frame(Date = date_col)
  
  M4_sim <- cbind(date_col, M4_sim) %>% 
    as.tbl() %>% tbl_xts() # make xts to work with performance analytics package
  
  # NB!! Data is in untidy format to create portfolio in performance analytics package
  
}