# Creating an equally weighted portfolio from the  simulation pathways


# set fund value at start
Fund_Size_at_Start <- 1000

# Creating equal weights at the beginning of simulation date
EW <- H1_sim_1W %>% xts_tbl() %>% 
  gather(Simulation, Return, -date) %>% 
  filter(date == first(date)) %>%
  ungroup() %>% 
  group_by(date) %>% 
  
  mutate(EqualWeights = 1/n()) %>% 
  ungroup() %>%
  select(-Return) %>% 
  spread(Simulation, EqualWeights) %>% 
  tbl_xts()



# Create the equally weighted portfolio
EW_RetPort <- 
  Return.portfolio(H1_sim_1W, weights = EW, verbose = TRUE,
                   contribution = TRUE, value = Fund_Size_at_Start, geometric = TRUE)
