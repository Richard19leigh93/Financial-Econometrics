##############################################
#ASSIGNMENT: PRACTICAL 2 (BONUS)
##############################################



# SETUP

install.packages("rmsfuns")
library(rmsfuns)
#creating a group consisting of all of the packages to load them at once
packagestoload <- c("xts", "tidyverse", "tbl2xts", "PerformanceAnalytics", 
                    "lubridate", "glue", "dplyr", "ggplot2", "ggthemes")
load_pkg(packagelist = packagestoload)

#Creating the relevent folders
Practical.loc.root <- file.path("C:/Users/Richard/Documents/ECONOMIC MASTERS/Financial Econometrics/Assignments/index_return_comparison")
Practical.loc.subdirs <- c("data", "code", "bin")
PracLoc <- build_path(glue::glue("{Practical.loc.root}/{Practical.loc.subdirs}"))

#loading data
library(tidyverse)
Daily_TRI <- 
  read_rds("C:/Users/Richard/Documents/ECONOMIC MASTERS/Financial Econometrics/Assignments/index_return_comparison/data/Fin_Data_SA_US_NKY.rds")

#omit NA values from data
Daily_TRI_NA_rem <- 
  Daily_TRI %>% 
  na.omit()


# 1) Calculating cap-weighted monthly index returns for every sector for each country

#take TRI of last day of each month per ticker
Cap_W_Month_Indx_Ret <- Daily_TRI_NA_rem %>%
  mutate(Year_Month = format(date, "%Y%B")) %>% 
  group_by(Year_Month, Ticker) %>% 
  filter(date == last(date)) %>%
  ungroup() %>% 
  
#calculate monthly returns from TRI
  group_by(Ticker) %>%
  mutate(Monthly_Ret = TRI/lag(TRI) - 1) %>% 
  mutate(Monthly_Ret = coalesce(Monthly_Ret, 0)) %>%  
  ungroup() %>% 

#add up market cap per sector
  group_by(Universe, BICS_LEVEL_1_SECTOR_NAME, date) %>% 
  mutate(sec_cap = sum(Market.Cap)) %>% 
  
#divide each company by market sector to get weight of index
  mutate(w_index = Market.Cap/sec_cap) %>% 
  
#multiply each observation price by weight
  mutate(w_Portfolio_Ret = sum(Monthly_Ret*w_index)) %>%
  summarise(w_Monthly_Ret = mean(w_Portfolio_Ret))

#graph cap-weighted returns by sector and by country
Cap_W_Month_Indx_Ret %>%
  arrange(date) %>% 
  ggplot() +
  geom_line(aes(date, w_Monthly_Ret, color=BICS_LEVEL_1_SECTOR_NAME))+
  facet_wrap(~Universe) +
  theme(axis.text.x = element_text(angle=90))
  

# 2) Calculate the sharpe ratio for the equally weighted by country, cap-weighted index returns since 2017
Cap_W_Month_Indx_Ret_Sharpe <- Cap_W_Month_Indx_Ret %>% 
  group_by(Universe, BICS_LEVEL_1_SECTOR_NAME) %>% 
  summarise(Sharpe = mean(w_Monthly_Ret, na.rm = TRUE) / sd(w_Monthly_Ret, na.rm = TRUE)) %>%
  ungroup() 


# 3) Plot cumulative returns for financial sectors
#fitering financial sector and calculate cumulative returns
financial_sec_data <- Cap_W_Month_Indx_Ret %>% 
  filter(BICS_LEVEL_1_SECTOR_NAME %in% c( "Financials")) %>% 
  group_by(Universe) %>%
  mutate(cum_Ret = cumprod( 1 + w_Monthly_Ret))

#plot financial sectors
ggplot(financial_sec_data) +
  geom_line(aes(date, cum_Ret, color=Universe))


# 4) JALSHAll index returns of 95% 
#Isolate JALSHAll and calculate returns
JALSHAll_95_index_ret <- Daily_TRI_NA_rem %>%
  group_by(Ticker) %>% 
  filter(Universe %in% c("JALSHAll")) %>% 
  mutate(daily_Ret = TRI/lag(TRI) - 1) %>%
  mutate(daily_Ret = coalesce(daily_Ret, 0)) %>% 
  ungroup() %>% 
#Calculate weighted returns and filter 95%
  group_by(date) %>% 
  mutate(jalsh_cap = sum(Market.Cap)) %>% 
  mutate(jalsh_w = Market.Cap/jalsh_cap) %>% 
  mutate(Cum_w = cumsum(jalsh_w)) %>% 
  filter(Cum_w >= 0.05) %>%
  mutate(w_portfolio_Ret = sum(daily_Ret*jalsh_w)) %>%
  summarise(jalsh_index_Ret = mean(w_portfolio_Ret)) %>% 
  ungroup()
#Plotting index returns
ggplot(JALSHAll_95_index_ret) +
  geom_line(aes(x = date, y = jalsh_index_Ret))


# 5) 60 day standard deviation for the Materials sectors of each country
materials_60_day_rolling <- Daily_TRI_NA_rem %>% 
  filter(BICS_LEVEL_1_SECTOR_NAME %in% "Materials") %>% 
  group_by(Ticker) %>% 
  mutate(Returns = TRI/lag(TRI)-1) %>%
  mutate(Returns = coalesce(Returns, 0)) %>%
  ungroup() %>% 
  group_by(Universe, date) %>% 
  mutate(cap_sec = sum(Market.Cap)) %>% 
  mutate(w_index = Market.Cap/cap_sec) %>% 
  mutate(w_Ret = sum(Returns*w_index)) %>% 
  summarise(materials_Ret = mean(w_Ret)) %>%
  ungroup() %>% 
  tbl_xts(., cols_to_xts = "materials_Ret", spread_by = "Universe")

chart.RollingPerformance(R = materials_60_day_rolling, FUN = "sd", 
                         width = 60, main = "Rolling 60 Day Standard Deviation", 
                         legend.loc = "bottomleft")





remove(Daily_TRI_NA_rem)
remove(Cap_W_Month_Indx_Ret)
remove(plot_data)
remove(JALSHAll_95_index_ret)
remove(materials_60_day_rolling)
rm(list=ls())

?facet_wrap()

