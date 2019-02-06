#================================================================================
# Financial Econometrics Final Project: Determining VaR for Funds using FHS
# by Richard Leigh
#================================================================================


# SET UP:

# packages
library(rmsfuns)
library(xtable)
library(Texevier)

packagestoload <- c("xts", "tidyverse", "devtools", "rugarch", "forecast", "tbl2xts", "PerformanceAnalytics", "dplyr",  
                    "lubridate", "glue", "ggthemes", "ggplot2", "Texevier", "parallel", "readr", "readxl")
load_pkg(packagelist = packagestoload)

# folders
Practical.loc.root <- file.path("C:/Users/Richard/Documents/ECONOMIC MASTERS/Financial Econometrics/Assignments/Final Project")
Practical.loc.subdirs <- c("data", "code", "bin")
PracLoc <- build_path(glue::glue("{Practical.loc.root}/{Practical.loc.subdirs}"))

rm(list=ls())


Data <- read_excel("data/Data.xlsx")

#==================================================================================================

# Calculating dlog returns from prices:
source("code/DLog_func.R")
Data <- DLog_func(Data)

# Calculating cumulative returns
source("code/Cumret_func.R")
Data <- Cumret_func(Data)

# Plot of Returns for different RiskRatings
source("code/H_ret_line.R")
H_ret_line(Data)
ggsave("bin/Ret_H.png", width = 7.5, height = 4.4)
dev.off()

source("code/M_ret_line.R")
M_ret_line(Data)
ggsave("bin/Ret_H.png", width = 7.5, height = 4.4)
dev.off()


# plotting cumulative returns for different RiskRatings
source("code/H_cumret_line.R")
H_cumret_line(Data)
ggsave("bin/Cumret_H.png", width = 8, height = 4.6)
dev.off()

source("code/M_cumret_line.R")
M_cumret_line(Data)
ggsave("bin/Cumret_M.png", width = 8, height = 4.6)
dev.off()



#==================================================================================================
# FILTERED HISTORICAL SIMULATION ANALYSIS:
#--------------------------------------------------------------------------------------------------

# STANLIB Global Balanced Feeder Fund B (H1):

# Fit appropriate GARCH model
source("code/H1_GARCH_func.R")
H1_fit <- H1_GARCH_func(Data)

# calculating standardized residuals
source("code/H1_st_resids_func.R")
H1_st_resids_func(Data)

# Simulation 1 Week ahead
source("code/H1_sim_1W_func.R")
H1_sim_1W <- H1_sim_1W_func(H1_fit)

# Histogram of 1 week simulations
source("code/H1_sim_1W_histogram.R")
H1_sim_1W_histogram(H1_sim_1W)
ggsave("bin/H1_1W_sim.png", width = 5, height = 5)
dev.off()

# Simulation 1 Month ahead
source("code/H1_sim_1M_func.R")
H1_sim_1M <- H1_sim_1M_func(H1_fit)

# Histogram of 1 month simulations
source("code/H1_sim_1M_histogram.R")
H1_sim_1M_histogram(H1_sim_1M)
ggsave("bin/H1_1M_sim.png", width = 5, height = 5)
dev.off()

# Simulation 2 Month ahead
source("code/H1_sim_2M_func.R")
H1_sim_2M <- H1_sim_2M_func(H1_fit)

# Histogram of 2 month simulations
source("code/H1_sim_2M_histogram.R")
H1_sim_2M_histogram(H1_sim_2M)
ggsave("bin/H1_2M_sim.png", width = 5, height = 5)
dev.off()
#------------------------------------------------------------------------------------------------------------
# VaR calculations

# VaR for simulations (1 Week from 2018-09-14 to 2018-09-20)
source("code/VaR_1W_sim_func.R")
H1_VaR_1W_sim <- VaR_1W_sim_func(H1_sim_1W)

# VaR for simulations (1 Month from 2018-09-14 to 2018-10-15)
source("code/VaR_1M_sim_func.R")
H1_VaR_1M_sim <- VaR_1M_sim_func(H1_sim_1M)

# VaR for simulations (2 Month from 2018-09-14 to 2018-11-15)
source("code/VaR_2M_sim_func.R")
H1_VaR_2M_sim <- VaR_2M_sim_func(H1_sim_2M)

# Historical VaR (1 Week 2018-09-14to 2018-09-20)
source("code/H1_VaR_1W_hist_func.R")
H1_VaR_1W_hist <- H1_VaR_1W_hist_func(Data)

# Historical VaR (1 Month 2018-09-14 to 2018-10-15 )
source("code/H1_VaR_1M_hist_func.R")
H1_VaR_1M_hist <- H1_VaR_1M_hist_func(Data)

# Historical VaR (2 Month 2018-09-14to 2018-11-15 )
source("code/H1_VaR_2M_hist_func.R")
H1_VaR_2M_hist <- H1_VaR_2M_hist_func(Data)

source("code/H1_VaR_table.R")
H1_VaR_table()

#==================================================================================================

# Analytics Ci Moderate Fund of Funds (H2):


# Fit appropriate GARCH model
source("code/H2_GARCH_func.R")
H2_fit <- H2_GARCH_func(Data)

# calculating standardized residuals
source("code/H2_st_resids_func.R")
H2_st_resids_func(Data)

# Simulation 1 Week ahead
source("code/H2_sim_1W_func.R")
H2_sim_1W <- H2_sim_1W_func(H2_fit)

# Histogram of 1 week simulations
source("code/H2_sim_1W_histogram.R")
H2_sim_1W_histogram(H2_sim_1W)
ggsave("bin/H2_1W_sim.png", width = 5, height = 5)
dev.off()

# Simulation 1 Month ahead
source("code/H2_sim_1M_func.R")
H2_sim_1M <- H2_sim_1M_func(H2_fit)

# Histogram of 1 month simulations
source("code/H2_sim_1M_histogram.R")
H2_sim_1M_histogram(H2_sim_1M)
ggsave("bin/H2_1M_sim.png", width = 5, height = 5)
dev.off()

# Simulation 2 Month ahead
source("code/H2_sim_2M_func.R")
H2_sim_2M <- H2_sim_2M_func(H2_fit)

# Histogram of 2 month simulations
source("code/H2_sim_2M_histogram.R")
H2_sim_2M_histogram(H2_sim_2M)
ggsave("bin/H2_2M_sim.png", width = 5, height = 5)
dev.off()
#------------------------------------------------------------------------------------------------------------
# VaR calculations

# VaR for simulations (1 Week from 2018-09-14 to 2018-09-20)
source("code/VaR_1W_sim_func.R")
H2_VaR_1W_sim <- VaR_1W_sim_func(H2_sim_1W)

# VaR for simulations (1 Month from 2018-09-14 to 2018-10-15)
source("code/VaR_1M_sim_func.R")
H2_VaR_1M_sim <- VaR_1M_sim_func(H2_sim_1M)

# VaR for simulations (2 Months from 2018-09-14 to 2018-11-15)
source("code/VaR_2M_sim_func.R")
H2_VaR_2M_sim <- VaR_2M_sim_func(H2_sim_2M)

# Historical VaR (1 Week from 2018-09-14 to 2018-09-20)
source("code/H2_VaR_1W_hist_func.R")
H2_VaR_1W_hist <- H2_VaR_1W_hist_func(Data)

# Historical VaR (1 Month from 2018-09-14 to 2018-10-15)
source("code/H2_VaR_1M_hist_func.R")
H2_VaR_1M_hist <- H2_VaR_1M_hist_func(Data)

# Historical VaR (2 Months from 2018-09-14 to 2018-11-15)
source("code/H2_VaR_2M_hist_func.R")
H2_VaR_2M_hist <- H2_VaR_2M_hist_func(Data)

source("code/H2_VaR_table.R")
H2_VaR_table()
#==================================================================================================

# Momentum International Balanced Feeder Fund A (H3):


# Fit appropriate GARCH model
source("code/H3_GARCH_func.R")
H3_fit <- H3_GARCH_func(Data)

# calculating standardized residuals
source("code/H3_st_resids_func.R")
H3_st_resids_func(Data)

# Simulation 1 Week ahead
source("code/H3_sim_1W_func.R")
H3_sim_1W <- H3_sim_1W_func(H3_fit)

# Histogram of 1 week simulations
source("code/H3_sim_1W_histogram.R")
H3_sim_1W_histogram(H3_sim_1W)
ggsave("bin/H3_1W_sim.png", width = 5, height = 5)
dev.off()

# Simulation 1 Month ahead
source("code/H3_sim_1M_func.R")
H3_sim_1M <- H3_sim_1M_func(H3_fit)

# Histogram of 1 month simulations
source("code/H3_sim_1M_histogram.R")
H3_sim_1M_histogram(H3_sim_1M)
ggsave("bin/H3_1M_sim.png", width = 5, height = 5)
dev.off()

# Simulation 2 Month ahead
source("code/H3_sim_2M_func.R")
H3_sim_2M <- H3_sim_2M_func(H3_fit)

# Histogram of 2 month simulations
source("code/H3_sim_2M_histogram.R")
H3_sim_2M_histogram(H3_sim_2M)
ggsave("bin/H3_2M_sim.png", width = 5, height = 5)
dev.off()
#------------------------------------------------------------------------------------------------------------
# VaR calculations

# VaR for simulations (1 Week from 2018-09-14 to 2018-09-20)
source("code/VaR_1W_sim_func.R")
H3_VaR_1W_sim <- VaR_1W_sim_func(H3_sim_1W)

# VaR for simulations (1 Month from 2018-09-14 to 2018-10-15)
source("code/VaR_1M_sim_func.R")
H3_VaR_1M_sim <- VaR_1M_sim_func(H3_sim_1M)

# VaR for simulations (2 Months from 2018-09-14 to 2018-11-15)
source("code/VaR_2M_sim_func.R")
H3_VaR_2M_sim <- VaR_2M_sim_func(H3_sim_2M)

# Historical VaR (1 Week from 2018-09-14 to 2018-09-20)
source("code/H3_VaR_1W_hist_func.R")
H3_VaR_1W_hist <- H3_VaR_1W_hist_func(Data)

# Historical VaR (1 Month from 2018-09-14 to 2018-10-15)
source("code/H3_VaR_1M_hist_func.R")
H3_VaR_1M_hist <- H3_VaR_1M_hist_func(Data)

# Historical VaR (2 Months from 2018-09-14 to 2018-11-15)
source("code/H3_VaR_2M_hist_func.R")
H3_VaR_2M_hist <- H3_VaR_2M_hist_func(Data)

source("code/H3_VaR_table.R")
H3_VaR_table()

#==================================================================================================

# Oasis Crescent Balanced High Equity Fund of Funds D (H4):

# Fit appropriate GARCH model
source("code/H4_GARCH_func.R")
H4_fit <- H4_GARCH_func(Data)

# calculating standardized residuals
source("code/H4_st_resids_func.R")
H4_st_resids_func(Data)

# Simulation 1 Week ahead
source("code/H4_sim_1W_func.R")
H4_sim_1W <- H4_sim_1W_func(H4_fit)

# Histogram of 1 week simulations
source("code/H4_sim_1W_histogram.R")
H4_sim_1W_histogram(H4_sim_1W)
ggsave("bin/H4_1W_sim.png", width = 5, height = 5)
dev.off()

# Simulation 1 Month ahead
source("code/H4_sim_1M_func.R")
H4_sim_1M <- H4_sim_1M_func(H4_fit)

# Histogram of 1 month simulations
source("code/H4_sim_1M_histogram.R")
H4_sim_1M_histogram(H4_sim_1M)
ggsave("bin/H4_1M_sim.png", width = 5, height = 5)
dev.off()

# Simulation 2 Month ahead
source("code/H4_sim_2M_func.R")
H4_sim_2M <- H4_sim_2M_func(H4_fit)

# Histogram of 2 month simulations
source("code/H4_sim_2M_histogram.R")
H4_sim_2M_histogram(H4_sim_2M)
ggsave("bin/H4_2M_sim.png", width = 5, height = 5)
dev.off()
#------------------------------------------------------------------------------------------------------------
# VaR calculations

# VaR for simulations (1 Week from 2018-09-14 to 2018-09-20)
source("code/VaR_1W_sim_func.R")
H4_VaR_1W_sim <- VaR_1W_sim_func(H4_sim_1W)

# VaR for simulations (1 Month from 2018-09-14 to 2018-10-15)
source("code/VaR_1M_sim_func.R")
H4_VaR_1M_sim <- VaR_1M_sim_func(H4_sim_1M)

# VaR for simulations (2 Months from 2018-09-14 to 2018-11-15)
source("code/VaR_2M_sim_func.R")
H4_VaR_2M_sim <- VaR_2M_sim_func(H4_sim_2M)

# Historical VaR (1 Week from 2018-09-14 to 2018-09-20)
source("code/H4_VaR_1W_hist_func.R")
H4_VaR_1W_hist <- H4_VaR_1W_hist_func(Data)

# Historical VaR (1 Month from 2018-09-14 to 2018-10-15)
source("code/H4_VaR_1M_hist_func.R")
H4_VaR_1M_hist <- H4_VaR_1M_hist_func(Data)

# Historical VaR (2 Months from 2018-09-14 to 2018-11-15)
source("code/H4_VaR_2M_hist_func.R")
H4_VaR_2M_hist <- H4_VaR_2M_hist_func(Data)

source("code/H4_VaR_table.R")
H4_VaR_table()

#==================================================================================================

# Allan Gray Tax-Free Balanced Fund A (M1):

# Fit appropriate GARCH model
source("code/M1_GARCH_func.R")
M1_fit <- M1_GARCH_func(Data)

# calculating standardized residuals
source("code/M1_st_resids_func.R")
M1_st_resids_func(Data)

# Simulation 1 Week ahead
source("code/M1_sim_1W_func.R")
M1_sim_1W <- M1_sim_1W_func(M1_fit)

# Histogram of 1 week simulations
source("code/M1_sim_1W_histogram.R")
M1_sim_1W_histogram(M1_sim_1W)
ggsave("bin/M1_1W_sim.png", width = 5, height = 5)
dev.off()

# Simulation 1 Month ahead
source("code/M1_sim_1M_func.R")
M1_sim_1M <- M1_sim_1M_func(M1_fit)

# Histogram of 1 month simulations
source("code/M1_sim_1M_histogram.R")
M1_sim_1M_histogram(M1_sim_1M)
ggsave("bin/M1_1M_sim.png", width = 5, height = 5)
dev.off()

# Simulation 2 Month ahead
source("code/M1_sim_2M_func.R")
M1_sim_2M <- M1_sim_2M_func(M1_fit)

# Histogram of 2 month simulations
source("code/M1_sim_2M_histogram.R")
M1_sim_2M_histogram(M1_sim_2M)
ggsave("bin/M1_2M_sim.png", width = 5, height = 5)
dev.off()
#------------------------------------------------------------------------------------------------------------
# VaR calculations

# VaR for simulations (1 Week from 2018-09-14 to 2018-09-20)
source("code/VaR_1W_sim_func.R")
M1_VaR_1W_sim <- VaR_1W_sim_func(M1_sim_1W)

# VaR for simulations (1 Month from 2018-09-14 to 2018-10-15)
source("code/VaR_1M_sim_func.R")
M1_VaR_1M_sim <- VaR_1M_sim_func(M1_sim_1M)

# VaR for simulations (2 Months from 2018-09-14 to 2018-11-15)
source("code/VaR_2M_sim_func.R")
M1_VaR_2M_sim <- VaR_2M_sim_func(M1_sim_2M)

# Historical VaR (1 Week from 2018-09-14 to 2018-09-20)
source("code/M1_VaR_1W_hist_func.R")
M1_VaR_1W_hist <- M1_VaR_1W_hist_func(Data)

# Historical VaR (1 Month from 2018-09-14 to 2018-10-15)
source("code/M1_VaR_1M_hist_func.R")
M1_VaR_1M_hist <- M1_VaR_1M_hist_func(Data)

# Historical VaR (2 Months from 2018-09-14 to 2018-11-15)
source("code/M1_VaR_2M_hist_func.R")
M1_VaR_2M_hist <- M1_VaR_2M_hist_func(Data)

source("code/M1_VaR_table.R")
M1_VaR_table()

#===================================================================================================

# Coronation Balanced Plus Fund A (M2):

# Fit appropriate GARCH model
source("code/M2_GARCH_func.R")
M2_fit <- M2_GARCH_func(Data)

# calculating standardized residuals
source("code/M2_st_resids_func.R")
M2_st_resids_func(Data)

# Simulation 1 Week ahead
source("code/M2_sim_1W_func.R")
M2_sim_1W <- M2_sim_1W_func(M2_fit)

# Histogram of 1 week simulations
source("code/M2_sim_1W_histogram.R")
M2_sim_1W_histogram(M2_sim_1W)
ggsave("bin/M2_1W_sim.png", width = 5, height = 5)
dev.off()

# Simulation 1 Month ahead
source("code/M2_sim_1M_func.R")
M2_sim_1M <- M2_sim_1M_func(M2_fit)

# Histogram of 1 month simulations
source("code/M2_sim_1M_histogram.R")
M2_sim_1M_histogram(M2_sim_1M)
ggsave("bin/M2_1M_sim.png", width = 5, height =5)
dev.off()

# Simulation 2 Month ahead
source("code/M2_sim_2M_func.R")
M2_sim_2M <- M2_sim_2M_func(M2_fit)

# Histogram of 2 month simulations
source("code/M2_sim_2M_histogram.R")
M2_sim_2M_histogram(M2_sim_2M)
ggsave("bin/M2_2M_sim.png", width = 5, height = 5)
dev.off()
#------------------------------------------------------------------------------------------------------------
# VaR calculations

# VaR for simulations (1 Week from 2018-09-14 to 2018-09-20)
source("code/VaR_1W_sim_func.R")
M2_VaR_1W_sim <- VaR_1W_sim_func(M2_sim_1W)

# VaR for simulations (1 Month from 2018-09-14 to 2018-10-15)
source("code/VaR_1M_sim_func.R")
M2_VaR_1M_sim <- VaR_1M_sim_func(M2_sim_1M)

# VaR for simulations (2 Months from 2018-09-14 to 2018-11-15)
source("code/VaR_2M_sim_func.R")
M2_VaR_2M_sim <- VaR_2M_sim_func(M2_sim_2M)

# Historical VaR (1 Week from 2018-09-14 to 2018-09-20)
source("code/M2_VaR_1W_hist_func.R")
M2_VaR_1W_hist <- M2_VaR_1W_hist_func(Data)

# Historical VaR (1 Month from 2018-09-14 to 2018-10-15)
source("code/M2_VaR_1M_hist_func.R")
M2_VaR_1M_hist <- M2_VaR_1M_hist_func(Data)

# Historical VaR (2 Months from 2018-09-14 to 2018-11-15)
source("code/M2_VaR_2M_hist_func.R")
M2_VaR_2M_hist <- M2_VaR_2M_hist_func(Data)

source("code/M2_VaR_table.R")
M2_VaR_table()

#==================================================================================================

# Kagiso Islamic Balanced Fund A (M3):

# Fit appropriate GARCH model
source("code/M3_GARCH_func.R")
M3_fit <- M3_GARCH_func(Data)

# calculating standardized residuals
source("code/M3_st_resids_func.R")
M3_st_resids_func(Data)

# Simulation 1 Week ahead
source("code/M3_sim_1W_func.R")
M3_sim_1W <- M3_sim_1W_func(M3_fit)

# Histogram of 1 week simulations
source("code/M3_sim_1W_histogram.R")
M3_sim_1W_histogram(M3_sim_1W)
ggsave("bin/M3_1W_sim.png", width = 5, height = 5)
dev.off()

# Simulation 1 Month ahead
source("code/M3_sim_1M_func.R")
M3_sim_1M <- M3_sim_1M_func(M3_fit)

# Histogram of 1 month simulations
source("code/M3_sim_1M_histogram.R")
M3_sim_1M_histogram(M3_sim_1M)
ggsave("bin/M3_1M_sim.png", width = 5, height = 5)
dev.off()

# Simulation 2 Month ahead
source("code/M3_sim_2M_func.R")
M3_sim_2M <- M3_sim_2M_func(M3_fit)

# Histogram of 2 month simulations
source("code/M3_sim_2M_histogram.R")
M3_sim_2M_histogram(M3_sim_2M)
ggsave("bin/M3_2M_sim.png", width =5, height = 5)
dev.off()
#------------------------------------------------------------------------------------------------------------
# VaR calculations

# VaR for simulations (1 Week from 2018-09-14 to 2018-09-20)
source("code/VaR_1W_sim_func.R")
M3_VaR_1W_sim <- VaR_1W_sim_func(M3_sim_1W)

# VaR for simulations (1 Month from 2018-09-14 to 2018-10-15)
source("code/VaR_1M_sim_func.R")
M3_VaR_1M_sim <- VaR_1M_sim_func(M3_sim_1M)

# VaR for simulations (2 Months from 2018-09-14 to 2018-11-15)
source("code/VaR_2M_sim_func.R")
M3_VaR_2M_sim <- VaR_2M_sim_func(M3_sim_2M)

# Historical VaR (1 Week from 2018-09-14 to 2018-09-20)
source("code/M3_VaR_1W_hist_func.R")
M3_VaR_1W_hist <- M3_VaR_1W_hist_func(Data)

# Historical VaR (1 Month from 2018-09-14 to 2018-10-15)
source("code/M3_VaR_1M_hist_func.R")
M3_VaR_1M_hist <- M3_VaR_1M_hist_func(Data)

# Historical VaR (2 Months from 2018-09-14 to 2018-11-15)
source("code/M3_VaR_2M_hist_func.R")
M3_VaR_2M_hist <- M3_VaR_2M_hist_func(Data)

source("code/M3_VaR_table.R")
M3_VaR_table()
#==================================================================================================

# Old Mutual Balanced Fund A:

# Fit appropriate GARCH model
source("code/M4_GARCH_func.R")
M4_fit <- M4_GARCH_func(Data)

# calculating standardized residuals
source("code/M4_st_resids_func.R")
M4_st_resids_func(Data)

# Simulation 1 Week ahead
source("code/M4_sim_1W_func.R")
M4_sim_1W <- M4_sim_1W_func(M4_fit)

# Histogram of 1 week simulations
source("code/M4_sim_1W_histogram.R")
M4_sim_1W_histogram(M4_sim_1W)
ggsave("bin/M4_1W_sim.png", width = 5, height = 5)
dev.off()

# Simulation 1 Month ahead
source("code/M4_sim_1M_func.R")
M4_sim_1M <- M4_sim_1M_func(M4_fit)

# Histogram of 1 month simulations
source("code/M4_sim_1M_histogram.R")
M4_sim_1M_histogram(M4_sim_1M)
ggsave("bin/M4_1M_sim.png", width = 5,height = 5)
dev.off()

# Simulation 2 Month ahead
source("code/M4_sim_2M_func.R")
M4_sim_2M <- M4_sim_2M_func(M4_fit)

# Histogram of 2 month simulations
source("code/M4_sim_2M_histogram.R")
M4_sim_2M_histogram(M4_sim_2M)
ggsave("bin/M4_2M_sim.png", width = 5, height = 5)
dev.off()
#------------------------------------------------------------------------------------------------------------
# VaR calculations

# VaR for simulations (1 Week from 2018-09-14 to 2018-09-20)
source("code/VaR_1W_sim_func.R")
M4_VaR_1W_sim <- VaR_1W_sim_func(M4_sim_1W)

# VaR for simulations (1 Month from 2018-09-14 to 2018-10-15)
source("code/VaR_1M_sim_func.R")
M4_VaR_1M_sim <- VaR_1M_sim_func(M4_sim_1M)

# VaR for simulations (2 Months from 2018-09-14 to 2018-11-15)
source("code/VaR_2M_sim_func.R")
M4_VaR_2M_sim <- VaR_2M_sim_func(M4_sim_2M)

# Historical VaR (1 Week from 2018-09-14 to 2018-09-20)
source("code/M4_VaR_1W_hist_func.R")
M4_VaR_1W_hist <- M4_VaR_1W_hist_func(Data)

# Historical VaR (1 Month from 2018-09-14 to 2018-10-15)
source("code/M4_VaR_1M_hist_func.R")
M4_VaR_1M_hist <- M4_VaR_1M_hist_func(Data)

# Historical VaR (2 Months from 2018-09-14 to 2018-11-15)
source("code/M4_VaR_2M_hist_func.R")
M4_VaR_2M_hist <- M4_VaR_2M_hist_func(Data)

source("code/M4_VaR_table.R")
M4_VaR_table()

