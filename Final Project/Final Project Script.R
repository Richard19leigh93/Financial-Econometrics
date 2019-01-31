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


# Plot of Returns for different RiskRatings
Data %>% 
  group_by(FundName) %>% 
  filter(RiskRating == "H") %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none") +
  facet_wrap(~FundName, scales = "free")

ggsave("bin/Ret_H.png")
dev.off()

Data %>% 
  group_by(FundName) %>% 
  filter(RiskRating == "HM") %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none") +
  facet_wrap(~FundName, scales = "free")

ggsave("bin/Ret_HM.png")
dev.off()

Data %>% 
  group_by(FundName) %>% 
  filter(RiskRating == "M") %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none") +
  facet_wrap(~FundName, scales = "free")

ggsave("bin/Ret_M.png")
dev.off()

# Calculating cumulative returns
source("code/Cumret_func.R")
Data <- Cumret_func(Data)

# plotting cumulative returns for different RiskRatings
Data %>%
  group_by(FundName) %>% 
  filter(RiskRating == "H") %>% 
  ggplot() + geom_line(aes(x = Date, y = Cumulative_Return, colour = FundName)) +theme(legend.position = "none")

ggsave("bin/Cumret_H.png")
dev.off()

Data %>%
  group_by(FundName) %>% 
  filter(RiskRating == "HM") %>% 
  ggplot() + geom_line(aes(x = Date, y = Cumulative_Return, colour = FundName)) +theme(legend.position = "none")

ggsave("bin/Cumret_HM.png")
dev.off()

Data %>%
  group_by(FundName) %>% 
  filter(RiskRating == "M") %>% 
  ggplot() + geom_line(aes(x = Date, y = Cumulative_Return, colour = FundName)) +theme(legend.position = "none")

ggsave("bin/Cumret_M.png")
dev.off()

#==================================================================================================
# FILTERED HISTORICAL ANALYSIS:
#--------------------------------------------------------------------------------------------------

# STANLIB Global Balanced Feeder Fund B (H1):

# Plot of Returns for different RiskRatings
Data %>% 
  group_by(FundName) %>% 
  filter(FundName == "STANLIB Global Balanced Feeder Fund B") %>% 
  filter(Date >= as.Date("2018-09-14"), Date <= as.Date("2018-10-15")) %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")



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
ggsave("bin/H1_W_sim.png")
dev.off()

# Simulation 1 Month ahead
source("code/H1_sim_1M_func.R")
H1_sim_1M <- H1_sim_1M_func(H1_fit)

# Histogram of 1 month simulations
source("code/H1_sim_1M_histogram.R")
H1_sim_1M_histogram(H1_sim_1M)
ggsave("bin/H1_M_sim.png")
dev.off()

# VaR calculations

# Historical VaR (1 Week 2018-10-16 to 2018-10-22)
source("code/H1_VaR_1W_hist_func.R")
H1_VaR_1W_hist_func(Data)

# Historical VaR (1 month 2018-10-16 to 2018-11-15 )
source("code/H1_VaR_1M_hist_func.R")
H1_VaR_1M_hist_func(Data)

# VaR for simulations (1 Week from 2018-10-16 to 2018-10-22)
source("code/H1_VaR_1W_sim_func.R")
H1_VaR_1W_sim_func(H1_sim_1W)

# VaR for simulations (1 Month from 2018-10-16 to 2018-11-15)
source("code/H1_VaR_1M_sim_func.R")
H1_VaR_1M_sim_func(H1_sim_1M)

H1_VaR <- matrix(c(-0.01960891, -0.0179255, -0.008946389, -0.01246722),ncol=2,byrow=TRUE)
colnames(H1_VaR) <- c("One-Week", "One-Month")
rownames(H1_VaR) <- c("FHS","Historical")

library(xtable)
H1_VaR <- H1_VaR %>%  tbl_df()

xtable(H1_VaR, caption = NULL, label = NULL, align = NULL, digits = NULL,
       display = NULL, auto = FALSE)


# Historical VaR (1 month prior 2018-10-15)
b <- Data %>% 
  group_by(FundName) %>% 
  filter(FundName == "STANLIB Global Balanced Feeder Fund B") %>% 
  filter(Date >= as.Date("2018-09-14")) %>% 
  filter(Date <= as.Date("2018-10-15")) %>% 
  select(Date, FundName, Return) %>%
  ungroup() %>% 
  tbl_xts %>% 
  VaR(p = 0.95, method = "historical")

b <- b %>% as.data.frame()

# Actual VaR (1 week after 2018-10-15)
V <- Data %>% 
  group_by(FundName) %>% 
  filter(FundName == "STANLIB Global Balanced Feeder Fund B") %>% 
  filter(Date >= as.Date("2018-10-16")) %>% 
  filter(Date <= as.Date("2018-11-15")) %>% 
  select(Date, FundName, Return) %>%
  ungroup() %>% 
  tbl_xts %>% 
  VaR(p = 0.95, method = "historical")

V <- V %>% as.data.frame()





# computing VaR from simulations
nu <- H1_fit@fit$coef["shape"] # extract (fitted) d.o.f. nu

H1_sim <- fitted(mysim_H1) # extract simulated values
H1_sim_sig <- sigma(mysim_H1) # extract simulated sigma
H1_sim_eps <- mysim_H1@simulation$residSim # extract epsilon

VaR.sim <- (H1_sim - H1_sim_eps) + H1_sim_sig * sqrt((nu-2)/nu) * qt(0.05, df = nu)



# plot histogram of simulated returns
H1_sim_tdy %>% group_by(simulation) %>% 
  ggplot() + geom_histogram(aes(x = Return), position = "stack", binwidth = 0.05, alpha=0.9) + theme(legend.position = "none") + 
  geom_vline(aes(xintercept=mean(Return)), color="blue", linetype="dashed", size=1) + 
  
  labs(title = "Simulations for STANLIB Global Balanced Feeder Fund B", 
       x = " Simulated Returns", y = "Count")

ggsave("Figures/Simulations/Returns/bal_H.png")
dev.off()

#==================================================================================================
# Analytics Ci Moderate Fund of Funds:

H2 <- Data %>%
  group_by(FundName) %>% 
  filter(FundName == "Analytics Ci Moderate Fund of Funds") %>% 
  select(Date, FundName, Price, Return, Cumulative_Return)

H2 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# reduce outliers
Extreme_Pos_Return <- 1
Extreme_Neg_Return <- -1  

H2 <- H2 %>% 
  mutate(Return = ifelse(Return > Extreme_Pos_Return, 
                         Extreme_Pos_Return, ifelse(Return < Extreme_Neg_Return, Extreme_Neg_Return, 
                                                    Return)))
H2 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# make time series
H2_ts <- H2 %>% 
  select(Date, FundName, Return) %>% 
  ungroup() %>% 
  tbl_xts()

#--------------------------------------------------------------------------------------------------

#   Fitting Appropriate GARCH model

# investigating which ARIMA model to use
load_pkg("parallel")
cl = makePSOCKcluster(10)

AC = autoarfima(as.numeric(H2_ts), ar.max = 2, ma.max = 2, 
                criterion = "AIC", method = "partial", arfima = FALSE, include.mean = NULL, 
                distribution.model = "norm", solver = "solnp", cluster = cl)
show(head(AC$rank.matrix))
# AR(2) MA(0) with mean is suggested

rm(cl)
rm(AC)

# fitting GARCH
garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[3]
                                            , garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(2, 0), include.mean = FALSE), 
                      distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                             "sged", "nig", "ghyp", "jsu")[1])

H2_fit = ugarchfit(spec = garch11, data = as.numeric(H2_ts), solver = "solnp")

H2_fit



# calculating standardized residuals
st_resids <- xts(H2_fit@fit$residuals/H2_fit@fit$sigma, order.by = index(H2_ts))

# convert to dataframe to use ggplot
st_resids <- st_resids %>% xts_tbl()
colnames(st_resids) <-  c("date", "st_resids")


# ACF's
st_resids <- st_resids %>% tbl_xts()
Acf(st_resids, main = "Standardized Residuals")
Acf(st_resids^2, main = "Squared Standardized Residuals")

# Simulation
mysim_H2 <- ugarchsim(H2_fit, n.sim = 60, m.sim =1000, startMethod = "sample")

# obtain fitted log returns
H2_sim <- fitted(mysim_H2)
# turn into data frame
H2_sim <- as.data.frame(H2_sim) 
# create new colomn to identify different simmulations (to needs to be same value as n.sim (simpath) above)
simulation <- seq(from = 1, to = 60, by = 1)
simulation <- as.data.frame(simulation)
H2_sim <- cbind(simulation, H2_sim)

# create new column to identify simulathion path periods (days) (also needs to be same value as n.sim)
H2_sim_tdy <- H2_sim %>% gather(simulation, Return)
simpath <- seq(from = 1, to = 60, by = 1)
simpath <- as.data.frame(simpath)
H2_sim_tdy <- cbind(simpath, H2_sim_tdy)

data.frame(date = seq(as.Date("2016-01-01"),
                      as.Date("2017-01-31"),"day"))

# plot histogram of simulated returns
H2_sim_tdy %>% group_by(simulation) %>% 
  ggplot() + geom_histogram(aes(x = Return), position = "stack", binwidth = 0.05, alpha=0.9) + theme(legend.position = "none") + 
  geom_vline(aes(xintercept=mean(Return)), color="blue", linetype="dashed", size=1) + 
  
  labs(title = "Simulations for Analytics Ci Moderate Fund of Funds", 
       x = " Simulated Returns", y = "Count")

ggsave("Figures/Simulations/Returns/bal_H.png")
dev.off()

#==================================================================================================
# Momentum International Balanced Feeder Fund A:

H3 <- Data %>%
  group_by(FundName) %>% 
  filter(FundName == "Momentum International Balanced Feeder Fund A") %>% 
  select(Date, FundName, Price, Return, Cumulative_Return)

H3 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# reduce outliers
Extreme_Pos_Return <- 3
Extreme_Neg_Return <- -3  

H3 <- H3 %>% 
  mutate(Return = ifelse(Return > Extreme_Pos_Return, 
                         Extreme_Pos_Return, ifelse(Return < Extreme_Neg_Return, Extreme_Neg_Return, 
                                                    Return)))
H3 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# make time series
H3_ts <- H3 %>% 
  select(Date, FundName, Return) %>% 
  ungroup() %>% 
  tbl_xts()

#--------------------------------------------------------------------------------------------------

#   Fitting Appropriate GARCH model

# investigating which ARIMA model to use
load_pkg("parallel")
cl = makePSOCKcluster(10)

AC = autoarfima(as.numeric(H3_ts), ar.max = 2, ma.max = 2, 
                criterion = "AIC", method = "partial", arfima = FALSE, include.mean = NULL, 
                distribution.model = "norm", solver = "solnp", cluster = cl)
show(head(AC$rank.matrix))
# AR(0) MA(2) with mean is suggested but we use AR(2) Ma(0) which is the second best option

rm(cl)
rm(AC)

# fitting GARCH
garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[1]
                                            , garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(2, 0), include.mean = FALSE), 
                      distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                             "sged", "nig", "ghyp", "jsu")[1])

H3_fit = ugarchfit(spec = garch11, data = as.numeric(H3_ts), solver = "solnp")

H3_fit



# calculating standardized residuals
st_resids <- xts(H3_fit@fit$residuals/H3_fit@fit$sigma, order.by = index(H3_ts))

# convert to dataframe to use ggplot
st_resids <- st_resids %>% xts_tbl()
colnames(st_resids) <-  c("date", "st_resids")


# ACF's
st_resids <- st_resids %>% tbl_xts()
Acf(st_resids, main = "Standardized Residuals")
Acf(st_resids^2, main = "Squared Standardized Residuals")

# Simulation
mysim_H3 <- ugarchsim(H3_fit, n.sim = 60, m.sim =1000, startMethod = "sample")

# obtain fitted log returns
H3_sim <- fitted(mysim_H3)
# turn into data frame
H3_sim <- as.data.frame(H3_sim) 
# create new colomn to identify different simmulations (to needs to be same value as n.sim (simpath) above)
simulation <- seq(from = 1, to = 60, by = 1)
simulation <- as.data.frame(simulation)
H3_sim <- cbind(simulation, H3_sim)

# create new column to identify simulathion path periods (days) (also needs to be same value as n.sim)
H3_sim_tdy <- H3_sim %>% gather(simulation, Return)
simpath <- seq(from = 1, to = 60, by = 1)
simpath <- as.data.frame(simpath)
H3_sim_tdy <- cbind(simpath, H3_sim_tdy)

data.frame(date = seq(as.Date("2016-01-01"),
                      as.Date("2017-01-31"),"day"))

# plot histogram of simulated returns
H3_sim_tdy %>% group_by(simulation) %>% 
  ggplot() + geom_histogram(aes(x = Return), position = "stack", binwidth = 0.05, alpha=0.9) + theme(legend.position = "none") + 
  geom_vline(aes(xintercept=mean(Return)), color="blue", linetype="dashed", size=1) + 
  
  labs(title = "Simulations for Momentum International Balanced Feeder Fund A", 
       x = " Simulated Returns", y = "Count")

ggsave("Figures/Simulations/Returns/bal_H.png")
dev.off()


#==================================================================================================
# Oasis Crescent Balanced High Equity Fund of Funds D:

H4 <- Data %>%
  group_by(FundName) %>% 
  filter(FundName == "Oasis Crescent Balanced High Equity Fund of Funds D") %>% 
  select(Date, FundName, Price, Return, Cumulative_Return)

H4 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# reduce outliers
Extreme_Pos_Return <- 1.25
Extreme_Neg_Return <- -1.25 

H4 <- H4 %>% 
  mutate(Return = ifelse(Return > Extreme_Pos_Return, 
                         Extreme_Pos_Return, ifelse(Return < Extreme_Neg_Return, Extreme_Neg_Return, 
                                                    Return)))
H4 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# make time series
H4_ts <- H4 %>% 
  select(Date, FundName, Return) %>% 
  ungroup() %>% 
  tbl_xts()

#--------------------------------------------------------------------------------------------------

#   Fitting Appropriate GARCH model

# investigating which ARIMA model to use
load_pkg("parallel")
cl = makePSOCKcluster(10)

AC = autoarfima(as.numeric(H4_ts), ar.max = 2, ma.max = 2, 
                criterion = "AIC", method = "partial", arfima = FALSE, include.mean = NULL, 
                distribution.model = "norm", solver = "solnp", cluster = cl)
show(head(AC$rank.matrix))
# AR(0) MA(0) with mean is suggested but we use AR(1) Ma(0) which is the third best option

rm(cl)
rm(AC)

# fitting GARCH
garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[3]
                                            , garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(1, 0), include.mean = FALSE), 
                      distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                             "sged", "nig", "ghyp", "jsu")[1])

H4_fit = ugarchfit(spec = garch11, data = as.numeric(H4_ts), solver = "solnp")

H4_fit



# calculating standardized residuals
st_resids <- xts(H4_fit@fit$residuals/H4_fit@fit$sigma, order.by = index(H4_ts))

# convert to dataframe to use ggplot
st_resids <- st_resids %>% xts_tbl()
colnames(st_resids) <-  c("date", "st_resids")


# ACF's
st_resids <- st_resids %>% tbl_xts()
Acf(st_resids, main = "Standardized Residuals")
Acf(st_resids^2, main = "Squared Standardized Residuals")

# Simulation
mysim_H4 <- ugarchsim(H4_fit, n.sim = 60, m.sim =1000, startMethod = "sample")

# obtain fitted log returns
H4_sim <- fitted(mysim_H4)
# turn into data frame
H4_sim <- as.data.frame(H4_sim) 
# create new colomn to identify different simmulations (to needs to be same value as n.sim (simpath) above)
simulation <- seq(from = 1, to = 60, by = 1)
simulation <- as.data.frame(simulation)
H4_sim <- cbind(simulation, H4_sim)

# create new column to identify simulathion path periods (days) (also needs to be same value as n.sim)
H4_sim_tdy <- H4_sim %>% gather(simulation, Return)
simpath <- seq(from = 1, to = 60, by = 1)
simpath <- as.data.frame(simpath)
H4_sim_tdy <- cbind(simpath, H4_sim_tdy)

data.frame(date = seq(as.Date("2016-01-01"),
                      as.Date("2017-01-31"),"day"))

# plot histogram of simulated returns
H4_sim_tdy %>% group_by(simulation) %>% 
  ggplot() + geom_histogram(aes(x = Return), position = "stack", binwidth = 0.05, alpha=0.9) + theme(legend.position = "none") + 
  geom_vline(aes(xintercept=mean(Return)), color="blue", linetype="dashed", size=1) + 
  
  labs(title = "Simulations for Oasis Crescent Balanced High Equity Fund of Funds D", 
       x = " Simulated Returns", y = "Count")

ggsave("Figures/Simulations/Returns/bal_H.png")
dev.off()

#==================================================================================================

# Allan Gray Tax-Free Balanced Fund A:

M1 <- Data %>%
  group_by(FundName) %>% 
  filter(FundName == "Allan Gray Tax-Free Balanced Fund A") %>% 
  select(Date, FundName, Price, Return, Cumulative_Return)

M1 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# reduce outliers
Extreme_Pos_Return <- 1
Extreme_Neg_Return <- -1 

M1 <- M1 %>% 
  mutate(Return = ifelse(Return > Extreme_Pos_Return, 
                         Extreme_Pos_Return, ifelse(Return < Extreme_Neg_Return, Extreme_Neg_Return, 
                                                    Return)))
M1 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# make time series
M1_ts <- M1 %>% 
  select(Date, FundName, Return) %>% 
  ungroup() %>% 
  tbl_xts()

#--------------------------------------------------------------------------------------------------

#   Fitting Appropriate GARCH model

# investigating which ARIMA model to use
load_pkg("parallel")
cl = makePSOCKcluster(10)

AC = autoarfima(as.numeric(M1_ts), ar.max = 2, ma.max = 2, 
                criterion = "AIC", method = "partial", arfima = FALSE, include.mean = NULL, 
                distribution.model = "norm", solver = "solnp", cluster = cl)
show(head(AC$rank.matrix))
# AR(0) MA(1) with mean is suggested but we use AR(1) Ma(0) which is the second best option

rm(cl)
rm(AC)

# fitting GARCH
garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[3]
                                            , garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(1, 0), include.mean = FALSE), 
                      distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                             "sged", "nig", "ghyp", "jsu")[1])

M1_fit = ugarchfit(spec = garch11, data = as.numeric(M1_ts), solver = "solnp")

M1_fit



# calculating standardized residuals
st_resids <- xts(M1_fit@fit$residuals/M1_fit@fit$sigma, order.by = index(M1_ts))

# convert to dataframe to use ggplot
st_resids <- st_resids %>% xts_tbl()
colnames(st_resids) <-  c("date", "st_resids")


# ACF's
st_resids <- st_resids %>% tbl_xts()
Acf(st_resids, main = "Standardized Residuals")
Acf(st_resids^2, main = "Squared Standardized Residuals")

# Simulation
mysim_M1 <- ugarchsim(m1_fit, n.sim = 60, m.sim =1000, startMethod = "sample")

# obtain fitted log returns
M1_sim <- fitted(mysim_M1)
# turn into data frame
M1_sim <- as.data.frame(M1_sim) 
# create new colomn to identify different simmulations (to needs to be same value as n.sim (simpath) above)
simulation <- seq(from = 1, to = 60, by = 1)
simulation <- as.data.frame(simulation)
M1_sim <- cbind(simulation, M1_sim)

# create new column to identify simulathion path periods (days) (also needs to be same value as n.sim)
M1_sim_tdy <- M1_sim %>% gather(simulation, Return)
simpath <- seq(from = 1, to = 60, by = 1)
simpath <- as.data.frame(simpath)
M1_sim_tdy <- cbind(simpath, M1_sim_tdy)

data.frame(date = seq(as.Date("2016-01-01"),
                      as.Date("2017-01-31"),"day"))

# plot histogram of simulated returns
M1_sim_tdy %>% group_by(simulation) %>% 
  ggplot() + geom_histogram(aes(x = Return), position = "stack", binwidth = 0.05, alpha=0.9) + theme(legend.position = "none") + 
  geom_vline(aes(xintercept=mean(Return)), color="blue", linetype="dashed", size=1) + 
  
  labs(title = "Simulations for Allan Gray Tax-Free Balanced Fund A", 
       x = " Simulated Returns", y = "Count")

ggsave("Figures/Simulations/Returns/bal_H.png")
dev.off()

#===================================================================================================

# Coronation Balanced Plus Fund A:

M2 <- Data %>%
  group_by(FundName) %>% 
  filter(FundName == "Coronation Balanced Plus Fund A") %>% 
  select(Date, FundName, Price, Return, Cumulative_Return)

M2 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# reduce outliers
Extreme_Pos_Return <- 1.25
Extreme_Neg_Return <- -1.25

M2 <- M2 %>% 
  mutate(Return = ifelse(Return > Extreme_Pos_Return, 
                         Extreme_Pos_Return, ifelse(Return < Extreme_Neg_Return, Extreme_Neg_Return, 
                                                    Return)))
M2 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# make time series
M2_ts <- M2 %>% 
  select(Date, FundName, Return) %>% 
  ungroup() %>% 
  tbl_xts()

#--------------------------------------------------------------------------------------------------

#   Fitting Appropriate GARCH model

# investigating which ARIMA model to use
cl = makePSOCKcluster(10)

AC = autoarfima(as.numeric(M2_ts), ar.max = 2, ma.max = 2, 
                criterion = "AIC", method = "partial", arfima = FALSE, include.mean = NULL, 
                distribution.model = "norm", solver = "solnp", cluster = cl)

show(head(AC$rank.matrix))
# AR(0) MA(1) with mean is suggested but we use AR(1) Ma(0) which is the second best option

rm(cl)
rm(AC)

# fitting GARCH
garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[3]
                                            , garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(1, 0), include.mean = FALSE), 
                      distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                             "sged", "nig", "ghyp", "jsu")[1])

M2_fit = ugarchfit(spec = garch11, data = as.numeric(M2_ts), solver = "solnp")

M2_fit



# calculating standardized residuals
st_resids <- xts(M2_fit@fit$residuals/M2_fit@fit$sigma, order.by = index(M2_ts))

# convert to dataframe to use ggplot
st_resids <- st_resids %>% xts_tbl()
colnames(st_resids) <-  c("date", "st_resids")


# ACF's
st_resids <- st_resids %>% tbl_xts()
Acf(st_resids, main = "Standardized Residuals")
Acf(st_resids^2, main = "Squared Standardized Residuals")

# Simulation
mysim_M2 <- ugarchsim(M2_fit, n.sim = 60, m.sim =1000, startMethod = "sample")

# obtain fitted log returns
M2_sim <- fitted(mysim_M2)
# turn into data frame
M2_sim <- as.data.frame(M2_sim) 
# create new colomn to identify different simmulations (to needs to be same value as n.sim (simpath) above)
simulation <- seq(from = 1, to = 60, by = 1)
simulation <- as.data.frame(simulation)
M2_sim <- cbind(simulation, M2_sim)

# create new column to identify simulathion path periods (days) (also needs to be same value as n.sim)
M2_sim_tdy <- M2_sim %>% gather(simulation, Return)
simpath <- seq(from = 1, to = 60, by = 1)
simpath <- as.data.frame(simpath)
M2_sim_tdy <- cbind(simpath, M2_sim_tdy)

data.frame(date = seq(as.Date("2016-01-01"),
                      as.Date("2017-01-31"),"day"))

# plot histogram of simulated returns
M2_sim_tdy %>% group_by(simulation) %>% 
  ggplot() + geom_histogram(aes(x = Return), position = "stack", binwidth = 0.05, alpha=0.9) + theme(legend.position = "none") + 
  geom_vline(aes(xintercept=mean(Return)), color="blue", linetype="dashed", size=1) + 
  
  labs(title = "Simulations for Coronation Balanced Plus Fund A", 
       x = " Simulated Returns", y = "Count")

ggsave("Figures/Simulations/Returns/bal_H.png")
dev.off()

#==================================================================================================

# Kagiso Islamic Balanced Fund A:

M3 <- Data %>%
  group_by(FundName) %>% 
  filter(FundName == "Kagiso Islamic Balanced Fund A") %>% 
  select(Date, FundName, Price, Return, Cumulative_Return)

M3 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# reduce outliers
Extreme_Pos_Return <- 1
Extreme_Neg_Return <- -1

M3 <- M3 %>% 
  mutate(Return = ifelse(Return > Extreme_Pos_Return, 
                         Extreme_Pos_Return, ifelse(Return < Extreme_Neg_Return, Extreme_Neg_Return, 
                                                    Return)))
M3 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# make time series
M3_ts <- M3 %>% 
  select(Date, FundName, Return) %>% 
  ungroup() %>% 
  tbl_xts()

#--------------------------------------------------------------------------------------------------

#   Fitting Appropriate GARCH model

# investigating which ARIMA model to use
load_pkg("parallel")
cl = makePSOCKcluster(10)

AC = autoarfima(as.numeric(M3_ts), ar.max = 2, ma.max = 2, 
                criterion = "AIC", method = "partial", arfima = FALSE, include.mean = NULL, 
                distribution.model = "norm", solver = "solnp", cluster = cl)
show(head(AC$rank.matrix))
# AR(0) MA(1) with mean is suggested but we use AR(1) Ma(0) which is the second best option

rm(cl)
rm(AC)

# fitting GARCH
garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[1]
                                            , garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(1, 0), include.mean = FALSE), 
                      distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                             "sged", "nig", "ghyp", "jsu")[1])

M3_fit = ugarchfit(spec = garch11, data = as.numeric(M3_ts), solver = "solnp")

M3_fit



# calculating standardized residuals
st_resids <- xts(M3_fit@fit$residuals/M3_fit@fit$sigma, order.by = index(M3_ts))

# convert to dataframe to use ggplot
st_resids <- st_resids %>% xts_tbl()
colnames(st_resids) <-  c("date", "st_resids")


# ACF's
st_resids <- st_resids %>% tbl_xts()
Acf(st_resids, main = "Standardized Residuals")
Acf(st_resids^2, main = "Squared Standardized Residuals")

# Simulation
mysim_M3 <- ugarchsim(M3_fit, n.sim = 60, m.sim =1000, startMethod = "sample")

# obtain fitted log returns
M3_sim <- fitted(mysim_M3)
# turn into data frame
M3_sim <- as.data.frame(M3_sim) 
# create new colomn to identify different simmulations (to needs to be same value as n.sim (simpath) above)
simulation <- seq(from = 1, to = 60, by = 1)
simulation <- as.data.frame(simulation)
M3_sim <- cbind(simulation, M3_sim)

# create new column to identify simulathion path periods (days) (also needs to be same value as n.sim)
M3_sim_tdy <- M3_sim %>% gather(simulation, Return)
simpath <- seq(from = 1, to = 60, by = 1)
simpath <- as.data.frame(simpath)
M3_sim_tdy <- cbind(simpath, M3_sim_tdy)

data.frame(date = seq(as.Date("2016-01-01"),
                      as.Date("2017-01-31"),"day"))

# plot histogram of simulated returns
M3_sim_tdy %>% group_by(simulation) %>% 
  ggplot() + geom_histogram(aes(x = Return), position = "stack", binwidth = 0.05, alpha=0.9) + theme(legend.position = "none") + 
  geom_vline(aes(xintercept=mean(Return)), color="blue", linetype="dashed", size=1) + 
  
  labs(title = "Simulations for Kagiso Islamic Balanced Fund A", 
       x = " Simulated Returns", y = "Count")

ggsave("Figures/Simulations/Returns/bal_H.png")
dev.off()

#==================================================================================================

# Old Mutual Balanced Fund A:

M4 <- Data %>%
  group_by(FundName) %>% 
  filter(FundName == "Old Mutual Balanced Fund A") %>% 
  select(Date, FundName, Price, Return, Cumulative_Return)

M4 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# reduce outliers
Extreme_Pos_Return <- 1
Extreme_Neg_Return <- -1

M4 <- M4 %>% 
  mutate(Return = ifelse(Return > Extreme_Pos_Return, 
                         Extreme_Pos_Return, ifelse(Return < Extreme_Neg_Return, Extreme_Neg_Return, 
                                                    Return)))
M4 %>% 
  ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")
dev.off()


# make time series
M4_ts <- M4 %>% 
  select(Date, FundName, Return) %>% 
  ungroup() %>% 
  tbl_xts()

#--------------------------------------------------------------------------------------------------

#   Fitting Appropriate GARCH model

# investigating which ARIMA model to use
load_pkg("parallel")
cl = makePSOCKcluster(10)

AC = autoarfima(as.numeric(M4_ts), ar.max = 2, ma.max = 2, 
                criterion = "AIC", method = "partial", arfima = FALSE, include.mean = NULL, 
                distribution.model = "norm", solver = "solnp", cluster = cl)
show(head(AC$rank.matrix))
# AR(2) MA(2) with mean is suggested 

rm(cl)
rm(AC)

# fitting GARCH
garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[3]
                                            , garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(2, 2), include.mean = FALSE), 
                      distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                             "sged", "nig", "ghyp", "jsu")[1])

M4_fit = ugarchfit(spec = garch11, data = as.numeric(M4_ts), solver = "solnp")

M4_fit



# calculating standardized residuals
st_resids <- xts(M4_fit@fit$residuals/M4_fit@fit$sigma, order.by = index(M4_ts))

# convert to dataframe to use ggplot
st_resids <- st_resids %>% xts_tbl()
colnames(st_resids) <-  c("date", "st_resids")


# ACF's
st_resids <- st_resids %>% tbl_xts()
Acf(st_resids, main = "Standardized Residuals")
Acf(st_resids^2, main = "Squared Standardized Residuals")

# Simulation
mysim_M4 <- ugarchsim(M4_fit, n.sim = 60, m.sim =1000, startMethod = "sample")

# obtain fitted log returns
M4_sim <- fitted(mysim_M4)
# turn into data frame
M4_sim <- as.data.frame(M4_sim) 
# create new colomn to identify different simmulations (to needs to be same value as n.sim (simpath) above)
simulation <- seq(from = 1, to = 60, by = 1)
simulation <- as.data.frame(simulation)
M4_sim <- cbind(simulation, M4_sim)

# create new column to identify simulathion path periods (days) (also needs to be same value as n.sim)
M4_sim_tdy <- M4_sim %>% gather(simulation, Return)
simpath <- seq(from = 1, to = 60, by = 1)
simpath <- as.data.frame(simpath)
M4_sim_tdy <- cbind(simpath, M4_sim_tdy)

data.frame(date = seq(as.Date("2016-01-01"),
                      as.Date("2017-01-31"),"day"))

# plot histogram of simulated returns
M4_sim_tdy %>% group_by(simulation) %>% 
  ggplot() + geom_histogram(aes(x = Return), position = "stack", binwidth = 0.05, alpha=0.9) + theme(legend.position = "none") + 
  geom_vline(aes(xintercept=mean(Return)), color="blue", linetype="dashed", size=1) + 
  
  labs(title = "Simulations for Old Mutual Balanced Fund A", 
       x = " Simulated Returns", y = "Count")

ggsave("Figures/Simulations/Returns/bal_H.png")
dev.off()

#==================================================================================================



# Calculating Historical VaR for STANLIB Global Balanced Feeder Fund B
H1 <- Data %>% 
  filter(FundName == "STANLIB Global Balanced Feeder Fund B") %>% 
  select(Date, Return) %>% 
  tbl_xts()
  



quantile(H1,p=0.05)

VaR(H1, p = 0.95, method = "modified")

# Calculating FHS VaR for STANLIB Global Balanced Feeder Fund B

H1_sim %>%
  tbl_xts()

porteqw <- Return.portfolio(H1_sim, weight = NULL, geometric = FALSE)

porteqw %>% xts_tbl() %>% 
  mutate(Cumret = cumsum(portfolio.returns)) %>% 
  ggplot() + geom_line(aes(x = date, y = Cumret)) + theme(legend.position = "none")



H1_sim_ts <- H1_sim_tdy %>% 
  select(simpath, Return) %>% 
  ungroup() %>% 
  tbl_xts()

quantile(H1_ts,p=0.05)
quantile()

