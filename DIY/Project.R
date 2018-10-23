#=================================================
#FINANCIAL ECONOMETRICS ASSIGNMENT:
#=================================================

# SET UP:

#packages
library(rmsfuns)
library(xtable)
library(Texevier)
library(readr)

packagestoload <- c("xts", "tidyverse", "devtools", "rugarch", "forecast", "tbl2xts", "PerformanceAnalytics", 
                    "lubridate", "glue", "ggthemes", "Texevier")
load_pkg(packagelist = packagestoload)

#folders
Practical.loc.root <- file.path("C:/Users/Richard/Documents/ECONOMIC MASTERS/Financial Econometrics/Assignments/DIY")
Practical.loc.subdirs <- c("data", "code", "bin")
PracLoc <- build_path(glue::glue("{Practical.loc.root}/{Practical.loc.subdirs}"))

rm(list=ls())
setwd("C:/Users/Richard/Documents/ECONOMIC MASTERS/Financial Econometrics/Assignments/DIY")
#loading data
dailydata <- read_csv("https://raw.githubusercontent.com/Nicktz/ExDat/master/extdata/findata.csv", 
                      col_types = cols(.default = "d", Date = "D"))

#clean column names as we know they are all JSE
colnames(dailydata) <- colnames(dailydata) %>% gsub("JSE.", "", .) %>% gsub(".Close", 
                                                                "", .)
#======================================================================================================================

# 1) CREATING A TABLE OF FIRST AND SECOND MOMENTS OR RETURNS

#gathering data to make tidy
dailydata_cl <- dailydata %>% gather(Fund, Close, - Date)

#calculating dlog returns returns
dailydata_cl <- dailydata_cl %>% 
  group_by(Fund) %>% 
  mutate(lreturns = (log(Close) - log(lag(Close)))*100) %>% 
  mutate(lreturns = coalesce(lreturns, 0)) %>% 
  ungroup() 

#calculating simple returns
dailydata_cl <- dailydata_cl %>% 
  group_by(Fund) %>% 
  mutate(returns = (Close/lag(Close) - 1)*100) %>% 
  mutate(returns = coalesce(returns, 0)) %>% 
  ungroup() 
#----------------------------------------------------------------------------------------------------------------------

#filter dates 2006-2008
date2006 <- dailydata_cl %>% 
  group_by(Fund) %>% 
  filter(Date >= as.Date("2006-01-01")) %>%
  filter(Date <= as.Date("2008-12-31"))


#calculating first (mean) and second (variance) moments of funds from 2006-2008

#ABSAP
ABSP_06 <- date2006 %>% 
  filter(Fund == "ABSP") %>% 
  pull(lreturns)

mABSP_06 <- mean(ABSP_06)
vABSP_06 <- var(ABSP_06)
ABSP_06 <- c(mABSP_06, vABSP_06) #grouping mean and variance

#BVT
BVT_06 <- date2006 %>% 
  filter(Fund == "BVT") %>% 
  pull(lreturns)

mBVT_06 <- mean(BVT_06)
vBVT_06 <- var(BVT_06)
BVT_06 <- c(mBVT_06, vBVT_06)

#FSR
FSR_06 <- date2006 %>% 
  filter(Fund == "FSR") %>% 
  pull(lreturns)

mFSR_06 <- mean(FSR_06)
vFSR_06 <- var(FSR_06)
FSR_06 <- c(mFSR_06, vFSR_06)

#NBKP
NBKP_06 <- date2006 %>% 
  filter(Fund == "NBKP") %>% 
  pull(lreturns)

mNBKP_06 <- mean(NBKP_06)
vNBKP_06 <- var(NBKP_06)
NBKP_06 <- c(mNBKP_06, vNBKP_06)

#RMH
RMH_06 <- date2006 %>% 
  filter(Fund == "RMH") %>% 
  pull(lreturns)

mRMH_06 <- mean(RMH_06)
vRMH_06 <- var(RMH_06)
RMH_06 <- c(mRMH_06, vRMH_06)

#SBK
SBK_06 <- date2006 %>% 
  filter(Fund == "SBK") %>% 
  pull(lreturns)

mSBK_06 <- mean(SBK_06)
vSBK_06 <- var(SBK_06)
SBK_06 <- c(mSBK_06, vSBK_06)

#SLM
SLM_06 <- date2006 %>% 
  filter(Fund == "SLM") %>% 
  pull(lreturns)

mSLM_06 <- mean(SLM_06)
vSLM_06 <- var(SLM_06)
SLM_06 <- c(mSLM_06, vSLM_06)

#table of 2006
M <- c("Mean", "Variance")

mean_var06 <- data.frame(Moment = M, ABSP = ABSP_06, BVT = BVT_06, 
                         FSR = FSR_06, NBKP = NBKP_06, RMH = RMH_06, SBK = SBK_06, SLM = SLM_06)
####saving data####
data.location <- paste0(getwd(), "/data/")
write_rds(mean_var06, path = paste0(data.location, "mean_var06.rds"))

xtable(mean_var06)
#----------------------------------------------------------------------------------------------------------------------

#filter dates 2010-2013
date2010 <- dailydata_cl %>% 
  group_by(Fund) %>% 
  filter(Date >= as.Date("2010-01-01")) %>%
  filter(Date <= as.Date("2013-12-31"))



#calculating first (mean) and second (variance) moments of funds from 2006-2008

#ABSAP
ABSP_10 <- date2010 %>% 
  filter(Fund == "ABSP") %>% 
  pull(lreturns)

mABSP_10 <- mean(ABSP_10)
vABSP_10 <- var(ABSP_10)
ABSP_10 <- c(mABSP_10, vABSP_10) #grouping mean and variance

#BVT
BVT_10 <- date2010 %>% 
  filter(Fund == "BVT") %>% 
  pull(lreturns)

mBVT_10 <- mean(BVT_10)
vBVT_10 <- var(BVT_10)
BVT_10 <- c(mBVT_10, vBVT_10)

#FSR
FSR_10 <- date2010 %>% 
  filter(Fund == "FSR") %>% 
  pull(lreturns)

mFSR_10 <- mean(FSR_10)
vFSR_10 <- var(FSR_10)
FSR_10 <- c(mFSR_10, vFSR_10)

#NBKP
NBKP_10 <- date2010 %>% 
  filter(Fund == "NBKP") %>% 
  pull(lreturns)

mNBKP_10 <- mean(NBKP_10)
vNBKP_10 <- var(NBKP_10)
NBKP_10 <- c(mNBKP_10, vNBKP_10)

#RMH
RMH_10 <- date2010 %>% 
  filter(Fund == "RMH") %>% 
  pull(lreturns)

mRMH_10 <- mean(RMH_10)
vRMH_10 <- var(RMH_10)
RMH_10 <- c(mRMH_10, vRMH_10)

#SBK
SBK_10 <- date2010 %>% 
  filter(Fund == "SBK") %>% 
  pull(lreturns)

mSBK_10 <- mean(SBK_10)
vSBK_10 <- var(SBK_10)
SBK_10 <- c(mSBK_10, vSBK_10)

#SLM
SLM_10 <- date2010 %>% 
  filter(Fund == "SLM") %>% 
  pull(lreturns)

mSLM_10 <- mean(SLM_10)
vSLM_10 <- var(SLM_10)
SLM_10 <- c(mSLM_10, vSLM_10)

#table of 2010
mean_var10 <- data.frame(Moment = M, ABSP = ABSP_10, BVT = BVT_10, 
                         FSR = FSR_10, NBKP = NBKP_10, RMH = RMH_10, SBK = SBK_10, SLM = SLM_10)

write_rds(mean_var10, path = paste0(data.location, "mean_var10.rds"))

xtable(mean_var10)
#----------------------------------------------------------------------------------------------------------------------

#filter dates 2016-2017
date2016 <- dailydata_cl %>% 
  group_by(Fund) %>% 
  filter(Date >= as.Date("2016-01-01")) %>%
  filter(Date <= as.Date("2017-07-31"))



#calculating first (mean) and second (variance) moments of funds from 2016-2017

#ABSAP
ABSP_16 <- date2016 %>% 
  filter(Fund == "ABSP") %>% 
  pull(lreturns)

mABSP_16 <- mean(ABSP_16)
vABSP_16 <- var(ABSP_16)
ABSP_16 <- c(mABSP_16, vABSP_16) #grouping mean and variance

#BVT
BVT_16 <- date2016 %>% 
  filter(Fund == "BVT") %>% 
  pull(lreturns)

mBVT_16 <- mean(BVT_16)
vBVT_16 <- var(BVT_16)
BVT_16 <- c(mBVT_16, vBVT_16)

#FSR
FSR_16 <- date2016 %>% 
  filter(Fund == "FSR") %>% 
  pull(lreturns)

mFSR_16 <- mean(FSR_16)
vFSR_16 <- var(FSR_16)
FSR_16 <- c(mFSR_16, vFSR_16)

#NBKP
NBKP_16 <- date2016 %>% 
  filter(Fund == "NBKP") %>% 
  pull(lreturns)

mNBKP_16 <- mean(NBKP_16)
vNBKP_16 <- var(NBKP_16)
NBKP_16 <- c(mNBKP_16, vNBKP_16)

#RMH
RMH_16 <- date2016 %>% 
  filter(Fund == "RMH") %>% 
  pull(lreturns)

mRMH_16 <- mean(RMH_16)
vRMH_16 <- var(RMH_16)
RMH_16 <- c(mRMH_16, vRMH_16)

#SBK
SBK_16 <- date2016 %>% 
  filter(Fund == "SBK") %>% 
  pull(lreturns)

mSBK_16 <- mean(SBK_16)
vSBK_16 <- var(SBK_16)
SBK_16 <- c(mSBK_16, vSBK_16)

#SLM
SLM_16 <- date2016 %>% 
  filter(Fund == "SLM") %>% 
  pull(lreturns)

mSLM_16 <- mean(SLM_16)
vSLM_16 <- var(SLM_16)
SLM_16 <- c(mSLM_16, vSLM_16)

#table of 2010
mean_var16 <- data.frame(Moment = M, ABSP = ABSP_16, BVT = BVT_16, 
                         FSR = FSR_16, NBKP = NBKP_16, RMH = RMH_16, SBK = SBK_16, SLM = SLM_16)

xtable(mean_var16)


#       Comparing the tables; means have increased while variances decreased
#======================================================================================================================

# 2) REFERENCING

#@tsay1989

#@article{tsay1989testing,
#  title={Testing and modeling threshold autoregressive processes},
#  author={Tsay, Ruey S},
#  journal={Journal of the American statistical association},
#  volume={84},
#  number={405},
#  pages={231--240},
#  year={1989},
#  publisher={Taylor \& Francis Group}
#}
#======================================================================================================================

# 3) Unconditional Correlations between stocks:

dailydata %>% cor()

#======================================================================================================================

# 4) PLOT GARCH MODELS FOR EACH SERIES

#set up GARCH(1,1) model
garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[1]
                                            , garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
                      distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                             "sged", "nig", "ghyp", "jsu")[1])

#ABSP
ABSP_ts <- dailydata_cl %>% 
  filter(Fund == "ABSP") %>% 
  select(Date, lreturns) %>% 
  tbl_xts
  

ABSAP_fit = ugarchfit(spec = garch11, data = as.numeric(ABSP_ts), solver = "solnp")

plot(ABSAP_fit, which = "all")

umysim <- ugarchsim(ABSP_fit, n.sim = 100, m.sim =1, startMethod = "sample")

#BVT
BVT_ts <- dailydata_cl %>% 
  filter(Fund == "BVT") %>% 
  select(Date, lreturns) %>% 
  tbl_xts


BVT_fit = ugarchfit(spec = garch11, data = as.numeric(BVT_ts), solver = "solnp")

plot(BVT_fit, which = "all")


#FSR
FSR_ts <- dailydata_cl %>% 
  filter(Fund == "FSR") %>% 
  select(Date, lreturns) %>% 
  tbl_xts


FSR_fit = ugarchfit(spec = garch11, data = as.numeric(FSR_ts), solver = "solnp")

plot(FSR_fit, which = "all")

#NBKP
NBKP_ts <- dailydata_cl %>% 
  filter(Fund == "NBKP") %>% 
  select(Date, lreturns) %>% 
  tbl_xts


NBKP_fit = ugarchfit(spec = garch11, data = as.numeric(NBKP_ts), solver = "solnp")

plot(NBKP_fit, which = "all")

#RMH
RMH_ts <- dailydata_cl %>% 
  filter(Fund == "RMH") %>% 
  select(Date, lreturns) %>% 
  tbl_xts


RMH_fit = ugarchfit(spec = garch11, data = as.numeric(RMH_ts), solver = "solnp")

plot(RMH_fit, which = "all")

#SBK
SBK_ts <- dailydata_cl %>% 
  filter(Fund == "SBK") %>% 
  select(Date, lreturns) %>% 
  tbl_xts


SBK_fit = ugarchfit(spec = garch11, data = as.numeric(SBK_ts), solver = "solnp")

plot(SBK_fit, which = "all")

#SLM
SLM_ts <- dailydata_cl %>% 
  filter(Fund == "SLM") %>% 
  select(Date, lreturns) %>% 
  tbl_xts


SLM_fit = ugarchfit(spec = garch11, data = as.numeric(SLM_ts), solver = "solnp")

plot(SLM_fit, which = "all")
#======================================================================================================================

# 5) PLOTTING CUMULATIVE RETURNS of EQUALLY WEIGHTED PORTFOLIO

# dlog returns:
rtn <- (diff(log(dailydata %>% arrange(Date) %>% tbl_xts()), 
             lag = 1)) * 100

# drop the first observation and corresponding date:
rtn <- rtn[-1, ]
# Center the data:
rtn <- scale(rtn, center = T, scale = F)

porteqw <- Return.portfolio(rtn, weight = NULL, geometric = FALSE)

porteqw %>% xts_tbl() %>% 
  mutate(Cumret = cumsum(portfolio.returns)) %>% 
  ggplot() + geom_line(aes(x = date, y = Cumret)) + theme(legend.position = "none")



rm(list=ls())
