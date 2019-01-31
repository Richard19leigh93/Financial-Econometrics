Information
-----------

This README demonstrates the coding procedure behind the assignment.
This file shows the Filtered Historical Simulation (FHS) analysis of
Value at Risk (VaR) for STANLIB Global Balanced Feeder Fund B only,
since the procedure is repeated identically for each fund. Only the
specified fits of ARIMA and GARCH models vary slightly with each fund.

Lastly, the code functions also contain details in the form of comments
to explain the relevant code chunks and their purpose.

### Set Up

The relevant packages are loaded along with the Data.

    library(rmsfuns)
    library(xtable)
    library(Texevier)

    packagestoload <- c("xts", "tidyverse", "devtools", "rugarch", "forecast", "tbl2xts", "PerformanceAnalytics", "dplyr", "lubridate", "glue", "ggthemes", "ggplot2", "Texevier", "parallel", "readr", "readxl")
    load_pkg(packagelist = packagestoload)

    # removing packages to avoid clutter in global enviroment
    rm(list=ls())

    # loading Data
    Data <- read_excel("data/Data.xlsx")

1. Log Returns
--------------

Log return for STANLIB Global Balanced Feeder Fund B are first
calculated from prices.

    source("code/DLog_func.R")
    Data <- DLog_func(Data)

    Data %>% 
      group_by(FundName) %>% 
      filter(FundName == "STANLIB Global Balanced Feeder Fund B") %>% 
      filter(Date >= as.Date("2016-02-01"), Date <= as.Date("2018-10-15")) %>% 
      ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")

![](README_files/figure-markdown_strict/Dlog%20returns-1.png)

2. Cumulative Returs
--------------------

Fitting the GARCH model requires cumulative returns, hence cumulative
returns for STANLIB Global Balanced Feeder Fund B are calculated from
the log returns.

    source("code/Cumret_func.R")
    Data <- Cumret_func(Data)

    Data %>% 
      group_by(FundName) %>% 
      filter(FundName == "STANLIB Global Balanced Feeder Fund B") %>% 
      filter(Date >= as.Date("2016-02-01"), Date <= as.Date("2018-10-15")) %>% 
      ggplot() + geom_line(aes(x = Date, y = Cumulative_Return, colour = FundName)) + theme(legend.position = "none")

![](README_files/figure-markdown_strict/unnamed-chunk-1-1.png)

3. Fitting ARIMA and GARCH Models
---------------------------------

The appropriate ARIMA and GARCH models are selected and fitted based on
obtaining the lowest Akaike Information Criterion (AIC). Furthermore,
the significance of the P-values for the appropriate coefficient
estimates are also assessed.The models are calibrated using the data
from 1 February 2016 to 15 October 2015.

    source("code/H1_GARCH_func.R")
    H1_fit <- H1_GARCH_func(Data)

4. Standardized Residuals
-------------------------

The residuals filtered to produce close to i.i.d Stationary standardized
residuals.

    source("code/H1_st_resids_func.R")
    H1_st_resids_func(Data)

![](README_files/figure-markdown_strict/unnamed-chunk-3-1.png)![](README_files/figure-markdown_strict/unnamed-chunk-3-2.png)

5. Simulating 1 Week Ahead
--------------------------

100 simulation paths of one week are simulated from 16 October 2018 to
22 October 2018.

    source("code/H1_sim_1W_func.R")
    H1_sim_1W <- H1_sim_1W_func(H1_fit)

    # Histogram of 1 week simulations
    source("code/H1_sim_1W_histogram.R")
    H1_sim_1W_histogram(H1_sim_1W)

![](README_files/figure-markdown_strict/unnamed-chunk-4-1.png)

6. Simulating 1 Month Ahead
---------------------------

100 simulation paths of one month are simulated from 16 October 2018 to
15 November 2018.

    source("code/H1_sim_1M_func.R")
    H1_sim_1M <- H1_sim_1M_func(H1_fit)

    # Histogram of 1 month simulations
    source("code/H1_sim_1M_histogram.R")
    H1_sim_1M_histogram(H1_sim_1M)

![](README_files/figure-markdown_strict/unnamed-chunk-5-1.png)

7. Calculating the VaR for both Simulated Periods
-------------------------------------------------

The VaR is calculated for both the one-week and one-month simulated
pathways.

    # VaR for simulations (1 Week from 2018-10-16 to 2018-10-22)
    source("code/H1_VaR_1W_sim_func.R")
    H1_VaR_1W_sim <- H1_VaR_1W_sim_func(H1_sim_1W)

    # VaR for simulations (1 Month from 2018-10-16 to 2018-11-15)
    source("code/H1_VaR_1M_sim_func.R")
    H1_VaR_1M_sim <- H1_VaR_1M_sim_func(H1_sim_1M)

8. Calculating the actual VaR from Historical Data
--------------------------------------------------

Using the historical values for the one-week and one-month time periods
(16 October 2018 to 22 October 2018. and 16 October 2018 to 15 November
2018.), the VaR is calculated to compare with the VaR of the simulated
pathways. Thus, the predictive strength of the FHS in determining VaR of
can be compared with actual data.

    # Historical VaR (1 Week 2018-10-16 to 2018-10-22)
    source("code/H1_VaR_1W_hist_func.R")
    H1_VaR_1W_hist <- H1_VaR_1W_hist_func(Data)

    # Historical VaR (1 Month 2018-10-16 to 2018-11-15 )
    source("code/H1_VaR_1M_hist_func.R")
    H1_VaR_1M_hist <- H1_VaR_1M_hist_func(Data)

9. Table to Compare VaR estimates
---------------------------------

A table is created to directly compare the VaR's of the simulated
periods and the VaR of the historical data for those periods.

NB! this table is produced in LateX and will not be shown in the README.

    source("code/H1_VaR_table.R")
    H1_VaR_table()

% latex table generated in R 3.4.4 by xtable 1.8-3 package % Thu Jan 31
23:14:52 2019
The following code chunk provides a matrix of the VaR table to see the
comparison of VaR's

    H1_VaR <- matrix(c(H1_VaR_1W_sim, H1_VaR_1M_sim, H1_VaR_1W_hist, H1_VaR_1M_hist),ncol=2,byrow=TRUE)
    #H1_VaR <- H1_VaR %>% as.data.frame()
    colnames(H1_VaR) <- c("One-Week", "One-Month")
    rownames(H1_VaR) <- c("FHS","Historical")

    print(H1_VaR)

    ##                One-Week   One-Month
    ## FHS        -0.017221537 -0.01723743
    ## Historical -0.008946389 -0.01246722