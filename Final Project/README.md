Information
-----------

This README demonstrates the coding procedure behind the assignment.
This file shows the Filtered Historical Simulation (FHS) analysis of
Value at Risk (VaR) for Analytics Ci Moderate Fund of Funds only, since
the procedure is repeated identically for each fund. Only the specified
fits of ARIMA and GARCH models vary slightly with each fund.

Lastly, the code functions (sourced .Rmd files) also contain details in
the form of comments to explain the relevant code chunks and their
purpose.

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

Log return for Analytics Ci Moderate Fund of Funds are first calculated
from prices.

    source("code/DLog_func.R")
    Data <- DLog_func(Data)

    Data %>% 
      group_by(FundName) %>% 
      filter(FundName == "Analytics Ci Moderate Fund of Funds") %>% 
      filter(Date >= as.Date("2016-02-01"), Date <= as.Date("2018-09-14")) %>% 
      ggplot() + geom_line(aes(x = Date, y = Return, colour = FundName)) + theme(legend.position = "none")

![](README_files/figure-markdown_strict/Dlog%20returns-1.png)

2. Cumulative Returns
---------------------

Fitting the GARCH model requires cumulative returns, hence cumulative
returns for Analytics Ci Moderate Fund of Funds are calculated from the
log returns.

    source("code/Cumret_func.R")
    Data <- Cumret_func(Data)

    Data %>% 
      group_by(FundName) %>% 
      filter(FundName == "Analytics Ci Moderate Fund of Funds") %>% 
      filter(Date >= as.Date("2016-02-01"), Date <= as.Date("2018-09-13")) %>% 
      ggplot() + geom_line(aes(x = Date, y = Cumulative_Return, colour = FundName)) + theme(legend.position = "none")

![](README_files/figure-markdown_strict/unnamed-chunk-1-1.png)

3. Fitting ARIMA and GARCH Models
---------------------------------

The appropriate ARIMA and GARCH models are selected and fitted based on
obtaining the lowest Akaike Information Criterion (AIC). Furthermore,
the significance of the P-values for the appropriate coefficient
estimates are also assessed.The models are calibrated using the data
from 1 February 2016 to 13 September 2015.

    source("code/H2_GARCH_func.R")
    H2_fit <- H2_GARCH_func(Data)

4. Standardized Residuals
-------------------------

The residuals are filtered to produce close to i.i.d Stationary
standardized residuals.

    source("code/H2_st_resids_func.R")
    H2_st_resids_func(Data)

![](README_files/figure-markdown_strict/unnamed-chunk-3-1.png)![](README_files/figure-markdown_strict/unnamed-chunk-3-2.png)

5. Simulating 1 Week Ahead
--------------------------

100 simulation pathways of one week are simulated from 14 September 2018
to 20 September 2018.

    source("code/H2_sim_1W_func.R")
    H2_sim_1W <- H2_sim_1W_func(H2_fit)

    # Histogram of 1 week simulations
    source("code/H2_sim_1W_histogram.R")
    H2_sim_1W_histogram(H2_sim_1W)

![](README_files/figure-markdown_strict/unnamed-chunk-4-1.png)

6. Simulating 1 Month Ahead
---------------------------

100 simulation paths of one month are simulated from 14 September 2018
to 15 October 2018.

    source("code/H2_sim_1M_func.R")
    H2_sim_1M <- H2_sim_1M_func(H2_fit)

    # Histogram of 1 month simulations
    source("code/H2_sim_1M_histogram.R")
    H2_sim_1M_histogram(H2_sim_1M)

![](README_files/figure-markdown_strict/unnamed-chunk-5-1.png)

7. Simulating 2 Months Ahead
----------------------------

100 simulation paths of two months are simulated from 14 September 2018
to 15 November 2018.

    source("code/H2_sim_2M_func.R")
    H2_sim_2M <- H2_sim_2M_func(H2_fit)

    # Histogram of 1 month simulations
    source("code/H2_sim_2M_histogram.R")
    H2_sim_2M_histogram(H2_sim_2M)

![](README_files/figure-markdown_strict/unnamed-chunk-6-1.png)

8. Calculating the VaR for the three Simulated time Periods
-----------------------------------------------------------

The VaR is calculated for the simulated pathways of the three time
periods.

    # VaR for simulations (1 Week from 2018-09-14 to 2018-09-20)
    source("code/VaR_1W_sim_func.R")
    H2_VaR_1W_sim <- VaR_1W_sim_func(H2_sim_1W)

    # VaR for simulations (1 Month from 2018-09-14 to 2018-10-15)
    source("code/VaR_1M_sim_func.R")
    H2_VaR_1M_sim <- VaR_1M_sim_func(H2_sim_1M)

    # VaR for simulations (2 Months from 2018-09-14 to 2018-11-15)
    source("code/VaR_2M_sim_func.R")
    H2_VaR_2M_sim <- VaR_2M_sim_func(H2_sim_2M)

9. Calculating the actual VaR from Historical Data
--------------------------------------------------

Using the historical values for the one-week, one-month and two-month
time periods (14 September 2018 to 20 September 2018, 14 September 2018
to 15 October 2018 and 14 September 2018 to 15 November 2018.), the VaR
is calculated to compare with the VaR of the simulated pathways. Thus,
the predictive strength of the FHS in determining VaR of can be compared
with actual data.

    # Historical VaR (1 Week from 2018-09-14 to 2018-09-20)
    source("code/H2_VaR_1W_hist_func.R")
    H2_VaR_1W_hist <- H2_VaR_1W_hist_func(Data)

    # Historical VaR (1 Month from 2018-09-14 to 2018-10-15)
    source("code/H2_VaR_1M_hist_func.R")
    H2_VaR_1M_hist <- H2_VaR_1M_hist_func(Data)

    # Historical VaR (2 Months from 2018-09-14 to 2018-11-15)
    source("code/H2_VaR_2M_hist_func.R")
    H2_VaR_2M_hist <- H2_VaR_2M_hist_func(Data)

10. Table to Compare VaR estimates of FHS and actual historical Data
--------------------------------------------------------------------

A table is created to directly compare the VaR's of the simulated
periods (first row) and the VaR of the historical data (second row) for
those periods. Furthermore, the third row shows the difference in
absolute values between the VaR calculated from simmulations and the VaR
calculated from the historical data.

NB! this table is produced in LateX for the PDR write-up and will not be
shown nicely in the README.

    source("code/H2_VaR_table.R")
    H2_VaR_table()

    ## % latex table generated in R 3.4.4 by xtable 1.8-3 package
    ## % Wed Feb 06 22:28:41 2019
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rrrr}
    ##   \hline
    ##  & One-Week & One-Month & Two-Months \\ 
    ##   \hline
    ## FHS & -0.00712 & -0.00704 & -0.00662 \\ 
    ##   Historical & -0.00367 & -0.00878 & -0.00796 \\ 
    ##   Difference & 0.00345 & 0.00174 & 0.00134 \\ 
    ##    \hline
    ## \end{tabular}
    ## \end{table}
