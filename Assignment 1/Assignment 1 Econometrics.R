library(dynlm)
library(stats)
library(tseries)
library(portes)
require(lmtest)
require(sandwich)
library(readxl)
library(forecast)

data("IncomeUK", package = "Ecdat")
df_inc = IncomeUK[,1]

df_diff = diff(df_inc)

# Function to create a list of i models using to using lag i.
# Meaning we will have model 1 using lag 1, model 2 using lag 2 etc.

AR_models = function(df, lags, start, freq){
  lm_list = list()
  df_embed = embed(df, (lags+1))
  df_embed = ts(df_embed, start = start, frequency = freq)
  for(i in 1:lags){
    
    lm_list[[i]] = dynlm(df_embed[,1] ~ df_embed[,2:(i+1)])
  }
  names(lm_list) =  paste0("AR", 1:lags)
  return(lm_list)
}


list_AR = AR_models(df_diff, 5, start = c(1970, 1), freq = 4)
list_AR



# Calculate AIC Scores and pick minimum for best fit
AIC_scores = lapply(list_AR, AIC)
AIC_scores
AIC_scores[which.min(AIC_scores)]

# Jarques bera and Ljung box tests for models
jb = list()
for(i in 1:length(list_AR)){
  jb[[paste0("AR", i)]] = jarque.bera.test(list_AR[[i]]$residuals)
}

lj = list()

for(i in 1:length(list_AR)){
  q <- floor(0.75*nobs(list_AR[[i]])^(1/3))
  df = length(list_AR[[i]]$coefficients)
  if(q <= df){
    q = q + df
  }
  lj[[paste0("AR", i)]] = LjungBox(list_AR[[i]]$residuals, lag = q, fitdf = df)
}

lj

jb
# As the AR(1:3) models residuals reject the null of no auto correlation,
# we can only consider AR(4) and AR(5), as IID is vital.
# All models residuals are considered normal by the test on any reasonable significance level, except for AR(1).
# This leaves us with AR 4:5 that fulfill our assumptions according to our tests.
# We choose then by lowest AIC, which results in choosing AR(4).

###########################################################################

# Part 2

setwd("~/R_projects/Econometrics 2B Applied") # Set WD to where the excel file is located
kpif = read_xlsx("2022_12_KPIF_rates.xlsx" )
kpif = na.exclude(kpif)
names(kpif) = c("index", "kpif")

kpif

kpif_ts = ts(kpif, start = c(1988, 1), end = c(2022, 12), frequency = 12)[,-1]



####################################################################################



hmax <- 24 # the maximum forecast horizon we're gonna use
origin_1 <- 2002 + (11/12) # first forecast origin
n_origins <- length(window(kpif_ts, start = origin_1)) - hmax # total number of forecast origins

# We start by creating two lists of length 228 in which we will store our models
# and forecasts.

ARIMA_models <- vector("list", n_origins)
ARIMA_forecasts <- vector("list", n_origins)

# We then create 24 time series starting at the first forecast origin.
# We will store our forecasts for horizons h = 1:24 in these series.

ARIMA_results <- ts(matrix(NA, nrow = n_origins, ncol = hmax),
                    start = origin_1,
                    frequency = frequency(kpif_ts))

# We can use the paste0() function to change the column names

colnames(ARIMA_results) <- paste0("h = ", 1:hmax)

# We now do loop from 1 to 217 and do the following at each step:
# 1) Define the origin which starts at 2002 and adds 1/12 at each step
# 2) Subset the data with the window() function
# 3) Use the auto.arima function on the subset of data in 2)
# 4) Use the forecast() function with the ARIMA model from 3) to forecast horizons = 1,2,3,4
# 6) Save results
# 5) Save model
# 6) Save forecast

for (i in 1:n_origins) {
  origin <- origin_1 + (i - 1)/12
  data <- window(kpif_ts, end = origin)
  m <- auto.arima(data)
  f <- forecast(m, h = hmax)
  ARIMA_results[i,] <- f$mean
  ARIMA_models[[i]] <- m
  ARIMA_forecasts[[i]] <- f
}
rm(i,origin,data,m,f) # the loop will leave a bunch of objects in the global environment
# that we don't want to keep


AR1_models <- vector("list", n_origins)
AR1_forecasts <- vector("list", n_origins)

AR1_results <- ts(matrix(NA, nrow = n_origins, ncol = hmax),
                  start = origin_1,
                  frequency = frequency(kpif_ts))
colnames(AR1_results) <- paste0("h = ", 1:hmax)

for (i in 1:n_origins) {
  origin <- origin_1 + (i - 1)/12
  data <- window(kpif_ts, end = origin)
  m <- arima(data, order = c(1,0,0), method = "ML")
  f <- forecast(m, h = hmax)
  AR1_results[i,] <- f$mean
  AR1_models[[i]] <- m
  AR1_forecasts[[i]] <- f
}
rm(i,origin,data,m,f)

####################################################################################

# Test for bias

# First we create four time series starting at the first forecast origin which
# contains the actual inflation rate for the forecasted horizons.

outcome <- ts(data = matrix(NA, nrow = n_origins, ncol = hmax),
              start = origin_1,
              frequency = frequency(kpif_ts))

for (i in 1:n_origins) {
  start_date <- origin_1 + i / 12
  end_date <- start_date + 23/12
  outcome[i,] <- window(kpif_ts, start = start_date, end = end_date)
}

rm(i, start_date, end_date)

ARIMA_errors <- outcome - ARIMA_results
AR1_errors <- outcome - AR1_results
colnames(ARIMA_errors) <- paste0("h = ", 1:hmax)
colnames(AR1_errors) <- paste0("h = ", 1:hmax)
rm(outcome) # no need for this anymore as we're only interested in the errors


# Bias test

# To test whether our forecast of a given horizon h is biased we can simply
# regress the forecast error on a constant and check whether the estimated
# coefficient (= mean forecast error) is significant.

bias_test <- function(h,series) {
  model <- dynlm(series[, h] ~ 1)
  test <- coeftest(model, vcov. = NeweyWest(model, lag = h - 1))
  pval <- test[1, 4]
  return(pval)
}


# Note that we used Newey-West standard errors and we need to specify that lag = h - 1.

# Let's create an empty data frame to store our p-values.

bias_pvals <- data.frame(h = 1:hmax, ARIMA = NA, AR1 = NA)

# Then use a loop to fill it with our p-values.

for (h in 1:hmax) {
  bias_pvals$ARIMA[h] <- bias_test(h,ARIMA_errors)
  bias_pvals$AR1[h] <- bias_test(h,AR1_errors)
}
rm(h)

# Results!

bias_pvals

# All models are not rejecting our null hypothesis for any value of h, if one had 
# a high significance level of 10% one might conclude that AR(1) rejects the null.
# Rejecting the null here means that the mean of errors or expected forecast errors
# are not zero according to the tests. 
# This would mean those models consistently over or underestimate the true values.

####################################################################################

# Diebold-Mariano test

# The null hypothesis of the Diebold-Mariano test is that the expected difference
# in average forecast error between two forecasts is equal to zero.

# To perform a Diebold-Mariano test we use the dm.test() function from the forecast
# package.

# First we create an empty dataframe to store the p-values from the Diebold-Mariano
# test for the our four different forecast horizons.

DM_pvals <- data.frame(h = 1:hmax,
                       pval = NA)

# Then we fill it with p-values from the test.

for (h in 1:hmax) {
  test <- dm.test(ARIMA_errors[,h], AR1_errors[, h], h = h)
  DM_pvals$pval[h] <- test$p.value
}
rm(h,test)

# Results!

DM_pvals
DM_pvals[DM_pvals$pval <= 5e-02,] # Using common significance level 0.05
# No value of h with statistically significant difference in average forecast errors between models.
# We interpret this as our models forecasts being off on average the same amount.
# If one had a higher significance level one could reject the null for h = 1 only.
# This would mean we have a statistically significant difference between models forecast errors for h = 1
