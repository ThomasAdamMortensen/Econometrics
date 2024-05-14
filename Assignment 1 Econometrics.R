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
# Ljbox tests shows only the 1st three models have significantly independent errors
# JB tests show that only the 1st model has significantly gaussian errors
# This can be ok though as with a large enough sample size, the estimation functions for our coefficients, approach a normal dist.
# The AR(4) model had the lowest AIC but violates independent errors according to our test,
# We might choose to use either our AR(2) model if we deem violating gaussian errors is worth the gain in forecast accuracy.
# Lastly we might choose AR(1) if we believe gaussian errors is more important than some gain in accuracy.

###########################################################################

# Part 2 

setwd("~/R_projects/Econometrics 2B Applied") # Set WD to where the excel file is located
kpif = read_xlsx("2022_12_KPIF.xlsx" )
kpif = kpif[-1,] # Careful to only run this once as we had a blank row
names(kpif) = c("index", "kpif")

kpif

kpif_ts = ts(kpif, start = c(1987, 1), end = c(2022, 12), frequency = 12)[,-1]



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

# We now do loop from 1 to 36 and do the following at each step:
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

# All auto.arima models are non significant on any reasonable level, while
# most AR() models are. This means that all auto arima models residuals aren't 
# biased according to this test as they have mean 0, the AR() models that are significant
# are biased though according to this test. This means using those AR() models you 
# will on average over or underestimate during forecasting.


# The null no bias (i.e. expected forecast error = 0) cannot be rejected at any
# conventional significance level except the AR(1) forecast for horizon h = 1.

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
# h = 1:4 are the ones the test show stat significantly difference in average forecast errors between models
# The rest of the models forecast errors the test tells us is not different on average.
# One could interpret this as the AR and ARIMA models forecasting only the first few 
# timepoints(h <= 4) to have different average forecast errors, while in the h > 4 case they are on average the same.
# According to the tasks stated horizons of 1,12 and 24 we would say there is a 
# statistical diff in forecast errors only for horizon h = 1.
# One could interpret this as for h = 1 we would expect a difference in model accuracy, 
# but not for h = 12 and h = 24.