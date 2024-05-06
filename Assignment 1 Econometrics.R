library(Ecdat)
library(dynlm)
library(stats)
library(tseries)
data("IncomeUK")
df_inc = IncomeUK[,1]

df_diff = diff(df_inc)

# Function to create a list of i models using to using lag i. 
# Meaning we will have model 1 using lag 1, model 2 using lag 2 etc.

AR_models = function(df, lags){
  lm_list = list()
  
  for(i in 1:lags){
    formula = as.formula(df ~ lag(df, -i))
    lm_list[[i]] = dynlm(formula)
  }
  names(lm_list) =  paste0("lag", 1:lags)
  return(lm_list)
}

# This version includes all lags so model 1 has lag 1 
# and model 2 has lag 1 and 2 etc.

# AR_models = function(df, lags){
#   lm_list = list()
#   
#   for(i in 1:lags){
#     formula = as.formula(df ~ lag(df, c(1:i)))
#     lm_list[[i]] = dynlm(formula)
#   }
#   names(lm_list) =  paste0("lag", 1:lags)
#   return(lm_list)
# }

list_AR = AR_models(df_diff, 5)
list_AR


# Just not sure what lag to use when specifying formula? if we use lag() from 
# base R should K be positive or negative? Shifting the time series back or forward?
# Using L(df, lag), in the formula of dynlm also works, problem here is that
# only shifting the time series backwards makes sense, as predicting the next value
# with the Kth lag of that value. Sadly shifting it backward either with lag
# or in formula L() makes us dynlm use less observations. 
# This makes no sense though as fitting df ~ lag(df, 1) makes us predict value
# in time t with value in time t+1.
# we could reverse this by fitting lag(df, 1) ~ df. That just feels wrong.

# dynlm(d(df) ~ L(d(df), lag)) This syntax also works and even 
# differences it for us, sadly it too results in series of differing length

# Calculate AIC Scores and pick minimum for best fit
AIC_scores = lapply(list_AR, AIC)
which.min(AIC_scores)

# Jarques bera and Ljung box tests for models
jb = list()
for(i in 1:length(list_AR)){
  jb[[i]] = jarque.bera.test(list_AR[[i]]$residuals)
}

lj = list()
for(i in 1:length(list_AR)){
  lj[[i]] = Box.test(list_AR[[i]]$residuals, type = "Ljung-Box")
}
lj
jb

# Here will be an analysis of the tests and AIC scores to determine 
# a suitable model that works best here.

# Part 2 
library(readxl)
library(forecast)
kpif = read_xlsx("2022_12_KPIF.xlsx" )
kpif = kpif[-1,] # Careful to only run this once as we had a blank row
kpif



