library(dynlm)
library(stats)
library(tseries)


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
AIC_scores[which.min(AIC_scores)]

# Jarques bera and Ljung box tests for models
jb = list()
for(i in 1:length(list_AR)){
  jb[[i]] = jarque.bera.test(list_AR[[i]]$residuals)
}

lj = list()

for(i in 1:length(list_AR)){
  q <- floor(0.75*nobs(list_AR[[i]])^(1/3)) 
  df = q - length(list_AR[[i]]$coefficients) # NOTE: Some degrees of freedom 
  # will be negative, if a df > lag it produces Nans, we set df = q in that case
  if(df < 0){
    df = q
  }
  lj[[i]] = Box.test(list_AR[[i]]$residuals, type = "Ljung-Box", lag = q, fitdf = df)
}
lj # Independence tests for residuals seem highly stat sign on all reasonable 
# levels for all but the 1st model which seems non-significant eg. not statistically diff from zero
jb 
# Reversibly here only the 1st model is significant for the test of normality
# This seems that all of our models violate at least one of our assumptions
# about our residuals which makes our forecasts with those models less reliable
# We might handle this using transformations, model specification or be okay 
# with it given a large sample size. A large sample size might make the coef 
# estimate function approximate normality.

# If we had to pick one, model 4 might be best with lowest AIC and only violating normality, not independence

# Part 2 
library(readxl)
library(forecast)
library(greybox)
setwd("~/R_projects/Econometrics 2B Applied")
kpif = read_xlsx("2022_12_KPIF.xlsx" )
kpif = kpif[-1,] # Careful to only run this once as we had a blank row
names(kpif) = c("index", "kpif")

kpif

kpif_ts = ts(kpif, start = c(1987, 1), end = c(2022, 12), frequency = 12)[,-1]



arima = auto.arima(kpif_ts)
arima

arima_call 
arima_val

auto_call
auto_val

# https://cran.r-project.org/web/packages/greybox/vignettes/ro.html 
# Use this for this task


