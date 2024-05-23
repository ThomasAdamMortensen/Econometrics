library(dynlm)
library(urca)
library(vars)
library(sandwich)

rm(list = ls())
data(ChickEgg, package = "lmtest")

df = ChickEgg
df_diff = diff(df)

plot(df)
plot(df_diff) # Seems to be visually I(1) but needs testing!

summary(ur.df(df[,1], type = "none", selectlags = "AIC"))
summary(ur.df(df[,2], type = "none", selectlags = "AIC"))

summary(ur.df(df_diff[,1], type = "none", selectlags = "AIC")) 
summary(ur.df(df_diff[,2], type = "none", selectlags = "AIC"))
# Seems to be I(1) as we reject unit root when differenced once but not at level.


coint = dynlm(chicken ~ egg, data = df)

summary(ur.df(coint$residuals, type = "drift", selectlags = "AIC"))
# The test statistic reported by ur.df() is correct but the critical values are
# not. The test stastic should be compared to the table in S&W p.665 which I've
# reproduced below.

# Significance:  10%      5%      1%
# 1 regressors: -3.12 # -3.41 # -3.96
# 2 regressors: -3.52 # -3.80 # -4.36
# 3 regressors: -3.84 # -4.16 # -4.73
# 4 regressors: -4.20 # -4.49 # -5.07

# As we can't reject null of unit root we seem to not have cointegration which 
# means we can use VAR instead of VECM.

VARselect(df_diff, type = "const") # allow for a constant

# Most common choice is AIC which suggests p = 1.
# The VAR() function estimates our reduced form VAR.

VAR <- VAR(df_diff, p = 1, type = "const")
summary(VAR)

serial.test(VAR, lags.pt = 2) # cannot reject null of 0 autocorrelation
serial.test(VAR, lags.pt = 3) # cannot reject null of 0 autocorrelation

normality.test(VAR)$jb.mul$JB # Cannot reject null of normally distributed residuals.

Granger_chicken_egg <- causality(VAR, cause = "chicken", vcov. = sandwich::vcovHC(VAR))

Granger_egg_chicken <- causality(VAR, cause = "egg", vcov. = sandwich::vcovHC(VAR))

Granger_chicken_egg$Granger # null not reject, no evidence of chicken granger causing egg
Granger_egg_chicken$Granger # Rejected, egg granger causes chicken

# We can conclude the egg came before the chicken!
