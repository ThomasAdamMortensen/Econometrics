library(tseries)
library(dynlm)
library(tsDyn)
library(vars)

setwd("~/R_projects/Econometrics 2B Applied/Term paper") # Set wd to where files are

rm(list = ls())

df = read.csv("BNP_gap_1981Q1-2020Q4.csv", skip = 1, sep = ";", col.names = c("value","index","BNP_gap"))
df$unemploy_perc = read.csv("16-64_Unemployed1981Q1-2020Q4.csv", skip = 1, sep = ",", header = FALSE)[,4]
df = df[,2:4]

par(mfrow = c(2,3))
plot(df[,-1], main = "Scatterplot") # seems to have an inverse relationship as expected!
plot(ts(df[2], start = c(1981, 1), frequency = 4), main = "GDP gap over time")
plot(ts(df[3], start = c(1981, 1), frequency = 4), main = "Unemployment rate over time")
plot(diff(ts(df[2], start = c(1981, 1), frequency = 4)), main = "Differenced BNP_gap")
plot(diff(ts(df[3], start = c(1981, 1), frequency = 4)), main = "Differenced unemployment rate")
adf.test(ts(df[,"BNP_gap"])) # Can't reject for BNP needs to be diffed
adf.test(ts(df[,"unemploy_perc"]))# Can't reject for Unemploy_perc needs to be diffed

adf.test(diff(ts(df[,"BNP_gap"]))) # Can reject, significant evidence BNP is I(1)
adf.test(diff(ts(df[,"unemploy_perc"]))) # Can reject, significant evidence Unemployment is I(1)

kpss.test(ts(df[,"BNP_gap"])) # Can't reject for BNP gap, no need to diff
kpss.test(ts(df[,"unemploy_perc"]))# Can reject for Unemploy_perc, need to diff

kpss.test(diff(ts(df[,"unemploy_perc"]))) # Can't reject, no evidence Unemployment is I(0)
kpss.test(diff(ts(df[,"unemploy_perc"]))) # Can't reject, no evidence Unemployment is I(0)

# As ADF and kpss can agree that both series differenced make them stationary, we will use them diffed once.

ts_df = ts(df[,2:3], start = c(1981, 1), frequency = 4)

coint = dynlm(BNP_gap ~ unemploy_perc, data = ts_df)
adf.test(coint$residuals) # evidence of cointegration which means we just fit a VAR in levels.
VARselect(ts_df) # p = 9 according to AIC and other measures.

okun_var = VAR(ts_df, p = 9)
summary(okun_var)

serial.test(okun_var, lags.pt = 18) # can reject null at 5%
serial.test(okun_var, lags.pt = 27) # cannot reject null
serial.test(okun_var, lags.pt = 36) # cannot reject null


# We now test if the residuals are normally distributed by performing a multivariate 
# Jarque-Bera test using normality.test()

normality.test(okun_var)$jb.mul$JB # We reject the null of gaussian residuals with an extremely large test statistic.
# Though this is fine as it is not a major assumption that makes the analysis useless or dubious if not fulfilled.


Granger_BNP_U <- causality(okun_var, cause = "BNP_gap", vcov. = sandwich::vcovHC(okun_var))
Granger_U_BNP <- causality(okun_var, cause = "unemploy_perc", vcov. = sandwich::vcovHC(okun_var))

Granger_BNP_U$Granger # Evidence BNP gap granger causes unemployment rate
Granger_U_BNP$Granger # No evidence of Unemployment rate granger causing BNP gap

IRF_BNP_U <- irf(okun_var, impulse = "BNP_gap", response = "unemploy_perc", ortho = TRUE, boot = FALSE)
IRF_U_BNP <- irf(okun_var, impulse = "unemploy_perc", response = "BNP_gap", ortho = TRUE, boot = FALSE)
plot(IRF_BNP_U, main = "A shock in GDP gap") # A shock in BNP_gap leads to an instant decrease in Unemployment rate that keeps decreasing 
# until 5 time periods after and then the effect of the shock weakens as time passes, with a small spike down again between 7:9.
plot(IRF_U_BNP, main = "A shock in Unemployment Rate") # A shock in unemployment rate leads to no instant decrease in BNP gap and starts decreasing 
# more every period until 4, then weakens as time passes, quite slowly though.

FEVD = fevd(okun_var)

round(FEVD$BNP_gap * 100, 1)
round(FEVD$unemploy_perc * 100, 1)
plot(FEVD) # For BNP gap it is mostly influenced by shocks in itself for all time periods, while unemployment rate 
# is mostly influenced by shocks in itself 1 time period back,while any longer than that it is 
# approximately equally affected by both BNP gap shocks and shocks in itself.

