# Okuns law? or philips law? https://www.investopedia.com/articles/economics/12/okuns-law.asp
# Okun = GDP and unemployment inverse relationship
# philips law unemployment and GDP inverse relationship

# VARS are made to estimate relationship between multiple quantities over time,
# meaning we can see how several quantities can be modeled using lags of itself and others.


# One example of such case https://www.youtube.com/watch?v=UYRFYWNxX4c&ab_channel=JustinEloriaga

# Download data with 2 variables, GDP and unemployment or inflation from Statistics sweden.

# Then estimate VAR(P) of P lags and interpret coefs

# Do tests to check if assumptions are met, what P to choose and more.
# Such as KPSS and ADF and plotting/differencing to find I(D) for this data
# Check for cointegration.

# After models estimated, check residuals for normality/autocorrelation for assumptions and interpret

# Either keep it in script format or Rmarkdown/quarto or stay to script.

# Use latex to bind it all together in overleaf preferably.

# Konjunkturinstitutet, BNP GAP: https://prognos.konj.se/PxWeb/pxweb/sv/SenastePrognosen/SenastePrognosen__f24_resursutnyttjande/F2404.px/table/tableViewLayout2/?rxid=76600014-4460-47f5-8c63-680654f2935d

setwd("~/R_projects/Econometrics 2B Applied/Term paper")

rm(list = ls())

df = read.csv("BNP_gap_1981Q1-2020Q4.csv", skip = 1, sep = ";", col.names = c("value","index","BNP_gap"))[,3]
df$unemploy_perc = read.csv("16-64_Unemployed1981Q1-2020Q4.csv", skip = 1, sep = ",", header = FALSE)[,4]

plot(df[,-1]) # seems to have an inverse relationship as expected!
cor(df[,-1]) # Negative correlation as expected



