# Analysis of Economic Time Series (R)

# economic time series gathered with this program are continually
# updated... so predictive models, forecasts, and data visualizations
# produced by this program may differ from those shown in the book

## lines 10 to 14 have a function "libraries". This function connects the user to the location where the R function code packages are stored. 
## The "quantmod" function enables the extravction of economic data alongside its respective dates using the "lubridate" function. The "latticeExtra" and "forecast" function use this data to set up a time series forecast on a horison plot.

library(quantmod) # use for gathering and charting economic data
library(lubridate) # date functions
library(latticeExtra) # package used for horizon plot
library(forecast) # functions for time series forecasting 
library(lmtest) # for Granger test of causality

## The code on line 18 below creates a matrix of nrows x ncoloums plots. In this case it will create 4 charts presented in a 2 by 2 display with a chart in each cell.

par(mfrow = c(2,2)) # four plots on one window/page

# Economic Data from Federal Reserve Bank of St. Louis (FRED system)
# National Civilian Unemployment Rate (monthly, percentage)

## The getSymbols is a wrapper to load data from various sources, local or remote. Data is fetched via one of the available getSymbols methods.
## It helps pull in the National Civilian Unemployment Rates. The unemployment variable is "UNRATENSA". The gets arrow is used to rename variables (Lines 27 to 32).
## Line 28. The dimnames() command is used to set or query the row and column names of a matrix (It helps query National Civilian Unemployment Rates).
getSymbols("UNRATENSA", src="FRED", return.class = "xts")
ER <- 100 - UNRATENSA # convert to employment rate
dimnames(ER)[2] <- "ER"
chartSeries(ER,theme="white")
ER.data.frame <- as.data.frame(ER)
ER.data.frame$date <- ymd(rownames(ER.data.frame))
ER.time.series <- ts(ER.data.frame$ER, 
  start = c(year(min(ER.data.frame$date)),month(min(ER.data.frame$date))),
  end = c(year(max(ER.data.frame$date)),month(max(ER.data.frame$date))),
  frequency=12)

# Manufacturers' New Orders: Durable Goods (millions of dollars) 

## The getSymbols is a wrapper to load data from various sources, local or remote. Data is fetched via one of the available getSymbols methods.
## It helps pull in the data on durable goods. The durable goods variable is "DGORDER". The gets arrow is used to simplify and rename variables (Lines 43 to 49).
## Line 45. "chartSeries" is a charting tool to create standard financial charts given a time series like object. In our example it charts data on durable goods.
getSymbols("DGORDER", src="FRED", return.class = "xts")
DGO <- DGORDER/1000 # convert to billions of dollars
dimnames(DGO)[2] <- "DGO" # use simple name for index
chartSeries(DGO, theme="white") 
DGO.data.frame <- as.data.frame(DGO)
DGO.data.frame$DGO <- DGO.data.frame$DGO
DGO.data.frame$date <- ymd(rownames(DGO.data.frame))
DGO.time.series <- ts(DGO.data.frame$DGO, 
  start = c(year(min(DGO.data.frame$date)),month(min(DGO.data.frame$date))),
  end = c(year(max(DGO.data.frame$date)),month(max(DGO.data.frame$date))),
  frequency=12)

# University of Michigan Index of Consumer Sentiment (1Q 1966 = 100)

## The getSymbols is a wrapper to load data from various sources, local or remote. Data is fetched via one of the available getSymbols methods.
## It helps pull in the data on Consumer Sentiment. The Consumer Sentiment variable is "UMCSENT". The gets arrow is used to simplify and rename variables (Lines 60 to 66).
## Line 65. ymd() helps automatically assign the Universal Coordinated Time Zone (UTC) to the parsed dates in the data.
getSymbols("UMCSENT", src="FRED", return.class = "xts")
ICS <- UMCSENT # use simple name for xts object
dimnames(ICS)[2] <- "ICS" # use simple name for index
chartSeries(ICS, theme="white")
ICS.data.frame <- as.data.frame(ICS)
ICS.data.frame$ICS <- ICS.data.frame$ICS
ICS.data.frame$date <- ymd(rownames(ICS.data.frame))
ICS.time.series <- ts(ICS.data.frame$ICS, 
  start = c(year(min(ICS.data.frame$date)), month(min(ICS.data.frame$date))),
  end = c(year(max(ICS.data.frame$date)),month(max(ICS.data.frame$date))),
  frequency=12)

# New Homes Sold in the US, not seasonally adjusted (monthly, millions)

## Line 76. The getSymbols is a wrapper to load data from various sources, local or remote. Data is fetched via one of the available getSymbols methods.
## It helps pull in the data on New Homes Sold in the US. The New Homes sale variable is "HSN1FNSA". The gets arrow is used to simplify and rename variables (Lines 77 to 83). 
# Line 83. The ts() function is used to create time-series objects. 
getSymbols("HSN1FNSA",src="FRED",return.class = "xts")
NHS <- HSN1FNSA
dimnames(NHS)[2] <- "NHS" # use simple name for index
chartSeries(NHS, theme="white")
NHS.data.frame <- as.data.frame(NHS)
NHS.data.frame$NHS <- NHS.data.frame$NHS
NHS.data.frame$date <- ymd(rownames(NHS.data.frame))
NHS.time.series <- ts(NHS.data.frame$NHS, 
  start = c(year(min(NHS.data.frame$date)),month(min(NHS.data.frame$date))),
  end = c(year(max(NHS.data.frame$date)),month(max(NHS.data.frame$date))),
  frequency=12)

# define multiple time series object

## Line 93. Line cbind() At this step the gets arrow is used to combine and save the time series details for all four variables (ER, DGO, ICS, and NHS) as "economic.mts" 
## Line 94 "time.series" gives a time series illustration of a specified variable. The gets arrow is used to simplify and rename variables (Lines 95 to 96)

economic.mts <- cbind(ER.time.series, DGO.time.series, ICS.time.series,
  NHS.time.series) 
  dimnames(economic.mts)[[2]] <- c("ER","DGO","ICS","NHS") # keep simple names 
modeling.mts <- na.omit(economic.mts) # keep overlapping time intervals only

# plot multiple time series 

## Line 104 "plot" is the function that enables R to plot a graph.
##  Line 105 "dev.off" returns the number and name of the new active device (after the specified device has been shut down). It indicates a normal termination of a session.

pdf(file="fig_economic_analysis_mts_R.pdf",width = 8.5,height = 11)    
plot(modeling.mts,main="")
dev.off()

# create new indexed series IER using base date March 1997
## LIne 110. mean() calculates the arithmetic mean of the elements of the numeric vector passed to it as argument. 

ER0 <- mean(as.numeric(window(ER.time.series,start=c(1997,3),end=c(1997,3))))
IER.time.series <- (ER.time.series/ER0) * 100  

# create new indexed series IDGO using base date March 1997
DGO0 <- mean(as.numeric(window(DGO.time.series,start=c(1997,3),end=c(1997,3))))
IDGO.time.series <- (DGO.time.series/DGO0) * 100  

# create new indexed series INHS using base date March 1997
NHS0 <- mean(as.numeric(window(NHS.time.series,start=c(1997,3),end=c(1997,3))))
INHS.time.series <- (NHS.time.series/NHS0) * 100  

# create a multiple time series object from the index series
## Line 127. The dimnames() command queries the row and column names of a matrix at once. This will impat the time sewies of IER on Line 123.
economic.mts <- cbind(IER.time.series,
IDGO.time.series,
ICS.time.series,
INHS.time.series) 
dimnames(economic.mts)[[2]] <- c("IER","IDGO","ICS","INHS")
working.economic.mts <- na.omit(economic.mts) # months complete for all series

# partial listing to check calculation
## The print function allows R to print out an image of specified variables. 
print(head(working.economic.mts))

# plot multiple economic time series as horizon plot
# using the index 100 as the reference point (origin = 100)
# with scaling fixed across the index numbers (horizonscale = 25)
# use ylab rather than strip.left, for readability
# also shade any times with missing data values.
# latticeExtra package used for horizon plot
pdf(file="fig_economic_time_series_indexed_R.pdf",width = 8.5,height = 11)
print(horizonplot(working.economic.mts, colorkey = TRUE,
  layout = c(1,4), strip.left = FALSE, origin = 100, horizonscale = 25,
  ylab = list(rev(colnames(working.economic.mts)), rot = 0, cex = 0.7)) +
  layer_(panel.fill(col = "gray90"), panel.xblocks(..., col = "white")))
dev.off()
  
# return to the individual economic time series prior to indexing  
# functions from forecast package for time series forecasting 

# ARIMA model fit to the employment rate data
## The auto.arima() function in R uses a combination of unit root tests, minimization of the AIC and MLE to obtain an ARIMA model.
## Line 162 to 163 helps generate a 24 month forecast for the national employment rates variable while line 162 to 167 gives a 2 year forecast for national employment rates.
ER.auto.arima.fit <- auto.arima(ER.time.series, d=NA, D=NA, max.p=3, max.q=3,
  max.P=2, max.Q=2, max.order=3, start.p=2, start.q=2,
  start.P=1, start.Q=1, stationary=FALSE, seasonal=TRUE,
  ic=c("aic"), stepwise=TRUE, trace=FALSE,
  approximation=FALSE, xreg=NULL,
  test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
  allowdrift=FALSE, lambda=NULL, parallel=FALSE, num.cores=NULL)
print(summary(ER.auto.arima.fit))
# national employment rate two-year forecast (horizon h = 24 months) 
ER.forecast <- forecast.Arima(ER.auto.arima.fit, h=24, level=c(90), 
  fan=FALSE, xreg=NULL, bootstrap=FALSE)
# plot national employment rate time series with two-year forecast 
pdf(file = "fig_economic_analysis_er_forecast_R.pdf", width = 11, height = 8.5)
plot(ER.forecast,main="", ylab="Employment Rate (100 - Unemployment Rate)",
  xlab = "Time", las = 1, lwd = 1.5)
dev.off()

# ARIMA model fit to the manufacturers' durable goods orders
## The auto.arima() function in R uses a combination of unit root tests, minimization of the AIC and MLE to obtain an ARIMA model.
## Line 182 to 183 helps generate a 24 month forecast for the durable goods orders variable while line 182 to 187 gives a 2 year forecast for national employment rates.
DGO.auto.arima.fit <- auto.arima(DGO.time.series, d=NA, D=NA, max.p=3, max.q=3,
  max.P=2, max.Q=2, max.order=3, start.p=2, start.q=2,
  start.P=1, start.Q=1, stationary=FALSE, seasonal=TRUE,
  ic=c("aic"), stepwise=TRUE, trace=FALSE,
  approximation=FALSE, xreg=NULL,
  test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
  allowdrift=FALSE, lambda=NULL, parallel=FALSE, num.cores=NULL)
print(summary(DGO.auto.arima.fit))
# durable goods orders two-year forecast (horizon h = 24 months) 
DGO.forecast <- forecast.Arima(DGO.auto.arima.fit, h=24, level=c(90), 
  fan=FALSE, xreg=NULL, bootstrap=FALSE)
# plot durable goods time series with two-year forecast 
pdf(file = "fig_economic_analysis_dgo_forecast_R.pdf", width = 11, height = 8.5)
plot(DGO.forecast,main="", ylab="Durable Goods Orders (billions of dollars)",
  xlab = "Time", las = 1, lwd = 1.5)
dev.off()  

# ARIMA model fit to index of consumer sentiment
## Line 201 to 202 helps generate a 24 month forecast for the consumer sentiment variable while line 204 to 206 gives a 2 year forecast for national employment rates.
ICS.auto.arima.fit <- auto.arima(ICS.time.series, d=NA, D=NA, max.p=3, max.q=3,
  max.P=2, max.Q=2, max.order=3, start.p=2, start.q=2,
  start.P=1, start.Q=1, stationary=FALSE, seasonal=TRUE,
  ic=c("aic"), stepwise=TRUE, trace=FALSE,
  approximation=FALSE, xreg=NULL,
  test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
  allowdrift=FALSE, lambda=NULL, parallel=FALSE, num.cores=NULL)
print(summary(ICS.auto.arima.fit))
# index of consumer sentiment two-year forecast (horizon h = 24 months) 
ICS.forecast <- forecast.Arima(ICS.auto.arima.fit, h=24, level=c(90), 
  fan=FALSE, xreg=NULL, bootstrap=FALSE)
# plot index of consumer sentiment time series with two-year forecast 
pdf(file = "fig_economic_analysis_ics_forecast_R.pdf", width = 11, height = 8.5)
plot(ICS.forecast,main="", ylab="Index of Consumer Sentiment (1Q 1966 = 100)",
  xlab = "Time", las = 1, lwd = 1.5)
dev.off()

# ARIMA model fit to new home sales
## Line 220 to 221 helps generate a 24 month forecast for the new home sales variable while line 223 to 225 gives a 2 year forecast for national employment rates.
NHS.auto.arima.fit <- auto.arima(NHS.time.series, d=NA, D=NA, max.p=3, max.q=3,
  max.P=2, max.Q=2, max.order=3, start.p=2, start.q=2,
  start.P=1, start.Q=1, stationary=FALSE, seasonal=TRUE,
  ic=c("aic"), stepwise=TRUE, trace=FALSE,
  approximation=FALSE, xreg=NULL,
  test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
  allowdrift=FALSE, lambda=NULL, parallel=FALSE, num.cores=NULL)
print(summary(NHS.auto.arima.fit))
# new home sales two-year forecast (horizon h = 24 months) 
NHS.forecast <- forecast.Arima(NHS.auto.arima.fit, h=24, level=c(90), 
  fan=FALSE, xreg=NULL, bootstrap=FALSE)
# plot new home sales time series with two-year forecast 
pdf(file = "fig_economic_analysis_nhs_forecast_R.pdf", width = 11, height = 8.5)
plot(NHS.forecast,main="", ylab="New Homes Sold (millions)",
  xlab = "Time", las = 1, lwd = 1.5)
dev.off()

# Which regressors have potential as leading indicators?
# look for relationships across three of the time series
# using the period of overlap for those series
# function from lmtest package for Granger test of causality
## Line 233 to 238. The grangertest()function is a generic function for performing a test for Granger causality.
grangertest(ICS~ER, order = 3, data=modeling.mts)
grangertest(ICS~DGO, order = 3, data=modeling.mts)
grangertest(DGO~ER, order = 3, data=modeling.mts)
grangertest(DGO~ICS, order = 3, data=modeling.mts)
grangertest(ER~DGO, order = 3, data=modeling.mts)
grangertest(ER~ICS, order = 3, data=modeling.mts)

# export data frames for economic measures 
## Line 242 to 245. The function "write.csv()" ensures that the dat is saved as a csv file.
write.csv(ER.data.frame, file = "FRED_ER_data.csv", row.names = FALSE)
write.csv(DGO.data.frame, file = "FRED_DGO_data.csv", row.names = FALSE)
write.csv(ICS.data.frame, file = "FRED_ICS_data.csv", row.names = FALSE)
write.csv(NHS.data.frame, file = "FRED_NHS_data.csv", row.names = FALSE)

# save current workspace
save.image(file = "R_workspace.Rdata")

# Suggestions for the student:
# Explore additional forecasting methods such as exponential smoothing.
# Explore dynamic linear models and state space approaches.
# Gather data on additional economic measures that might be regarded
# as leading indicators. Select an industry to study, examine relevant 
# economic indicators and possible relationships to financial performance 
# of companies within that industry (stock prices or returns).

