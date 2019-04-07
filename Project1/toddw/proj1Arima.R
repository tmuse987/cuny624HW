source("proj1_loadAndCleanup.r")

h <- 140


arimaFitVar02 <- auto.arima(TsVar02, seasonal = TRUE)
autoplot(forecast(arimaFitVar02), h= h)

arimaFitVar03 <- auto.arima(TsVar03, seasonal = TRUE)
autoplot(forecast(arimaFitVar03), h = h)

dfTss05[1623:1762,4] <- forecast(arimaFitVar02, h = h)$mean
dfTss05[1623:1762,5] <- forecast(arimaFitVar03, h = h)$mean

writeResults()
# 
# 
# diffTsNoVar02 <- diff(TsNoVar02)
# diffTsVar02 <- diff(TsVar02)
# diffTsVar03 <- diff(TsVar03)

bcTsVar02 <- BoxCox(TsVar02, lambda = "auto")

##use regression for var03
reg <-  matrix(c(diff(bcTsVar02), diff(dfTs$Var01)), ncol = 2)
reg <- rbind(reg, c(0))
arimaFitVar03 <- auto.arima(TsVar03, seasonal = FALSE, xreg = reg)

reg <- matrix(c(rep(mean(diff(bcTsVar02)), h),rep(mean(diff(dfTs$Var01)), h)), ncol = 2)
fArimaRegressors <-  forecast(arimaFitVar03,  h = h, xreg = reg)
autoplot(fArimaRegressors)

##use regression for var02

##append the last cell in the diff to the end to get the right number of rows
reg <- append(diff(dfTs$Var01), diff(dfTs$Var01)[1621])
arimaFitVar02 <- auto.arima(TsVar02, seasonal = FALSE, xreg = reg)
fArimaRegressors <-  forecast(arimaFitVar02,  h = h, xreg = reg)
autoplot(fArimaRegressors)

