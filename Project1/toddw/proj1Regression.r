source("proj1_loadAndCleanup.r")
h <- 150

# 
# fitVar02 <- tslm(TsVar02 ~ trend + season)
# fCastVar02 <- forecast(fitVar02, h =h)
# autoplot(fCastVar02)


fCastAndPlot <- function(fitTs, var)
{
    fcast <- forecast(fitTs,
                      newdata = data.frame(
                          Var02 = rep(mean(Ts[,"Var02"]), h),
                          Var01 = rep(mean(Ts[,"Var01"]), h),
                          Var05 = rep(mean(Ts[,"Var05"]), h),
                          Var07 = rep(mean(Ts[,"Var07"]), h),
                          Var03 = rep(mean(Ts[,"Var03"]), h)
                      ) )
    
    
    autoplot(window(Ts[,var], start =5)) + 
        autolayer(fcast)
}

fitTs <- tslm(Var03 ~ trend + season, data = Ts)
fCastAndPlot(fitTs, "Var03")

fitTs <- tslm(Var03 ~ trend, data = Ts)
fCastAndPlot(fitTs, "Var03")

fitTs <- tslm(Var03 ~ season, data = Ts)
fCastAndPlot(fitTs, "Var03")

fitTs <- tslm(Var03 ~ Var02 + trend + season , data = Ts)
fCastAndPlot(fitTs, "Var03")

fitTs <- tslm(Var03 ~ Var02 + Var01 + Var05 + Var07 + trend + season , data = Ts)
fCastAndPlot(fitTs, "Var03")


fitTs <- tslm(Var02 ~ trend + season, data = Ts)
fCastAndPlot(fitTs, "Var02")

fitTs <- tslm(Var02 ~ trend, data = Ts)
fCastAndPlot(fitTs, "Var02")

fitTs <- tslm(Var02 ~ season, data = Ts)
fCastAndPlot(fitTs, "Var02")


fitTs <- tslm(Var02 ~ Var03 + Var01 + Var05 + Var07 + trend + season , data = Ts)
fCastAndPlot(fitTs, "Var02")



#




#correlations  obviously everything is highly correlated 
#stock prices match 1 to 1 almost as expected
#trading volume ~63-65% correlation to price
cor(dfTs[,'Var01'],dfTs[,'Var02'] )
cor(dfTs[,'Var01'],dfTs[,'Var03'] )
cor(dfTs[,'Var01'],dfTs[,'Var05'] )
cor(dfTs[,'Var01'],dfTs[,'Var07'] )
cor(dfTs[,'Var03'],dfTs[,'Var05'] )
cor(dfTs[,'Var03'],dfTs[,'Var02'] )
cor(dfTs[,'Var05'],dfTs[,'Var02'] )
cor(dfTs[,'Var07'],dfTs[,'Var02'] )



