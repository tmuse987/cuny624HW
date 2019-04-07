source("proj1_loadAndCleanup.r")
autoplot(TsNoVar02)
autoplot(TsVar02)

diffTsNoVar02 <- diff(TsNoVar02)
diffTsVar02 <- diff(TsVar02)


#plot using some basic functions (e.g., naive)
#naive(TsVar02)

compResiduals <- function(Ts)
{
    resMean <- residuals(meanf(Ts))
    resNaive <- residuals(naive(Ts))
    resSeasonalNaive <- residuals(snaive(Ts))
    df <- data.frame(resMean, resNaive, resSeasonalNaive)
    colnames(df) <- c("Mean", "Naive", "SeasonalNaive")
    return(df)
}


plotTS <- function(Ts, column, h, PI)
{
    if(column == "")
    {
        plot(autoplot(window(Ts, start = 7))  +
            autolayer(meanf(Ts, h = h), series = "Mean", PI = PI) +
            autolayer(naive(Ts, h = h), series = "Naive", PI = PI) +
            autolayer(snaive(Ts, h = h), series = "Naive Season", PI = PI))
        resMean <- residuals(meanf(Ts))
        resNaive <- residuals(naive(Ts))
        resSNaive <- residuals(snaive(Ts))
    }
    else
    {
        plot(autoplot(window(Ts[,column], start = 7))  +
            autolayer(meanf(Ts[,column], h = h), series = "Mean", PI = PI) +
            autolayer(naive(Ts[,column], h = h), series = "Naive", PI = PI) +
            autolayer(snaive(Ts[,column], h = h), series = "Naive Season", PI = PI))
        resMean <- residuals(meanf(Ts[, column]))
        resNaive <- residuals(naive(Ts[, column]))
        resSNaive <- residuals(snaive(Ts[, column]))
    }
    
    plot(autoplot(resMean, series = "Mean") +
        autolayer(resNaive, series = "Naive") +
        autolayer(resSNaive, series = "Naive Season"))
    
    plot(gghistogram(resMean) + ggtitle("Mean Residuals"))
    plot(gghistogram(resNaive) + ggtitle("Naive Residuals"))
    plot(gghistogram(resSNaive) + ggtitle("Seasonal Naive Residuals"))
}

h = 100

#mean residuals don't look reasonable
plotTS(TsVar02, "", h, FALSE)
#naive looks reasonable for residuals, others don't
plotTS(TsNoVar02,"Var03", h, FALSE)

#mean residuals don't look reasonable
plotTS(TsVar02, "", h, FALSE)
#naive looks reasonable for residuals, others don't
plotTS(TsNoVar02,"Var03", h, TRUE)



