source("proj1_loadAndCleanup.r")

# autoplot(Ts)
# autoplot(TsVar02)
# 

autoplot(TsNoVar02)
autoplot(TsVar02)

#nothing useful from this...it seems

#appears some seasonality, with drops in "middle" of year
ggseasonplot(TsNoVar02[,1])
ggseasonplot(TsNoVar02[,2])
ggseasonplot(TsNoVar02[,3])
ggseasonplot(TsNoVar02[,4])
ggseasonplot(TsVar02)


diffTsNoVar02 <- diff(TsNoVar02)
diffTsVar02 <- diff(TsVar02)
autoplot(diffTsNoVar02)
autoplot(diffTsVar02)



#acf(diff(Ts))
acf(TsVar02)
acf(TsNoVar02)

acf(diff(TsVar02), )
ggAcf(diffTsVar02)
acf(diffTsNoVar02)





