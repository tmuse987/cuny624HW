library(tidyverse)
library(fpp2)
#library(writexl)
library(openxlsx)

dfTimeSeries <- readxl::read_excel("Set for Class.XLS")
#timeSeries <- openxlsx::read.xlsx("Set for Class.XLS", sheet = 1)

dfTss01 <- filter(dfTimeSeries, group == "S01")
dfTss02 <- filter(dfTimeSeries, group == "S02")
dfTss03 <- filter(dfTimeSeries, group == "S03")
dfTss04 <- filter(dfTimeSeries, group == "S04")
dfTss05 <- filter(dfTimeSeries, group == "S05")
dfTss06 <- filter(dfTimeSeries, group == "S06")

sheets <- list(dfTss01,dfTss02, dfTss03, dfTss04, dfTss05, dfTss06, dfTimeSeries)
#write.xlsx(sheets, "groupSeparated.xlsx")  --commented out for time, uncomment to write

dfTs <- subset(dfTss05, select = -group)
dfTs <- slice(dfTs, 1:1622) #remove forecast cells for now

#data cleanup--five rows with na's
dfTs[rowSums(is.na(dfTs))>0,]
#mutate them from next value

dfTs[rowSums(is.na(dfTs))>0,]
while(nrow(dfTs[rowSums(is.na(dfTs))>0,]) > 0)
{           
    
    dfTs <- transmute(dfTs, 
                      SeriesInd = SeriesInd,
                      Var01 = if_else(is.na(Var01), lead(Var01), Var01),
                      Var02 = if_else(is.na(Var02), lead(Var02), Var02),
                      Var03 = if_else(is.na(Var03), lead(Var03), Var03),
                      Var05 = if_else(is.na(Var05), lead(Var05), Var05),
                      Var07 = if_else(is.na(Var07), lead(Var07), Var07))
    print(dfTs[rowSums(is.na(dfTs))>0,])
}


Ts <- ts(dfTs, frequency = 5*52)

#broken up into two to make plotting easier
TsNoVar02 <- ts(dfTs[,c(-1,-3)], frequency = 5*52)
TsVar02 <- ts(dfTs[,3], frequency = 5*52)
TsVar03 <- ts(dfTs[,4], frequency = 5*52)


writeResults <- function()
{
    sheets <- list(dfTss01,dfTss02, dfTss03, dfTss04, dfTss05, dfTss06, dfTimeSeries)
    write.xlsx(sheets, "proj1Output.xlsx")
}

