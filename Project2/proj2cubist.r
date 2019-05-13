(mgcv)###note to self, remember any transform needs to happen on both train and test

library(naniar)
library(corrplot)
library(caret)
library(corrr)
library(mice)
library(randomForest)
library(dplyr)
library(caret)
library(plyr)
library(e1071)
library(Cubist)
library(gbm)
library(ModelMetrics)

setwd("C:\\school\\cuny\\624\\cuny624HW\\Project2")



###commented out, we will just load "cleaned up file"
#cleanup
# project_df <-read.csv("StudentData.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)
# project_df$Brand.Code[project_df$Brand.Code==""]<- "N"
# 
# #compute imputed values for missing values
# project_df_new <- mice(data = project_df, m = 2, method = 'rf', maxit = 3)
# #and  recreate the dataframe
# project_df <- complete(project_df_new)
# write.csv(project_df, file = "StudentDataClean.csv")

project_df <-read.csv("StudentDataClean.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)
##remove sequence number written out previously
project_df <- project_df[,-1] 
PHTest <- NULL
dfTest <- NULL
PHTrain <- NULL
dfTrain <- NULL
PH <- project_df$PH
project_df <<- project_df[, !(names(project_df) %in% c("PH"))]

#create dummy variables for the categorical brand.code

dummys <- dummyVars(~Brand.Code, data=project_df)
dfDummys <- data.frame(predict(dummys, project_df))
project_df <- bind_cols(project_df, dfDummys)
project_df <- project_df[,-1]

set.seed(42) #make reproducible

partition <- function(df, seed)
{
    #divide into test and train
    set.seed(seed)
    partitions <- createDataPartition(seq(1, nrow(df)), p = 0.8, list = FALSE)
    dfTrain <<- df[partitions,]
    dfTest <<- df[-partitions,]
    
    PHTrain <<- PH[partitions]
    PHTest <<- PH[-partitions]
}

lmFit <- function()
{
    lmFitAll <- lm(PHTrain ~ ., data = dfTrain)
    print(summary(lmFitAll))
    
    lmPred <- predict(lmFitAll, dfTest)
    
    #yea, it kinda correlates, but pretty badly :(
    xyplot(PHTest ~ lmPred)
    print(cor(PHTest, lmPred))
}

# let's make an easier to work with name
df <- project_df 
partition(df)

nearZeroVar(df)
#this shows column 13, which is hyd.pressure, probably should be removed
names(df)[13]
df <- df[,-13]
#won't bother to rerun tests yet, difference will be minimal



partition(df)
lmFit()
nearZeroVar(df)
#this shows column 13, which is hyd.pressure, probably should be removed
names(df)[36]
df <- df[,-36]
partition(df)
lmFit()
cor(df)
colsToDrop <- findCorrelation(cor(df), cutoff = 0.75)
df <-(df[-colsToDrop])
partition(df)
lmFit()  ###this made it worse???

df <- project_df 
skews <- apply(df, 2, skewness)
skews[skews>2]
dfIndBoxCox <- df
#we see three values with skew higher than 2 (outside of dummyvars), so we will apply boxcox transform on those
#this improves results marginally 
dfIndBoxCox$Temperature <- predict(BoxCoxTrans(dfIndBoxCox$Temperature), dfIndBoxCox$Temperature)
dfIndBoxCox$Oxygen.Filler <- predict(BoxCoxTrans(dfIndBoxCox$Oxygen.Filler), dfIndBoxCox$Oxygen.Filler)
dfIndBoxCox$Air.Pressurer <- predict(BoxCoxTrans(dfIndBoxCox$Air.Pressurer), dfIndBoxCox$Air.Pressurer)  

df<- dfIndBoxCox


##doing PCA increases RMSE noticably (10% or so), so we won't do it.
pp <- preProcess(df, method = c("BoxCox", "center", "scale", "pca"), thresh = .98)
pp <- preProcess(df, method = c("BoxCox", "center", "scale"))
pp <- preProcess(df, method = c("center", "scale", "corr"))
#pp <- preProcess(df, thresh = .98)
pp  #22 variables account for 95%
df <- predict(pp, df)
df <- partition(df, 666)

#use cross validation
#use more folds later...minimal until we get a good model
controlObject <- trainControl(method = "repeatedcv", repeats = 5, number = 10)
#cubistGrid <- expand.grid(.committees = c(1,5,10,50,75,100),
#                          .neighbors = c(0,1,3,5,7,9))

cubistGrid <- expand.grid(.committees = seq(50,100,10),
                          .neighbors = seq(1,9,2))

set.seed(666)
df <- project_df 
pp <- preProcess(df, method = c("BoxCox", "center", "scale"))
df <- predict(pp, df)
df <- partition(df, 666)
cubistModel <- train(dfTrain, PHTrain,
                     method = "cubist",
                     tuneGrid = cubistGrid,
                     trControl = controlObject)

#73%
cubistPred <- predict(cubistModel, dfTest)
xyplot(PHTest ~ cubistPred)
print(cor(PHTest, cubistPred))
print(rmse(PHTest, cubistPred))

saveRDS(cubistModel, file = "cubistModel.rds")
