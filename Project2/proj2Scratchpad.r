###note to self, remember any transform needs to happen on both train and test

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

setwd("C:\\school\\cuny\\624\\cuny624HW\\Project2")
project_df <-read.csv("StudentData.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)


#cleanup
project_df$Brand.Code[project_df$Brand.Code==""]<- "N"

#compute imputed values for missing values
project_df_new <- mice(data = project_df, m = 2, method = 'rf', maxit = 3)
#and  recreate the dataframe
project_df <- complete(project_df_new)

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

partition <- function(df)
{
    #divide into test and train
    set.seed(42) #make reproducible
    partitions <- createDataPartition(seq(1, nrow(df)), p = 0.8, list = FALSE)
    dfTrain <<- df[partitions,]
    dfTest <<- df[-partitions,]
    
    PHTrain <<- PH[partitions]
    PHTest <<- PH[-partitions]
    
    #remove result from df 
    # PHTrain <<- dfTrain$PH
    # dfTrain <<- dfTrain[, !(names(dfTrain) %in% c("PH"))]
    # 
    # PHTest <<- dfTest$PH
    # dfTest <<- dfTest[, !(names(dfTest) %in% c("PH"))]

    #    return(df[, !(names(df) %in% c("PH"))])
    
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

partition(project_df)
# let's make an easier to work with name
df <- project_df 
lmFit()

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


skews <- apply(df, 2, skewness)
skews[skews>2]
dfIndBoxCox <- df
#we see three values with skew higher than 2 (outside of dummyvars), so we will apply boxcox transform on those
dfIndBoxCox$Temperature <- predict(BoxCoxTrans(dfIndBoxCox$Temperature), dfIndBoxCox$Temperature)
dfIndBoxCox$Oxygen.Filler <- predict(BoxCoxTrans(dfIndBoxCox$Oxygen.Filler), dfIndBoxCox$Oxygen.Filler)
dfIndBoxCox$Air.Pressurer <- predict(BoxCoxTrans(dfIndBoxCox$Air.Pressurer), dfIndBoxCox$Air.Pressurer)  

partition(dfIndBoxCox)
lmFit()  # very modest improvement in performance

pp <- preProcess(df, method = c("BoxCox", "center", "scale", "pca"))
pp  #22 variables account for 95%
df <- predict(pp, df)


df <- partition(df)
lmFit()

#use cross validation
#use more folds later...minimal until we get a good model
controlObject <- trainControl(method = "repeatedcv", repeats = 1, number = 2)
#first linear regression

set.seed(42)
lr <- train(dfTrain, PHTrain,
            method = "lm",
            trControl = controlObject)
lmPred <- predict(lr, dfTest)
xyplot(PHTest ~ lmPred)
print(cor(PHTest, lmPred))

#next use pls (much worse)
pls <- train(dfTrain, PHTrain,
            method = "pls",
            trControl = controlObject)
plsPred <- predict(pls, dfTest)
xyplot(PHTest ~ plsPred)
print(cor(PHTest, plsPred))

#next use enet?
enet <- train(dfTrain, PHTrain,
             method = "enet",
             preProc = c("center", "scale"),
             trControl = controlObject)
enetPred <- predict(enet, dfTest)
xyplot(PHTest ~ enetPred)
print(cor(PHTest, enetPred))

#SVM  74.61% correlation with tune length of 5, 74.67 if tune length is 15, and time takes forever (20 minutes)
#78.7 if not pca....but takes much longer, do we want pca, else overfitting?
svm <- train(dfTrain, PHTrain,
              method = "svmRadial",
              preProc = c("center", "scale"),
             tuneLength = 5,
              trControl = controlObject)
svmPred <- predict(svm, dfTest)
xyplot(PHTest ~ svmPred)
print(cor(PHTest, svmPred))

nnetGrid <- expand.grid(.decay = c(.001, .01, .1),
                        .size = seq(1, 22, by = 2),
                        .bag = FALSE)
nnetModel <- train(dfTrain, PHTrain,
                   method = "avNNet",
                   tuneGrid = nnetGrid,
                   preProc = c("center", "scale"),
                   linout = TRUE,
                   trace = FALSE,
                   maxit = 1,
                   trControl = controlObject)
nnetPred <- predict(nnetModel, dfTest)
xyplot(PHTest ~ nnetPred)
print(cor(PHTest, nnetPred))

#62%
marsModel <- train(dfTrain, PHTrain,
              method = "earth",
              tuneGrid = expand.grid(.degree = 1, .nprune = 2:25),
              trControl = controlObject)
marsPred <- predict(marsModel, dfTest)
xyplot(PHTest ~ marsPred)
print(cor(PHTest, marsPred))


