#Before we begin, let's set up the environment

library(ISLR)
library(dplyr)
library(pls)
library(tree)
library(randomForest)

##My first step is importing the data set using the 'Import Dataset'function available
##in the environment.

ProblemSet3_Data <- read.csv("C:/Users/crort/OneDrive - orgoca/orgoca/Documents/HKS/Fall/API222 (Machile Learning)/ProblemSet3_Data.csv")

str(ProblemSet3_Data)

colnames(ProblemSet3_Data)

#Create a new data set so I can keep going back to the original data as I mess up

PSData <- ProblemSet3_Data

#Eliminate the columns indicated in PS3

PSData$MiscFeature <- NULL
PSData$Fence <- NULL
PSData$PoolQC <- NULL
PSData$FireplaceQu <- NULL
PSData$Alley <- NULL

Cleandata <- PSData

str(Cleandata)

#I created this loop to check each column class type. If class is integer then
#it forces a change to numeric. 

i = 1

while(i < (ncol(Cleandata)+1))
{
  lolo <- class(Cleandata[,i])
  if(lolo=='integer'){
    Cleandata[,i] <- as.numeric(Cleandata[,i])
    print(class(Cleandata[,i]))
    } 
  else{ print(class(Cleandata[,i]))}
   i=i+1
}  

##Check if values are preserved, as per PS3 recommendation

View(Cleandata)

#1.a.How many predictors are in the data set (after you drop 
#the variables according to the directions)?

dim(Cleandata)

#75 predictors

#1.b.How many observations have missing values? 

sum(is.na(Cleandata))

#There are 868 NA values

j = 1
coco = 0

while(j < (ncol(Cleandata)+1))
{
  lilo <- sum(is.na(Cleandata[,j]))
  if(lilo>0) {coco <- coco+1} else {coco <- coco+0}    
     j = j+1
}
  
print(coco)

#There are 14 predictors that have NA values

Cleandata <- na.omit(Cleandata)

#Now I have 75 predictors with 1,094 observations each.

#How many categorical string variables are in the data set? 

j = 1
cico = 0

while(j < (ncol(Cleandata)+1))
{
  lilo <- class(Cleandata[,j])
  if(lilo=='factor') {cico <- cico+1} else {cico <- cico+0}    
  j = j+1
}
 
print(cico)
 
#There are 38 categorical string variables in the data set.

str(Cleandata)

#Let's convert categorical to dummy variables

for (i in ncol(Cleandata):1) {
  if (is.factor(Cleandata[,i])) {
    for (j in unique(Cleandata[,i])) {
      new_col             <- paste(colnames(Cleandata)[i], j, sep = "_")
      Cleandata[,new_col] <- as.numeric(Cleandata[,i] == j)  
    } 
    Cleandata       <- Cleandata[,-i]                        
  }
}

#now let's count the variables with variance <0.05

View(Cleandata)

cilco = 0
j = 1

while(j < (ncol(Cleandata)+1))
{
  if(var(Cleandata[,j])<0.05) {cilco <- cilco+1} else {cilco <- cilco+0}   
  print(var(Cleandata[,j]))
   j = j+1
}
print(cilco)

dim(Cleandata)

#There are 145 predictors with a variance lower than 0.05. Now lets eliminate
#those columns. For reference, I start with a dim of 1094 260. I should expect
#115 columns after cleaning up.

for (i in ncol(Cleandata):1)
{
  if(var(Cleandata[,i])<0.05) {Cleandata[,i] <- NULL} 
}

dim(Cleandata)

#Data is now fully clean. Lets start working with the training/test data sets

smp_size <- floor(0.80 * nrow(Cleandata))

set.seed(222)

tr_ind <- sample(seq_len(nrow(Cleandata)), size = smp_size)

train <- Cleandata[tr_ind, ]
test <- Cleandata[-tr_ind, ]

dim(train)
dim(test)

#Awesome! Train is 875 115 and test is 219 115 
#Let's start doing some Machine Learning!

#2.Run Principal Components Regression on the training data.

View(Cleandata$SalePrice)

?pcr()

PCR = pcr(SalePrice~., data = train, scale = TRUE, validation = "CV")

summary(PCR)

validationplot(PCR, val.type = "RMSE", type = "b")

pcr_msep          <- MSEP(PCR)

pcr_rmsep         <- RMSEP(PCR)




#2.a.The model with 5PC improves in 49% over the model with 0PC, so yes
#this is a big improvement.

#2.b.The improvement between 5 and 10 PC's is of only ~2%.

#2.c.I have two large drops (from 37% PC0 to PC1 and 12% from PC2 to PC3)
#yet all the other drops are consistently under 1%.

#3.a.Principal component 24 with an RSME of 38,423 is the best CV RSME

#3.b.CVRSME=38,423

pcr_pred = predict(PCR, test, ncomp=24)

View(pcr_pred)

pcr_test_MSE      <- mean((pcr_pred - test$SalePrice)^2)

print(pcr_test_MSE)

sqrt(pcr_test_MSE)

#3.c. Test RSME = 37,294.97

##4. Plot is included in word write up submitted with the assignment!

##Partial Least Squares

pls_fit = plsr(SalePrice~., data = train, scale = TRUE, validation = "CV")

RSME_TEST <- RMSEP(pls_fit)

View(pls_fit)

which.min(RSME_TEST$val[1,1,])

#5.a. Number of components correspond to lowest CV RSME = 2

#5.b. What is the CV RMSE = 38,921

PLS_pred = predict(pls_fit, test, ncomp=2)

View(PLS_pred)

pls_test_MSE      <- mean((PLS_pred - test$SalePrice)^2)

print(sqrt(pls_test_MSE))

#5.c. What is the test RMSE = 36,379

##Decision trees

DecisionTree = tree(SalePrice~., data = data.frame(train))

##Cross Validation

?cv.tree

OptimalTree <- cv.tree(DecisionTree)

#6.a. What is the optimal size = 10 trees

#6.b.Plot is included in the word write up!

DTree_pred = predict(DecisionTree, data.frame(test), best=10)

View(DTree_pred)

Tree_test_MSE      <- mean((DTree_pred - test$SalePrice)^2)

print(sqrt(Tree_test_MSE))

#6.c. Test RSME for Tress = 47,048

##Random forests

?randomForest

rndfrst_fit = randomForest(SalePrice~., data = data.frame(train))

View(rndfrst_fit)

RndFrst_pred = predict(rndfrst_fit, data.frame(test))

RndFrst_test_MSE      <- mean((RndFrst_pred - test$SalePrice)^2)

print(sqrt(RndFrst_test_MSE))

#7. What is the test RSME = 34,098

#8. I would use random forests since it is yielding the lowest RSME (34,098)