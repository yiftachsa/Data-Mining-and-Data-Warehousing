#utils:::menuInstallPkgs())
#install.packages("arules")
#install.packages("rattle")
#install.packages("RColorBrewer")

library("caret")
library("arules")
library("rpart")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#1.1 loading the dataset
DS<-read.csv("D:\\Documents\\Studies\\Documents for higher education\\Courses\\Year 3 Semester 2\\מדעי הנתונים ובינה עסקית\\עבודות להגשה\\תרגיל 2\\Ex-2_dataset.csv")
#Replacing the empty strings with NA
DS[DS==""]<-NA
#Removing the irrelavent feature
DS$Request_Number<-NULL
#Seting the features as the appropriate data types
DS$Credit_History<-as.factor(DS$Credit_History)
#1.2 Data partitioning
#Getting the training set records indexes
trainingIndex<-createDataPartition(y=DS$Request_Approved,p=0.7, list=FALSE)
#Initializing sets function
initSets<- function(dataset, trainingIndex){
  trainSet<-dataset[trainingIndex,]
  testSet<-dataset[-trainingIndex,]
  return(list(trainSet, testSet))
}
#calling initSets
sets<-initSets(DS,trainingIndex)
trainSet<-sets[[1]]
testSet<-sets[[2]]
#1.3
#1.3.1 - replacing NA with columns mean
#The numeric features "Monthly_Profit" and "Spouse_Income" have no NA values.

#Calculating the required numeric columns means
numericDS<-DS[,c( "Loan_Amount", "Payment_Terms")]
numericColMean<-colMeans(numericDS, na.rm = TRUE)
#Getting the missing values indexes for numeric features with missing values
numericNAIndexLoan<-which(is.na(numericDS$Loan_Amount), arr.ind=TRUE)
numericNAIndexPayment<-which(is.na(numericDS$Payment_Terms), arr.ind=TRUE)
#Replacing the missing values with their respective features means
DS$Loan_Amount[numericNAIndexLoan]<-numericColMean[[1]]
DS$Payment_Terms[numericNAIndexPayment]<-numericColMean[[2]]


#function to extract the most common values in a dataset. source:https://www.r-bloggers.com/computing-the-mode-in-r/
Mode = function(x){
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}
#Replacing missing valuses in the DS in a given categoric feature
replacingMissingValuseCategoric = function(featureName){
  #Getting the missing values indexes for categoric features with missing values
  NAIndex<-which(is.na(DS[,featureName]), arr.ind=TRUE)
  #Getting the most common value for each catagoric feature
  fMode<-Mode(DS[,featureName])
  #Replacing the missing values with their respective features modes
  DS[,featureName][NAIndex]<-fMode
  return (DS[,featureName])
}
#Replacing the missing valuses in the categoric feature
DS$Employees<-replacingMissingValuseCategoric("Employees")
DS$Credit_History<-replacingMissingValuseCategoric("Credit_History")
DS$Export_Abroad<-replacingMissingValuseCategoric("Export_Abroad")
DS$Gender<-replacingMissingValuseCategoric("Gender")
DS$Married<-replacingMissingValuseCategoric("Married")

#1.4 discretization 
#Receives the number of intervals desired and the feature to discretizise
discretization = function(breakPoints, discMethod, featureName){
  bounderies<-discretize(DS[,featureName], method=discMethod, breaks=breakPoints, onlycuts=TRUE)
  for(i in 1:(length(bounderies)-1)) {
    intervalIndexes<-which((DS[,featureName]>bounderies[i]) & (DS[,featureName]<=bounderies[i+1]), arr.ind = TRUE)
    if(i == 1){
      intervalIndexes<-which((DS[,featureName]>=bounderies[i]) & (DS[,featureName]<=bounderies[i+1]), arr.ind = TRUE)
    }
    intervalValues<-DS[,featureName][intervalIndexes]
    DS[,featureName][intervalIndexes]<-mean(intervalValues)
  }
  return (DS[,featureName])
}
#Discretization
#Divide Monthly_Profit into 5 equal depth intervals and assigining each interval it's mean value.
DS$Monthly_Profit<-discretization(5,"frequency","Monthly_Profit")
#Divide Spouse_Income into 4 interval and assigining each interval it's mean value.
#the bounderies were determined to provide a good coverage over the values range - including an interval only for the records with the value - 0
DS$Spouse_Income<-discretization(c(0,0.1,1820,2850,41667), "fixed","Spouse_Income")
#Divide Loan_Amount into 5 equal depth intervals and assigining each interval it's mean value.
DS$Loan_Amount<-discretization(5,"frequency", "Loan_Amount")



#2
#Model generation
#calling initSets initilzaing trainingSet and testSet
sets<-initSets(DS,trainingIndex)
trainSet<-sets[[1]]
testSet<-sets[[2]]

#Gini, Minsplit=5% of the number of records
treeModelGiniMinSplit50 <- rpart(Request_Approved ~.,data = trainSet, method="class", minsplit=50, cp = -1) #cp=-1 do not pre-prune the tree
#Potential pruninig to the tree. 
Prune_treeModelGiniMinSplit50<- prune(treeModelGiniMinSplit50, cp=treeModelGiniMinSplit50$cptable[which.min(treeModelGiniMinSplit50$cptable[,"xerror"]),"CP"])

#Gini, Minsplit=10% of the number of records
treeModelGiniMinSplit100 <- rpart(Request_Approved ~.,data = trainSet, method="class", minsplit=100, cp = -1) #cp=-1 do not pre-prune the tree
#Potential pruninig to the tree. 
Prune_treeModelGiniMinSplit100<- prune(treeModelGiniMinSplit100, cp=treeModelGiniMinSplit100$cptable[which.min(treeModelGiniMinSplit100$cptable[,"xerror"]),"CP"])

#Information Gain, Minsplit=5% of the number of records
treeModelIGMinSplit50 <- rpart(Request_Approved ~.,data = trainSet, method="class",parms = list(split = 'information'), minsplit=50, cp = -1) #cp=-1 do not pre-prune the tree
#Potential pruninig to the tree. 
Prune_treeModelIGMinSplit50<- prune(treeModelIGMinSplit50, cp=treeModelIGMinSplit50$cptable[which.min(treeModelIGMinSplit50$cptable[,"xerror"]),"CP"])

#Information Gain, Minsplit=10% of the number of records
treeModelIGMinSplit100 <- rpart(Request_Approved ~.,data = trainSet, method="class",parms = list(split = 'information'), minsplit=100, cp = -1) #cp=-1 do not pre-prune the tree
#Potential pruninig to the tree. 
Prune_treeModelIGMinSplit100<- prune(treeModelIGMinSplit100, cp=treeModelIGMinSplit100$cptable[which.min(treeModelIGMinSplit100$cptable[,"xerror"]),"CP"])

#Plot
#fancyRpartPlot(treeModelIGMinSplit50, caption = NULL,cex = 0.5, tweak=0.8)


#3 Evaluation
testSetClasses<-DS$Request_Approved[-trainingIndex]
#prediction for each Model
predictionGiniMinSplit50<-predict(treeModelGiniMinSplit50,newdata=testSet, type="class")
cmGini50<-confusionMatrix(predictionGiniMinSplit50,reference=testSetClasses)

predictionGiniMinSplit100<-predict(treeModelGiniMinSplit100,newdata=testSet, type="class")
cmGini100<-confusionMatrix(predictionGiniMinSplit100,reference=testSetClasses)

predictionIGMinSplit50<-predict(treeModelIGMinSplit50,newdata=testSet, type="class")
cmIG50<-confusionMatrix(predictionIGMinSplit50,reference=testSetClasses)

predictionIGMinSplit100<-predict(treeModelIGMinSplit100,newdata=testSet, type="class")
cmIG100<-confusionMatrix(predictionIGMinSplit100,reference=testSetClasses)


