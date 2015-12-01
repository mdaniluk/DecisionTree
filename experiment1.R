#clean workspace
rm(list = ls())

# Classification Tree with rpart
library(rpart)

#include files
source("extractRules.R")
source("treeClasses.R")
source("predict.R")
source("ComputeErrorRate.R")
source("Prune.R")

# load datasets
dataset <- read.table(file = "balance-scale.data", sep = ",")

#dataset <- read.table(file = "pima-indians-diabetes.data", sep = ",")
#dataset <-dataset[c(9, 8, 7, 6, 5, 4, 3, 2, 1)]

#data
lengthOfRulesModel1 = 0
errorModel1 = 0
lengthOfRulesModel2 = 0
lengthOfRulesModel2Pruning = 0
errorModel2onTrainData = 0
errorModel2 = 0
errorModel2Pruning = 0

lenM1 <- c()
eM1 <- c()
lenM2 <- c()
eM2train <- c()
eM2 <- c()
eM2Prune <- c()


iter = 2
for (i in 1:iter) {
  dataset.perm<-dataset[sample(nrow(dataset),nrow(dataset)),]
  rownames(dataset.perm) <- NULL
  #partitioning into train and test samples
  dataset.perm.trainBig <- dataset.perm[1:ceiling(nrow(dataset.perm)*0.50),]
  dataset.perm.train <- dataset.perm[1:ceiling(nrow(dataset.perm)*0.25),]
  dataset.perm.prune <- dataset.perm[ceiling(nrow(dataset.perm)*0.25 + 1):ceiling(nrow(dataset.perm)*0.5),]
  dataset.perm.test <- dataset.perm[(ceiling(nrow(dataset.perm)*0.5)+1):nrow(dataset.perm),]
  
  #trainig Model 1
  treeBig <- rpart(dataset.perm.trainBig, method="class", data=dataset.perm.trainBig, minsplit = 1, cp = 0.005)
  printcp(treeBig)
  # plot tree 
  plot(treeBig, uniform=TRUE, 
       main="Decision Tree Model 1")
  text(treeBig, use.n=FALSE, all=TRUE, cex=.8)
  rulesBig = extractRules(treeBig)
  eTrainBig =  ComputeErrorRate(rulesBig, dataset.perm.test)
  print(eTrainBig)
  print ("length of rules big: ")
  print((lengthOfRules(rulesBig)))
  errorModel1 = errorModel1 + eTrainBig
  lengthOfRulesModel1 = lengthOfRulesModel1 + (lengthOfRules(rulesBig))
  
  #training phase Model 2
  fit <- rpart(dataset.perm.train, method="class", data=dataset.perm.train, minsplit = 1, cp = 0.005)
  printcp(fit)
  # plot tree 
  plot(fit, uniform=TRUE, 
       main="Classification Tree for Wines")
  text(fit, use.n=FALSE, all=TRUE, cex=.8)
  rules = extractRules(fit)
  print ('length of rules: ')
  print((lengthOfRules(rules)))
  lengthOfRulesModel2 = lengthOfRulesModel2 + lengthOfRules(rules)
  
  #predictions = PredictAll(rules,dataset.perm.test)
  eTrain = ComputeErrorRate(rules, dataset.perm.train)
  eTest = ComputeErrorRate(rules, dataset.perm.test)
  print (eTrain)
  print (eTest)
  errorModel2onTrainData = errorModel2onTrainData + eTrain
  errorModel2 = errorModel2 + eTest
  bestRules = pruneSetOfRules(rules, dataset.perm.prune, cp = 0)
  ePrune = ComputeErrorRate(bestRules, dataset.perm.test)
  errorModel2Pruning = errorModel2Pruning + ePrune
  print (lengthOfRules(rules))
  print (lengthOfRules(bestRules))
  lengthOfRulesModel2Pruning = lengthOfRulesModel2Pruning + lengthOfRules(bestRules)
  
  
  print ("Statystyka: ")
  print ("d³ugoœæ regu³ Model 1: ")
  print (lengthOfRules(rulesBig))
  print ("d³ugoœæ regu³ Model 2: ")
  print (lengthOfRules(rules))
  print ("D³ugoœæ regu³ model 2 po przycieiu: ")
  print (lengthOfRules(bestRules))
  
  
  print ("B³¹d modelu 1: ")
  print (eTrainBig)
  print ("B³¹d modelu 2 po przycieciu: ")
  print (ePrune)
  print ("B³¹d modelu 2 przed przycieciem: ")
  print (eTest)
  print ("B³¹d modelu 2 na danych trenujacych: ")
  print (eTrain)
  
  lenM1[i] <- lengthOfRules(rulesBig)
  eM1[i]  <- eTrainBig
  lenM2[i] <- lengthOfRules(bestRules)
  eM2train[i] <- eTrain
  eM2[i] <- eTest
  eM2Prune[i] <- ePrune
  
}

lengthOfRulesModel1 = lengthOfRulesModel1/iter
errorModel1 = errorModel1/iter
lengthOfRulesModel2 = lengthOfRulesModel2/iter
lengthOfRulesModel2Pruning = lengthOfRulesModel2Pruning/iter
errorModel2onTrainData = errorModel2onTrainData/iter
errorModel2 = errorModel2/iter
errorModel2Pruning = errorModel2Pruning/iter

print ("Statystyka ostateczna: ")
print ("d³ugoœæ regu³ Model 1: ")
print (lengthOfRulesModel1)
print ("d³ugoœæ regu³ Model 2: ")
print (lengthOfRulesModel2)
print ("D³ugoœæ regu³ model 2 po przycieiu: ")
print (lengthOfRulesModel2Pruning )


print ("B³¹d modelu 1: ")
print (errorModel1)
print ("B³¹d modelu 2 po przycieciu: ")
print (errorModel2Pruning)
print ("B³¹d modelu 2 przed przycieciem: ")
print (errorModel2 )
print ("B³¹d modelu 2 na danych trenujacych: ")
print (errorModel2onTrainData)

lenM1.min <- min(lenM1)
eM1.min <- min(eM1)
lenM2.min <- min(lenM2)
eM2train.min <- min(eM2train)
eM2.min <- min(eM2)
eM2Prune.min <- min(eM2Prune)

lenM1.mean <- mean (lenM1)
eM1.mean <- mean(eM1)
lenM2.mean  <- mean (lenM2)
eM2train.mean  <- mean (eM2train)
eM2.mean  <- mean (eM2)
eM2Prune.mean  <- mean (eM2Prune)


errors.min <- t(data.frame(M1 = eM1.min, M2Prune = eM2Prune.min, M2 = eM2.min, M2Train = eM2train.min))
length.min <- t(data.frame(M1 = lenM1.min, M2 = lenM2.min))
errors.mean <- t(data.frame(M1 = eM1.mean, M2Prune = eM2Prune.mean, M2 = eM2.mean, M2Train = eM2train.mean))
length.mean <- t(data.frame(M1 = lenM1.mean, M2 = lenM2.mean))


png(filename="error_min.png", height=300, width=800,bg="white")
barplot(as.matrix(errors.min), main="", ylab= "Minimalny b³¹d klasyfikacji", names.arg = c("Model 1", "Model 2", "Model 2 przed przyciêciem", "Model 2 na dan. trenuj¹cych"),
        beside=TRUE, col=rainbow(4))
dev.off()

png(filename="len_min.png", height=300, width=600,bg="white")
barplot(as.matrix(length.min), main="", ylab= "Minimalna d³ugoœæ zestawu regu³", names.arg = c("Model 1", "Model 2"),
        beside=TRUE, col=rainbow(2))

dev.off()


png(filename="error_mean.png", height=300, width=800,bg="white")
barplot(as.matrix(errors.mean), main="", ylab= "Œredni b³¹d klasyfikacji", names.arg = c("Model 1", "Model 2", "Model 2 przed przyciêciem", "Model 2 na dan. trenuj¹cych"),
        beside=TRUE, col=rainbow(4))
dev.off()

png(filename="len_mean.png", height=300, width=600,bg="white")
barplot(as.matrix(length.mean), main="", ylab= "Œrednia d³ugoœæ zestawu regu³", names.arg = c("Model 1", "Model 2"),
        beside=TRUE, col=rainbow(2))

dev.off()

