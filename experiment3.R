#clean workspace
rm(list = ls())

# Classification Tree with rpart
library(rpart)
library(RWeka)
#include files
source("extractRules.R")
source("treeClasses.R")
source("predict.R")
source("ComputeErrorRate.R")
source("Prune.R")

# load datasets
dataset <- read.table(file = "balance-scale.data", sep = ",")
C1 <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
C2 <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
C3 <- make_Weka_classifier("weka/classifiers/meta/AdaBoostM1")

iter = 5
error1 <- c()
error2 <- c()
error3 <- c()
error4 <- c()
for (i in 1:iter) {
  dataset.perm<-dataset[sample(nrow(dataset),nrow(dataset)),]
  rownames(dataset.perm) <- NULL
  #partitioning into train and test samples
  dataset.perm.trainBig <- dataset.perm[1:ceiling(nrow(dataset.perm)*0.50),]
  dataset.perm.train <- dataset.perm[1:ceiling(nrow(dataset.perm)*0.25),]
  dataset.perm.prune <- dataset.perm[ceiling(nrow(dataset.perm)*0.25 + 1):ceiling(nrow(dataset.perm)*0.5),]
  dataset.perm.test <- dataset.perm[(ceiling(nrow(dataset.perm)*0.5)+1):nrow(dataset.perm),]
  
  model1 <- C1(paste(colnames(dataset.perm.trainBig)[1],"~ ."), data = dataset.perm.trainBig)
  model1.evaluation <- evaluate_Weka_classifier(model1, newdata=dataset.perm.test)
  e1 <- model1.evaluation$details[2]
  print (e1)
  
  model2 <-C2(paste(colnames(dataset.perm.trainBig)[1],"~ ."), data = dataset.perm.trainBig)
  model2.evaluation <- evaluate_Weka_classifier(model2, newdata=dataset.perm.test)
  e2 <- model2.evaluation$details[2]
  print (e2)
  
  model3 <-C3(paste(colnames(dataset.perm.trainBig)[1],"~ ."), data = dataset.perm.trainBig)
  model3.evaluation <- evaluate_Weka_classifier(model3, newdata=dataset.perm.test)
  e3 <- model3.evaluation$details[2]
  print (e3)
  
  
  
  tree <- rpart(dataset.perm.train, method="class", data=dataset.perm.train, minsplit = 20, cp = 0.01)
  #printcp(tree)
  # plot tree 
  plot(tree, uniform=TRUE, 
       main="Decision Tree")
  text(tree, use.n=FALSE, all=TRUE, cex=.8)
  rules = extractRules(tree)
  
  eTest = ComputeErrorRate(rules, dataset.perm.test)
  print ("Tree: ")
  print (eTest)
  
  bestRules = pruneSetOfRules(rules, dataset.perm.prune, cp = 0)
  eSetPrune = ComputeErrorRate(bestRules, dataset.perm.test)
  print ("Rules: ")
  print (eSetPrune)
  error1[i] <- e1/100
  error2[i] <- e2/100
  error3[i] <- e3/100
  error4[i] <- eSetPrune
  
}

error1.mean = mean(error1)
error2.mean = mean(error2)
error3.mean = mean(error3)
error4.mean = mean(error4)

errors.mean <- t(data.frame(er1 = error1.mean, er2 = error2.mean, er3 = error3.mean, er4 = error4.mean))

png(filename="exp3.png", height=300, width=800,bg="white")
barplot(as.matrix(errors.mean), main="", ylab= "Œredni b³¹d klasyfikacji", names.arg = c("Naive Bayes", "Random Forest", "AdaBoostM1", "Set of Rules"),
        beside=TRUE, col=rainbow(4))
dev.off()
print ("XXXXXXX")
print ("e1")
print (error1.mean/100)
print ("e2")
print (error2.mean/100)
print ("e3")
print (error3.mean/100)
print ("e4")
print (error4.mean/100)