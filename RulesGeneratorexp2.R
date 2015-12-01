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
#dataset <-dataset[c(9,8,7,6,5, 4, 3, 2, 1)]

#data
lengthTreePrune = 0
lengthSetRules = 0
errorTreePrune = 0
errorSetRules = 0

lenTreePrune <- c()
eTreePrune <- c()
lenSetRules <- c()
eSetRules <- c()

eTreeBeforePrune <-c()
errorTreeBeforePrune = 0
iter = 5
for (i in 1:iter) {
  dataset.perm<-dataset[sample(nrow(dataset),nrow(dataset)),]
  rownames(dataset.perm) <- NULL
  #partitioning into train and test samples
  dataset.perm.trainBig <- dataset.perm[1:ceiling(nrow(dataset.perm)*0.50),]
  dataset.perm.train <- dataset.perm[1:ceiling(nrow(dataset.perm)*0.25),]
  dataset.perm.prune <- dataset.perm[ceiling(nrow(dataset.perm)*0.25 + 1):ceiling(nrow(dataset.perm)*0.5),]
  dataset.perm.test <- dataset.perm[(ceiling(nrow(dataset.perm)*0.5)+1):nrow(dataset.perm),]
  
  #trainig Model 1
  treeBig <- rpart(dataset.perm.trainBig, method="class", data=dataset.perm.trainBig, minsplit = 30, cp = 0.00001)
  printcp(treeBig)
  plot(treeBig, uniform=TRUE, 
       main="Decision Tree")
  text(treeBig, use.n=FALSE, all=TRUE, cex=.8)  
  
  
  ###
  
  rulesTree = extractRules(treeBig)
  eTreeBig =  ComputeErrorRate(rulesTree, dataset.perm.test)
  errorTreeBeforePrune = errorTreeBeforePrune + eTreeBig
  print ("eTreeBig ")
  print(eTreeBig)
  ###
  
  #pruning tree
  cp.best = treeBig$cptable[which.min(treeBig$cptable[,"xerror"]),"CP"]
  prune.tree <- prune(treeBig, cp = cp.best)
  printcp(prune.tree)
  plot(prune.tree, uniform=TRUE, 
       main="Decision Tree Pruning")
  text(prune.tree, use.n=FALSE, all=TRUE, cex=.8)
  print(cp.best)
  ###
  
  #compute error of pruning tree
  rulesTreePrune = extractRules(prune.tree)
  eTree =  ComputeErrorRate(rulesTreePrune, dataset.perm.test)
  
  
  print ("length of rules tree: ")
  print((lengthOfRules(rulesTreePrune)))
  
  
  
  errorTreePrune = errorTreePrune + eTree
  lengthTreePrune = lengthTreePrune + (lengthOfRules(rulesTreePrune))
  
  
  #training phase Model 2 - set of rules
  tree.small <- rpart(dataset.perm.train, method="class", data=dataset.perm.train, minsplit = 5, cp = 0.00001)
  printcp(tree.small)
  # plot tree 
  plot(tree.small, uniform=TRUE, 
       main="Decision Tree Pruning Model2")
  text(tree.small, use.n=FALSE, all=TRUE, cex=.8)
  rules = extractRules(tree.small)
  
  eTrain = ComputeErrorRate(rules, dataset.perm.train)
  eTest = ComputeErrorRate(rules, dataset.perm.test)
  print ("eTreeBig ")
  print(eTreeBig)
  print ("eTree prune ")
  print(eTree)
  print ("Etrain: ")
  print (eTrain)
  print ("ETest: ")
  print (eTest)
  
  bestRules = pruneSetOfRules(rules, dataset.perm.prune, cp = 0)
  eSetPrune = ComputeErrorRate(bestRules, dataset.perm.test)
  errorSetRules = errorSetRules + eSetPrune  
  lengthSetRules = lengthSetRules + lengthOfRules(bestRules)
  
  
  print ("Statystyka: ")
  print ("d³ugoœæ regu³ Tree Prune: ")
  print (lengthOfRules(rulesTreePrune))
  print ("d³ugoœæ regu³ Set of Rules Prune: ")
  print (lengthOfRules(bestRules))
  
  
  print ("B³¹d Tree Prune: ")
  print (eTree)
  print ("B³¹d Set of Rules Prune: ")
  print (eSetPrune)
  
  lenTreePrune[i] <- lengthOfRules(rulesTreePrune)
  eTreePrune[i] <- eTree
  lenSetRules[i] <- lengthOfRules(bestRules)
  eSetRules[i] <- eSetPrune
  eTreeBeforePrune[i] = eTreeBig
  
}


lengthTreePrune = lengthTreePrune/iter
lengthSetRules = lengthSetRules/iter
errorTreePrune = errorTreePrune/iter
errorSetRules = errorSetRules/iter

errorTreeBeforePrune = errorTreeBeforePrune/iter
print ("Statystyka ostateczna: ")
print ("d³ugoœæ regu³ Tree Prune: ")
print (lengthTreePrune)
print ("d³ugoœæ regu³ Set Rules prune: ")
print (lengthSetRules)

print ("B³¹d Tree: ")
print (errorTreeBeforePrune)
print ("B³¹d Tree Prune: ")
print (errorTreePrune)
print ("B³¹d regu³ Set Rules prune: ")
print (errorSetRules)


lenTreePrune.min<- min(lenTreePrune)
eTreePrune.min <- min(eTreePrune)
lenSetRules.min <- min(lenSetRules)
eSetRules.min <- min(eSetRules)
eTreeBeforePrune.min <- min(eTreeBeforePrune)

lenTreePrune.mean <- mean (lenTreePrune)
eTreePrune.mean <- mean(eTreePrune)
lenSetRules.mean  <- mean (lenSetRules)
eSetRules.mean  <- mean (eSetRules)
eTreeBeforePrune.mean <- mean(eTreeBeforePrune)

errors.min <- t(data.frame(M1before = eTreeBeforePrune.min,M1 = eTreePrune.min, M2Prune = eSetRules.min))
length.min <- t(data.frame(M1 = lenTreePrune.min, M2 = lenSetRules.min))
errors.mean <- t(data.frame(M1before = eTreeBeforePrune.mean, M1 = eTreePrune.mean, M2Prune = eSetRules.mean))
length.mean <- t(data.frame(M1 = lenTreePrune.mean, M2 = lenSetRules.mean))


png(filename="error_min2.png", height=300, width=800,bg="white")
barplot(as.matrix(errors.min), main="", ylab= "Minimalny b³¹d klasyfikacji", names.arg = c("Drzewo przed przyciêciem", "Drzewo po przyciêciu", "Zestaw Regu³ po przyciêciu"),
        beside=TRUE, col=rainbow(3))
dev.off()

png(filename="len_min2.png", height=300, width=600,bg="white")
barplot(as.matrix(length.min), main="", ylab= "Minimalna d³ugoœæ zestawu regu³", names.arg = c("Drzewo po przyciêciu", "Zestaw Regu³ po przyciêciu"),
        beside=TRUE, col=rainbow(2))

dev.off()


png(filename="error_mean2.png", height=300, width=800,bg="white")
barplot(as.matrix(errors.mean), main="", ylab= "Œredni b³¹d klasyfikacji", names.arg = c("Drzewo przed przyciêciem", "Drzewo po przyciêciu", "Zestaw Regu³ po przyciêciu"),
        beside=TRUE, col=rainbow(3))
dev.off()

png(filename="len_mean2.png", height=300, width=600,bg="white")
barplot(as.matrix(length.mean), main="", ylab= "Œrednia d³ugoœæ zestawu regu³", names.arg = c("Drzewo po przyciêciu", "Zestaw Regu³ po przyciêciu"),
        beside=TRUE, col=rainbow(2))

dev.off()
