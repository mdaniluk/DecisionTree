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
# grow tree 
fit <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis, minsplit = 20, cp = 0.002)

printcp(fit) # display the results 
#plotcp(fit) # visualize cross-validation results 
#summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


rules = extractRules(fit)
len = lengthOfRules(rules)
predictions = PredictAll(rules,kyphosis)

e = ComputeErrorRate(rules, kyphosis)

bestRules = pruneSetOfRules(rules, kyphosis, cp = 0.01)
e2 = ComputeErrorRate(bestRules, kyphosis)