#clean workspace
rm(list = ls())

# Classification Tree with rpart
library(rpart)

#include files
source("treeClasses.R")

# grow tree 
fit <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis)#,
            # minsplit = 10, cp = 0.0001)

printcp(fit) # display the results 
#plotcp(fit) # visualize cross-validation results 
#summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
