# which parts of an rpart tree are needed to plot it?
# a lot of data in the tree object can be removed or replaced by constants (vectors or arbitrary values)
# the aim is to transform the ranger tree to a pseudo rpart-object and plot it like an rpart tree.

library(rpart)
library(rpart.plot)

v <- iris$Species
table(v)

set.seed(1)
treeFit <- rpart(Species~.,data=iris
                 ,method = 'class'
                 , control=rpart.control(maxsurrogate=0, maxcompete=0))
class(treeFit) # rpart , not merely a list
print(treeFit)

rpart.plot(treeFit)#, box.col=c("red", "green"))

Prediction1 <- predict(treeFit,newdata=iris,type = 'class')

## Print the confusion matrix to check the accuracy and other statistics
library(caret)

confusionMatrix(Prediction1,iris$Species)

num.nodes <- 5
mutilTree <-  treeFit
names(mutilTree)
mutilTree$terms <-  NULL
# mutilTree$y <-  NULL # easy to implement, no neet to NULL-ify
mutilTree$ordered <-  NULL
mutilTree$parms <- NULL
mutilTree$functions <- NULL
mutilTree$numresp <- NULL
# mutilTree$splits <- NULL # split values for split nodes # we want to keep this info in the plot
mutilTree$control <- NULL
mutilTree$cptable <- NULL
mutilTree$variable.importance <- NULL
# mutilTree$where <- NULL # easy to implement, no need to set to NULL
# mutilTree$call <- NULL # plots but gives a warning for invalid formula
# mutilTree$frame$wt <- c(1,1,1,1,1) # same as n , no neet to dummify
mutilTree$frame$complexity <- c(1,1,1,1,1) # ok
#mutilTree$frame$yval <- c(1,1,1,1,1) # keep it, don't nullify, don't dummify
mutilTree$frame$dev <- c(1,1,1,1,1) # ok
#mutilTree$frame$n <- c(1,1,1,1,1) # keep it, don't nullify, don't dummify

rpart.plot(mutilTree)
rpart.plot(treeFit)

# we cannot remove mutilTree$splits
# with controls maxsurrogate=0 , maxcompete=0 this table becomes small and simple.

# ...$frame$yval2
# row-numbers correspond to nodes
# 1st column: mayority class in that node
# 2nd - 4th column: absolute number of obs for each class in the node
# 5th - 7th column: relative number, empirical distribution in the node
# 8th column : nodeprob : 'Probability' to flow through / end in that node
# number of observations passing through this node (if it is a split node) or falling into the terminal node
treeFit2 <- rpart(Species~.,data=iris[sample(150,100),] ,method = 'class')
rpart.plot(treeFit2) 
print(treeFit2)

#### frame ####
################

treeFit$frame
dim(treeFit$frame) # 5 x 9
treeFit$frame[,9] 
treeFit$frame[,9] %>% class # class is "matrix", "array" - one column can be a matrix?
treeFit$frame[,9] %>% dim # 5 x 8
treeFit$frame[,10] # undefined column
treeFit$frame[,9][1,2] # works

# how can we build such an object? such an abomination??

A <- data.frame(Haus=c(1,2,3,4), Tier=c(10,20,30,40))
dim(A)
B <- matrix(1:12,nrow=4)
A$yval2 <- B
dim(A) ; A


#### ranger ####
################
library(ranger)
# load Cleve!
data.train <- Cleve[,1:11]
set.seed(1)
rg <- ranger(CAD~.,num.trees = 5, mtry=4, data=data.train , keep.inbag = T)

treeFit_Cleve <- rpart(CAD~.,data=data.train
                 ,method = 'class'
                 , control=rpart.control(maxsurrogate=0, maxcompete=0))
