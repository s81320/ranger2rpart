# transform a single tree in a ranger forest into an rpart object 
# -> so we can plot it :-)

path <-  'R'
files <- list.files(path, pattern = '*.R')
for(file in files) source(paste(path,file,sep='/'))

library(ranger)
library(dplyr)
library(rpart)
library(rpart.plot)

load('data/CleveHungSwissVA.rda')

data.train <- Cleve[,1:11]

set.seed(4)
rg <- ranger(CAD~.
             , num.trees = 1
             , mtry=4
             , data=data.train 
             , keep.inbag = T
             , max.depth = 4
             #, respect.unordered.factors = 'partition'
             , min.node.size=50
             )

predict(rg, data=data.train, type='terminalNodes')$predictions %>% table
#predict(rg, data=data.train )$predictions

tri <- 1

plotTree1(rg,tri)

rprtObj <- list(frame=NULL, call=NULL, method='class',splits=NULL)
class(rprtObj) <-  'rpart'

rprtObj$frame <- generate_frame(rg,tri)
rprtObj$frame

rprtObj$where <- predict(rg, data = data.train, type = 'terminalNodes')$predictions[,tri]

rprtObj$splits <- generate_splits(rg, tri)
rprtObj$splits

rprtObj$call <- treeFit_Cleve$call # this is bad cheating!
# put something original here!

predict(rprtObj) %>% length
predict(rg, data=Cleve)$predictions %>% as.integer %>% length

(predict(rprtObj) - predict(rg, data=Cleve)$predictions %>% as.integer) %>% table
which((predict(rprtObj) != predict(rg, data=Cleve)$predictions %>% as.integer)) %>% Cleve[.,]

# accuracy, should be the same
predict(rprtObj, newdata=Cleve, type='class') # not working. Why??
(predict(rprtObj) - Cleve$CAD %>% as.integer) %>% table
(predict(rg, data=Cleve)$predictions %>% as.integer - Cleve$CAD %>% as.integer)  %>% table
which((predict(rprtObj) != predict(rg, data=Cleve)$predictions %>% as.integer)) %>% Cleve[.,]

rprtObj$splits$index
View(rprtObj$splits)

plotTree1(rg,1,T)
rpart.plot(rprtObj, box.palette='Blues') # problem : NAs in the fitted values. WHERE are there any fitted values?? I did not generate them!!

