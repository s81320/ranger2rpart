data.train <- Cleve[,1:11]

# how are factors handeled in ranger?
# does it matter if they are ordered?

ordered <- T # factor ordered or not
if(ordered){
  data.train$Chestpaintype <- factor(Cleve$Chestpaintype
                                     , ordered=F)
}else{
  data.train$Chestpaintype <- factor(Cleve$Chestpaintype
                                     , ordered=T
                                     #  , levels(Cleve$Chestpaintype)[c(1,4,2,3)])
                                     , levels(Cleve$Chestpaintype)[c(2,4,1,3)])
}

set.seed(1)
rg <- ranger(CAD~.
             , num.trees = 1
             , mtry=3
             , data=data.train 
             , keep.inbag = T
             , max.depth = 20
             , respect.unordered.factors = 'partition'
             #, respect.unordered.factors = 'ignore'
)

if(ordered){
  pred.tn.ordered <- predict(rg, data=data.train, type='terminalNodes')$predictions
  pred.class.ordered <- predict(rg, data=data.train )$predictions
}else{
  pred.tn.not <- predict(rg, data=data.train, type='terminalNodes')$predictions
  pred.class.not <- predict(rg, data=data.train )$predictions
}

plotTree1(rg, tri=1)

(as.integer(pred.class.ordered) - as.integer(pred.class.not)) %>% table

# in ranger it makes a difference, ordering factor Chestpaintype or not
# with option respect.unordered.factors=T