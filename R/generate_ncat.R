# for a continuous split variable ncat equals 1 or -1
# for a categorical variable it is the number of categories (with a positive or negative sign? I have only ever seen positive signs...)

generate_ncat <-  function(rg , tri, data.train){
  # for all columns of data.train
  ncat <-  NULL
  for(ivn in rg$forest$independent.variable.names){ # pass data.train or get() it from the training data given in rg$call$data ??
    if(is.factor(data.train[,ivn])){
      ncat[ivn] <- length(levels(data.train[,ivn]))
    }else{
      ncat[ivn] <- -1
    }
  }
  ncat
  }
  
# if the variable is continuous, ncat is -1 or 1
# -1 corresponds to < , 1 to >=
# it is chosen such that observations are split in such a way 
# that the class probability for the positive class is higher on the right.
# in ranger it is always -1 and <
# if I implement it as in rpart the plot will not look the same as my ranger plot
# but it is a nice feature to have terminal nodes a bit ordered from left to right
# with increasing probabilities for the positive class...
  