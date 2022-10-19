# uses explicit target variable name. Species , CAD , make it generic!!
# not open to different numbers of classes, like 2 (as in Cleve) or 3 (as in iris)

generate_yval2 <- function(rg, inbag.obs.tri, tri, d.split){
  
  # from d.split generate class probabilities at split and terminal nodes
  # class count at each node (split and terminal)
  
  View(d.split)
  
  Vectorize(function(i) i %>% d.split[[.]] %>% data.train[.,'CAD'] %>% table)(1:length(d.split)) -> yval2
  # Vectorize(function(i,d.split,data.train) i %>% d.split[[.]] %>% data.train[.,'CAD'] %>% table)(i=1:length(d.split)) -> yval2
  # the anonymous function uses d.split outside of Vectorize? As passed in argument of generate_yval2? Any doubts?
  
  'printing yval2' %>% print
  yval2 %>% print
  
  # add relative class count , i.e. class balance at each node
  yval2 <- rbind(yval2,apply(yval2,2, function(x) x/(x[1]+x[2])  )) %>% t # apply the user generated function is faster than repeating the above with table %> prop.table
  # add majority class at each node
  yval2 <- cbind(apply(yval2[,c(1,2)], 1 , which.max),yval2)
  # add column for node probability : 
  # number of observations passing through a split node or ending in a terminal node 
  # divided by number of observations
  yval2 <- cbind(yval2, apply(yval2[,2:3],1,sum) / length(d.split[[1]])) # number of observations : length(d.split[[1]]) or length(inbag.obs.tri)
  # name column node probability
  colnames(yval2)[length(colnames(yval2))]<-'nodeprob'
  return(yval2)
}
