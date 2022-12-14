# frame is a central data frame in the rpart object
# here we transform information from the ranger object to fill this (data) frame
# 11.10.2022

generate_frame <- function(rg, tri){
  # inbag observations for sprecified tree tri
  rg$inbag.counts[[tri]] %>% (function(x) which(x==1)) -> inbag.obs.tri # for tree tri in forest rg$forest
  
  d.split <- generate_d.split(rg,inbag.obs.tri,tri)
  
  num.nodes <- length(d.split)
  
  yval2 <-  generate_yval2(rg, inbag.obs.tri, tri, d.split)
  
  frame <- data.frame(
    var=generate_var(rg,tri) 
    , n=lengths(d.split)
    , wt=lengths(d.split)
    #, wt=n # working??
    , dev=rep(1,num.nodes)
    , yval=yval2[,1]
    , complexity=rep(1,num.nodes)
    , ncompete=rep(0,num.nodes)
    , nsurrogate=rep(0,num.nodes)
  )
  frame$yval2 <- yval2
  
  frame <-  frame[ranger_tree_in_preorder(rg,tri),]
  return(frame)
}


