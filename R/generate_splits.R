# ERROR , problem: splits should not have NAs
# should have one row per split node

generate_splits <- function(rg, tri){
  # inbag observations for sprecified tree tri
  rg$inbag.counts[[tri]] %>% (function(x) which(x==1)) -> inbag.obs.tri # for tree tri in forest rg$forest
  
  d.split <- generate_d.split(rg,inbag.obs.tri,tri)
  splitNodes <- sNodes(rg$forest, tri) %>% ID2pos
  print('positions of split nodes from sNodes')
  print(splitNodes)
  print(ID2pos(rg$forest$split.varIDs[[tri]][splitNodes]))
  print(rg$forest$independent.variable.names[ID2pos(rg$forest$split.varIDs[[tri]][splitNodes])])
  print(lengths(d.split[splitNodes]))
  print(generate_ncat(rg, tri, data.train)[rg$forest$independent.variable.names[ID2pos(rg$forest$split.varIDs[[tri]][splitNodes])]] )
  print(rg$forest$split.values[[tri]][splitNodes])
  
  nSplitNodes <- length(splitNodes)
  
  # in ranger nodeIDs and varIDs are 0-based
  # we need positions
  split_node_var_names <- rg$forest$independent.variable.names[ID2pos(rg$forest$split.varIDs[[tri]][splitNodes])] 
  splits <- data.frame(
    split_node_var_names
    , count=lengths(d.split[splitNodes])
    , ncat=generate_ncat(rg, tri, data.train)[split_node_var_names] 
    , improve=runif(nSplitNodes,1,50) # have to be positive? what else? does it matter for plotting or predictions?
    , index=rg$forest$split.values[[tri]][splitNodes]
    , adj=rep(0,nSplitNodes)
  )
  
  # correcting the ordering for all columns at once
  splits <-  splits[ranger_tree_in_preorder(rg, tri),]
  
  return(splits)
}

