# ERROR , problem: splits should not have NAs
# should have one row per split node

generate_splits <- function(rg, tri){
  # inbag observations for sprecified tree tri
  rg$inbag.counts[[tri]] %>% (function(x) which(x==1)) -> inbag.obs.tri # for tree tri in forest rg$forest
  
  d.split <- generate_d.split(rg,inbag.obs.tri,tri)
  #splitNodes <- sNodes(rg$forest, tri) %>% ID2pos
  
  # starting at 1 (not 0 as ranger nodeIDs)
  splitNodesPreorder <- base::setdiff(ranger_tree_in_preorder(rg, tri), tNodes(rg$forest,tri) %>% ID2pos)
  
  "print('positions of split nodes in full binary tree in level order from sNodes')
  print(splitNodes)
  print('split variables used')
  print(ID2pos(rg$forest$split.varIDs[[tri]][splitNodes]))
  print('names of split variables in preorder')"
  splitNodesPreorder %>%
    rg$forest$split.varIDs[[tri]][.] %>%
    ID2pos %>%
    rg$forest$independent.variable.names[.] -> split_node_var_names
  "print(split_node_var_names)
   
  print(lengths(d.split[splitNodes]))
  print(generate_ncat(rg, tri, data.train)[rg$forest$independent.variable.names[ID2pos(rg$forest$split.varIDs[[tri]][splitNodesPreorder])]] )
  print(rg$forest$split.values[[tri]][splitNodes])"
  
  nSplitNodes <- length(splitNodesPreorder)
  
  splits <- data.frame(
    split_node_var_names
    , count=lengths(d.split[splitNodesPreorder])
    , ncat=generate_ncat(rg, tri, data.train)[split_node_var_names] 
    , improve=splitNodesPreorder # have to be positive? what else? does it matter for plotting or predictions?
    , index=rg$forest$split.values[[tri]][splitNodesPreorder] # correct!
    , adj=rep(0,nSplitNodes)
  )
  
  
  # correcting the ordering for all columns at once
  #splits <-  splits[splitNodesPreorder,]
  
  return(splits)
}

