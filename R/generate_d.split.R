generate_d.split <-  function(rg, inbag.obs.tri, tri){
  #' it starts with all observations that are training data for tree tri
  #' checks the split variable and split value and splits the data in d.split[[1]]
  #' into d.split[[2]] and d.split[[3]]
  #' continues to the terminal nodes
  #' returns number of observations in split and terminal nodes, uses positions (starting at 1) not IDs (starting at 0)
  
  d.split <- list()
  d.split[[1]] <- inbag.obs.tri
  all.nodes <- unique(rg$forest$child.nodeIDs[[tri]] %>% unlist)
  #all.nodes <- base::setdiff(all.nodes,0)
  all.nodes <- all.nodes[order(all.nodes)]
  # instead of going through all nodes, why not start with going only through split nodes?
  # use sNodes from subforest.R (returns nodeIDs of split nodes)
  for(nodeID in all.nodes){
    #nodeID %>% paste('nodeID') %>% print
    #ti[ti$nodeID == nodeID , 'terminal'] %>% paste('terminal') %>% print
    
    nodeID %>% 
      ID2pos %>% 
      rg$forest$child.nodeIDs[[tri]][[1]][.] %>% 
      (function(x) x==0) -> terminal
    
    if(!terminal){
      
      nodeID %>%
        ID2pos %>%
        rg$forest$child.nodeIDs[[tri]][[1]][.] %>%
        ID2pos -> pos1
      
      #pos1 %>% paste('pos for left child, pos1')  %>% print
      
      nodeID %>%
        ID2pos %>%
        rg$forest$child.nodeIDs[[tri]][[2]][.] %>%
        ID2pos -> pos2
      
      #pos2 %>% paste('pos for right child, pos2')  %>% print
      
      d.split[[pos1]] <- route_in_tree(rg=rg
                                       , inbag.obs.tri=inbag.obs.tri
                                       , d.split=d.split
                                       , data=data.train
                                       , nodeID=nodeID
                                       , tri=tri)
      #d.split[[pos1]] %>% length %>% print
      d.split[[pos2]] <- base::setdiff(d.split[[ID2pos(nodeID)]], d.split[[pos1]])
      #d.split[[pos2]] %>% length %>% print
    }
  }
  return(d.split)
}
