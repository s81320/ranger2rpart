route_in_tree <-  function(rg, inbag.obs.tri, d.split, data, nodeID, tri){
  #' do a split of the data at a split node, route rows of data left or right according to split variable and split value at nodeID
  #' 
  #' needs rg, d.split, inbag.obs.tri in parent environment, builds d.split
  #' based in the ranger package
  #' @param nodeID a split node in a ranger tree
  #' @param data observations at node nodeID
  #' 
  #' function name was changed from f6 to route in tree
  
  # get split variable name from ranger presentation of tree
  nodeID %>% 
    ID2pos %>% 
    rg$forest$split.varIDs[[tri]][.] %>% # returns a nodeID
    ID2pos %>% 
    rg$forest$independent.variable.names[.] -> svN
  
  # get split value from ranger presentation of tree
  nodeID %>%
    ID2pos %>%
    rg$forest$split.values[[tri]][.] -> sVal
  
  if(nodeID==0){
    idcs <- inbag.obs.tri # refers to rows of rg object training data
  }else{
    nodeID %>%
      ID2pos %>%
      d.split[[.]] -> idcs
  }
  
  #print(idcs)
  
  # indices (for observations) agree with the split rule
  # data row 's  value at the split variable is less than or equal to the split value
  idcs %>% 
    (function(x)
      # as.numeric is needed only for factors - but does not hurt when applying to already numeric features
      as.numeric(data[x,svN]) <= sVal
    ) %>% # mask for Cleveland rows of input (not original Cleveland rows)
    which %>% 
    idcs[.] %>% # transform back to original Cleveland rows
    return
}
