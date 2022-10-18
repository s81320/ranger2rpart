preorder <-  function(rg,tri,x){
  # if node position x is terminal, return x , else recurse
  if(rg$forest$child.nodeIDs[[tri]][[1]][[x]]==0){
    x
  }else{
    c(x
      , preorder(rg, tri, rg$forest$child.nodeIDs[[tri]][[1]][[x]] %>% ID2pos)
      , preorder(rg, tri, rg$forest$child.nodeIDs[[tri]][[2]][[x]] %>% ID2pos)
    )
    # note: in a full binary tree the recusion would be c(x,preorder(2*x), preorder(2*x+1))
    # but ranger trees don't leave holes when a subtree is missing
    # we have to get the left and right child from child.nodeIDs
  }
}