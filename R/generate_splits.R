
generate_splits <- function(rg, tri){
  # inbag observations for sprecified tree tri
  rg$inbag.counts[[tri]] %>% (function(x) which(x==1)) -> inbag.obs.tri # for tree tri in forest rg$forest
  
  # generate _* functions use rg, inbag.obs.tri and tri from parent directory: this function
  # or should we pass it explicitly? closer to functional programming...
  d.split <- generate_d.split(rg,inbag.obs.tri,tri)
  nVars <- length(rg$forest$independent.variable.names) # REMOVE??!!
  sNodes <-  which(lengths(d.split)>0)
  nSplitNodes <- length(d.split[sNodes])
  splits <- data.frame(
    rg$forest$independent.variable.names[sNodes] # not working, check generate_var
    , count=lengths(d.split[sNodes])
    , ncat=rep(-1,nSplitNodes)
    , improve=runif(nSplitNodes,1,50) # have to be positive? what else? does it matter?
    , index=runif(nSplitNodes,0,100) # split values # dummies currently!
    , adj=rep(0,nSplitNodes)
  )
  return(splits)
}
