generate_var <- function(rg){
  Vectorize(function(i){
    rg$forest$independent.variable.names[ID2pos(rg$forest$split.varIDs[[1]][i])] %>%
      (function(x) if(length(x)==0){
        '<leaf>'
      }else{
        x
      })
  })(0:(length(rg$forest$split.varIDs[[1]])-1)) %>% unlist
}
