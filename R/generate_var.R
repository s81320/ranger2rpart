generate_var <- function(rg,tri){
  termNodes <- tNodes(rg$forest,tri)
  'terminal node IDs' %>% print
  print(termNodes)
  Vectorize(function(i){
    if(i %in% termNodes){
      return('<leaf>')
    }else{
      i %>%
        ID2pos %>%
        rg$forest$split.varIDs[[tri]][.] %>%
        ID2pos %>%
        rg$forest$independent.variable.names[.] 
    }
    })(0:(length(rg$forest$split.varIDs[[tri]])-1)) %>% unlist
}


