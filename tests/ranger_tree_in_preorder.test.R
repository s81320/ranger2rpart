
# testing ranger tree in preorder , which uses preorder
# visual test: compare to plotted tree
# meaningful test is for a ranger tree that is not a full binary tree

set.seed(1)
rg <- ranger(CAD~.
             , num.trees = 5
             , mtry=3
             , data=data.train 
             , keep.inbag = T
             , max.depth = 4
             , respect.unordered.factors = 'partition'
             #, respect.unordered.factors = 'ignore'
)

tri <- 5
ranger_tree_in_preorder(rg,tri)

plotTree1(rg,tri)
