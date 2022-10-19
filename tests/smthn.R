load('data/CleveHungSwissVA.rda')

is.factor(Cleve$Chestpaintype)
is.ordered(Cleve$Chestpaintype)

data.train <- Cleve[,1:11]

# not necessary: 
#data.train$Chestpaintype <-  factor(data.train$Chestpaintype , ordered=T)
#data.train$RestingECG <-  factor(data.train$RestingECG , ordered=T)
# ranger treats factors as ordered

{tri <- 1
inbag.obs <-  function(rg, tri){
  rg$inbag.counts[[tri]] %>% (function(x) which(x==1))
}
inbag.obs.tri <- inbag.obs(rg, tri)
}

# tfc : tREE fIT cLEVE
set.seed(1)
tfc <- rpart(CAD~.
             # , data=data.train
             , data=data.train#[inbag.obs.tri,]
             , method = 'class'
             , control=rpart.control(maxsurrogate=0, maxcompete=0))

rpart.plot(tfc)
# plot(tfc) ; text(tfc)

tfc$frame

tfc$splits 

# manipulate splits
tfc$splits[1,'index'] <- 1
tfc$splits[1,'ncat'] <- 4
tfc$splits[6,'index'] <- 3
tfc$splits[6,'ncat'] <- 2

summary(tfc) # numbering : parent k -> children 2k and 2k+1
tfc # numbering : parent k -> children 2k and 2k+1

# totally not clear: When selecting levels b,c from a,b,c,d how is that coded in splits?
# probably in powers of 2! select b,c -> (0,1,1,0) -> 0*2^0 + 1*2^1 + 1*2^2 + 0*2^3= 6
# unselected a,d -> (1,0,0,1) -> 2^0 + 2^3 = 1 + 8 = 9
# select and unselect codes the same split: one set goes left, the other right
# characteristics are coded such that selected goes left, remains go right. Also: the set with higher probability goes right
# thus selection is fixed, the choice for selecting a,d or b,c is no longer open.
# strange: plot says Sex=Female in 2 places, index is not the same both times. Rather: 2 and 3
# similar: plot says Chestpaintype =Typical angina, Non-anginal pain twice.
# Encoded in splits differently, as 5 and 6

# it is probably easier to work only with ordered factors in ranger and in rpart.
