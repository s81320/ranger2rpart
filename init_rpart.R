# transform a single tree in a ranger forest into an rpart object 
# -> so we can plot it :-)

files <- list.files(path='R')
for(file in files) source(file)

rprtObj <- list(frame=NULL, call=NULL, method='class',splits=NULL)
class(rprtObj) <-  'rpart'

tri <- 1  
rprtObj$frame <- generate_frame(rg,tri)

rprtObj$where <- predict(rg,data = data.train, type = 'terminalNodes')$predictions[,tri]

rprtObj$splits <- generate_splits(rg,tri)

rprtObj$call <- treeFit_Cleve$call # this is bad cheating!
# put something original here!

rpart.plot(rprtObj, box.palette='Blues') # problem : NAs in the fitted values. WHERE are there any fitted valuues?? I did not generate them!!
rpart.plot(mutilTree)
