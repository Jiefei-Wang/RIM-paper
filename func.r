## Compute the candidate values of parameters for grid search
computeGrid<-function(Nparm, NSearch){
  if(Nparm==1)return(matrix(c(-1,1),1,2))
  mySeq= seq(from=-1,to=1,length.out = NSearch)
  mySeqList=rep(list(mySeq),Nparm)
  grid=t(as.matrix(expand.grid(mySeqList)))
  grid=apply(grid,2,function(x) x/sum(abs(x)))
  grid[,!is.na(colSums(grid))]
}

## Load C++ function that compute the loss value for each patient
Rcpp::sourceCpp(file="RcppCode.cpp")

## Fit the parameters using grid search
fitParm<-function(ID,covariate,Totalsearch=100000){
  #browser()
  uniqueID=unique(ID)
  Nparm=ncol(covariate)
  NSearch=round(Totalsearch^(1/ncol(covariate)))
  parmsMatrix=computeGrid(Nparm,NSearch)
  score=covariate%*%parmsMatrix
  lossValue=ComputeLoss(length(uniqueID),ID, score)
  BestParmID=which.min(colSums(lossValue))
  parm=parmsMatrix[,BestParmID]
  minLoss=colSums(lossValue)[BestParmID]
  list(parm=parm,loss=minLoss)
}
