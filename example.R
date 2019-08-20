## install.packages("Rcpp")
library(Rcpp)

## Set working directory, if you are using Rstudio, 
## this command can set the directory to the script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load functions
source("func.r")


## Read the data
## The data is a data.fram where each row is a sample for a patient.
## The column must contains ID and rupture variable, and the rest are covariates.
## Samples from the same patient occupy contiguous rows in the data 
## The first row for a patient must be a ruptured aneurysm. 
myData=read.csv("exampleData.csv",header = T)

## Exract ID, rupture and covariates
ID=myData$ID
rupture=myData$rupture
covariate=as.matrix(myData[,!colnames(myData)%in%c("ID","rupture")])


res=fitParm(ID,covariate)
res
# $parm
# Var1        Var2 
# 0.94642857 -0.05357143 
# 
# $loss
# [1] -3.762389e-05

## You can get the misclassification number by rounding the loss value up
## In this example the misclassification number is 0.




