#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix ComputeLoss(int patientNum, NumericVector ID, NumericMatrix score) {
  NumericMatrix lossValue(patientNum,score.ncol());
  NumericVector LargestUnrupturedScore(score.ncol());
  double curPatientID=ID[0];
  int curPatientNum=0;
  int rupturedRow=0;
  
  //reset the largest unruptrued score
  for(int j=0;j<score.ncol();j++){
    LargestUnrupturedScore[j]=score(1,j);
  }
  for(int i=1;i<ID.length();i++){
    if(curPatientID==ID[i]){
      for(int j=0;j<score.ncol();j++){
        if(score(rupturedRow,j)<score(i,j)){
          lossValue(curPatientNum,j) = 1;
        }else{
          //If the current unruptured score is larger
          //Than the record, update it.
          if(LargestUnrupturedScore[j]<score(i,j)){
            LargestUnrupturedScore[j]=score(i,j);
          }
        }
      }
    }else{
      //Compute loss value for the current patient
      for(int j=0;j<score.ncol();j++){
        if(lossValue(patientNum,j)==0){
          lossValue(patientNum,j)=score(patientNum,j)-LargestUnrupturedScore[j];
          if(lossValue(patientNum,j)>10) lossValue(patientNum,j)=10;
          lossValue(patientNum,j)=-lossValue(patientNum,j)/10000;
        }
      }
      //reset the largest unruptrued score
      for(int j=0;j<score.ncol();j++){
        LargestUnrupturedScore[j]=score(i+1,j);
      }
      curPatientNum++;
      rupturedRow=i;
      curPatientID=ID[i];
    }
  }
  return lossValue;
}