
#'Estimating sensitivity
#'
#'Computes the sensitivity or recall of a classifier, by using measured data vs data predicted by a classifier.
#'
#'Equation used is # True Positives / # All Positives.
#'
#'@export
#'
#'@param pred A logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives provided by the classifier.
#'@param truth A logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives measured.
#'
#'@return A value from 0 (low sensitivity) to 1 (high sensitivity).
#'
#'@examples
#'pred_values=sample(c(0,1),replace=T,size=100)
#'true_values=sample(c(0,1),replace=T,size=100)
#'sensitivity(pred_values,true_values)
#'
sensitivity=function(pred,truth){
  #convert to 1s and 0s if pred and truth were provided as logicals
  if (class(pred)=='logical'){
    pred=as.numeric(pred)
  }
  if (class(truth)=='logical'){
    truth=as.numeric(truth)
  }

  #calculate sensitivity:
  result=sum(pred==1 & truth==1)/sum(truth)
  result
}

#'Estimating specificity
#'
#'Computes the specificity of a classifier, by using recorded data vs data predicted by a classifier.
#'
#'Equation used is # True Negatives / # All Negatives.
#'
#'@export
#'
#'@param pred A logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives provided by the classifier.
#'@param truth A logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives measured.
#'
#'@return A value from 0 (low specificity) to 1 (high specificity).
#'
#'@examples
#'pred_values=sample(c(0,1),replace=T,size=100)
#'true_values=sample(c(0,1),replace=T,size=100)
#'specificity(pred_values,true_values)
#'
specificity=function(pred,truth){
  #convert to 1s and 0s if pred and truth were provided as logicals
  if (class(pred)=='logical'){
    pred=as.numeric(pred)
  }
  if (class(truth)=='logical'){
    truth=as.numeric(truth)
  }

  #calculate specificity:
  result=sum(pred==0 & truth==0)/sum(truth==0)
  result
}

#'Estimating accuracy
#'
#'Computes the accuracy of a classifier, by using recorded data vs data predicted by a classifier.
#'
#'Equation used is (# True Positives + # True Negatives) / (# All Positives + # All Negatives).
#'
#'@export
#'
#'@param pred A logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives provided by the classifier.
#'@param truth A logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives measured.
#'
#'@return A value from 0 (low accuracy) to 1 (high accuracy).
#'
#'@examples
#'pred_values=sample(c(0,1),replace=T,size=100)
#'true_values=sample(c(0,1),replace=T,size=100)
#'accuracy(pred_values,true_values)
#'
accuracy=function(pred,truth){
  #convert to 1s and 0s if pred and truth were provided as logicals
  if (class(pred)=='logical'){
    pred=as.numeric(pred)
  }
  if (class(truth)=='logical'){
    truth=as.numeric(truth)
  }

  #calculate accuracy:
  result=(sum(pred==1 & truth==1)+sum(pred==0 & truth==0))/(length(pred))
  result
}

#'Estimating positive predictive value
#'
#'Computes the Positive Predictive Value (PPV) or precision of a classifier, by using recorded data vs data predicted by a classifier.
#'
#'Equation used is # True Positives / (# True Positives + # False Positives).
#'
#'@export
#'
#'@param pred A logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives provided by the classifier.
#'@param truth A logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives measured.
#'
#'@return A value from 0 (low PPV) to 1 (high PPV).
#'
#'@examples
#'pred_values=sample(c(0,1),replace=T,size=100)
#'true_values=sample(c(0,1),replace=T,size=100)
#'ppv(pred_values,true_values)
#'
ppv=function(pred,truth){
  #convert to 1s and 0s if pred and truth were provided as logicals
  if (class(pred)=='logical'){
    pred=as.numeric(pred)
  }
  if (class(truth)=='logical'){
    truth=as.numeric(truth)
  }

  #calculate prediction:
  result=sum(pred==1 & truth==1)/(sum(pred==1 & truth==1)+sum(pred==1 & truth==0))
  result
}

#'Estimating F1 score
#'
#'Computes the F1 score of a classifier, by using recorded data vs data predicted by a classifier.
#'
#'Equation used is 2 x (precision x recall) / (precision + recall). Precision is calculated using function PPV, while recall is calculated using     function Sensitivity.
#'
#'@export
#'
#'@param pred A logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives provided by the classifier.
#'@param truth A logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives measured.
#'
#'@return A value from 0 (low F1 score) to 1 (high F1 score).
#'
#'@examples
#'pred_values=sample(c(0,1),replace=T,size=100)
#'true_values=sample(c(0,1),replace=T,size=100)
#'f1(pred_values,true_values)
#'
f1=function(pred,truth){
  result=2*(PPV(pred,truth)*Sensitivity(pred,truth))/(PPV(pred,truth)+Sensitivity(pred,truth))
  result
}


