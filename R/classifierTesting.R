
#' Title
#'
#'@export
#'
#'@param pred
#'@param truth
#'
#'@return
#'
#'@examples
#'
sensitivity=function(pred,truth){
  #Description: computes the sensitivity or recall of a classifier, by using recorded data vs data predicted by a classifier.
  #
  #Usage: Sensitivity(pred,truth)
  #
  #Arguments:
  #pred: a logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives provided by the classifier.
  #truth: a logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives measured.
  #
  #Details: equation used is # True Positives / # All Positives

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

#' Title
#'
#'@export
#'
#'@param pred
#'@param truth
#'
#'@return
#'
#'@examples
#'
specificity=function(pred,truth){
  #Description: computes the specificity of a classifier, by using recorded data vs data predicted by a classifier.
  #
  #Usage: Specificity(pred,truth)
  #
  #Arguments:
  #pred: a logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives.
  #truth: a logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives measured.
  #
  #Details: equation used is # True Negatives / # All Negatives

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

#' Title
#'
#'@export
#'
#'@param pred
#'@param truth
#'
#'@return
#'
#'@examples
#'
accuracy=function(pred,truth){
  #Description: computes the accuracy of a classifier, by using recorded data vs data predicted by a classifier.
  #
  #Usage: Accuracy(pred,truth)
  #
  #Arguments:
  #pred: a logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives.
  #truth: a logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives measured.
  #
  #Details: equation used is (# True Positives + # True Negatives) / (# All Positives + # All Negatives)

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

#' Title
#'
#'@export
#'
#'@param pred
#'@param truth
#'
#'@return
#'
#'@examples
#'
ppv=function(pred,truth){
  #Description: computes the Positive Predictive Value (PPV) or precision of a classifier, by using recorded data vs data predicted by a classifier.
  #
  #Usage: PPV(pred,truth)
  #
  #Arguments:
  #pred: a logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives.
  #truth: a logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives measured.
  #
  #Details: equation used is # True Positives / (# True Positives + # False Positives)

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

#' Title
#'
#'@export
#'
#'@param pred
#'@param truth
#'
#'@return
#'
#'@examples
#'
f1=function(pred,truth){
  #Description: computes the F1 score of a classifier, by using recorded data vs data predicted by a classifier.
  #
  #Usage:F1(pred,truth)
  #
  #Arguments:
  #pred: a logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives.
  #truth: a logical (T=positive, F=negative) or numeric (1=positive, 0=negative) vector of positives and negatives measured.
  #
  #Details: equation used is 2 x (precision x recall) / (precision + recall). Precision is calculated using function PPV, while recall is calculated using     function Sensitivity.

  result=2*(PPV(pred,truth)*Sensitivity(pred,truth))/(PPV(pred,truth)+Sensitivity(pred,truth))
  result
}


