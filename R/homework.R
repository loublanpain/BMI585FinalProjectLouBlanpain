
#' Title
#'
#'@export
#'
#'@param n
#'
#'@return
#'
#'@examples
#'
boxMuller=function(n){
  samples=numeric(n)
  for (i in seq(1,n,by=2)){
    U1=runif(1)
    U2=runif(1)
    samples[i]=sqrt(-2*log(U1))*cos(2*pi*U2)
    samples[i+1]=sqrt(-2*log(U1))*sin(2*pi*U2)
  }
  return(samples)
}

#' Title
#'
#'@export
#'
#'@param t
#'@param n
#'
#'@return
#'
#'@examples
#'
twoSidedT=function(t,n){
  pt(q=t,df=n,lower.tail=F)*2
}

#' Title
#'
#'@export
#'
#'@param z
#'
#'@return
#'
#'@examples
#'
twoSidedZ=function(z){
  pnorm(q=z,lower.tail=F)*2
}

#' Title
#'
#'@export
#'
#'@param x
#'@param q
#'
#'@return
#'
#'@examples
#'
effectSize=function(x,q){
  abs((mean(x[q==0])-mean(x[q==1]))/sd(x))
}

#' Title
#'
#'@export
#'
#'@param x
#'@param y
#'
#'@return
#'
#'@examples
#'
welchT=function(x,y){
  t=(mean(x)-mean(y))/sqrt((sd(x)^2/length(x))+sd(y)^2/length(y)) #using the Satterwaithe approximation
  df=((((sd(x)^2)/length(x))+((sd(y)^2)/length(y)))^2)/(((1/(length(x)-1))*((sd(x)^2)/length(x))^2)+((1/(length(y)-1))*((sd(y)^2)/length(y))^2))
  p=two_sided_t(t,df)
  return(list(t=t,df=df,p=p))
}

#' Title
#'
#'@export
#'
#'@param d
#'
#'@return
#'
#'@examples
#'
minimumN=function(d){
  n=power.t.test(delta=1,power=d)
  ceiling(n$n)
}

#' Title
#'
#'@export
#'
#'@param tib
#'
#'@return
#'
#'@examples
#'
#???
chiSquareCounts=function(tib){
  data_table=table(dataset[,y_var],dataset[,x_var])
  temp=chisq.test(data_table,correct=F)
  list(table=data_table,result=temp)
}

#' Title
#'
#'@export
#'
#'@param d
#'@param n1
#'@param n2
#'
#'@return
#'
#'@examples
#'
#???
postHocPower=function(d,n1,n2){
  pw_percent=double(1000)
  for (i in 1:1000){
    temp=t.test(rnorm(n,0),rnorm(n,d))
    pw_percent[i]=temp$p.value
  }
  sum(pw_percent<0.05)/length(pw_percent)
}

#' Title
#'
#'@export
#'
#'@param p
#'
#'@return
#'
#'@examples
#'
#???
bhAdjust=function(p){
  p_adjust=p.adjust(p,"bonferroni")
  p_adjust<alpha
}

#' Title
#'
#'@export
#'
#'@param p
#'
#'@return
#'
#'@examples
#'
#???
fdrAdjust=function(p){
  p_adjust=p.adjust(p,"fdr")
  p_adjust<alpha
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
#???
r2=function(pred,truth){
  RSS=sum((y-y_hat)^2)
  TSS=sum((y-mean(y))^2)

  R_squared=1-(RSS/TSS)
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
#???
adjR2=function(pred,truth){
  RSS=sum((y-y_hat)^2)
  TSS=sum((y-mean(y))^2)

  Adjusted_R_squared=1-((RSS/(length(y)-d-1))/(TSS/(length(y)-1)))
}
