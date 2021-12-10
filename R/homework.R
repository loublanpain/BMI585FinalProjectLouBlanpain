
#' The Box-Muller transformation
#'
#'Takes two samples from the uniform distribution on the interval \[0,1\] and maps them to two standard, normally distributed samples.
#'
#'Uses the following equations to generate the two standard, normally distributed samples:
#'
#'\eqn{X1=\sqrt{−2log(U_1)}cos(2\pi U_2)}
#'
#'\eqn{X2=\sqrt{−2log(U_1)}sin(2\pi U_2)}
#'
#'@export
#'
#'@param n The number of samples to be returned.
#'
#'@return A vector of samples from the standard normal distribution.
#'
#'@examples
#'boxMuller(500)
#'
boxMuller = function(n = 1){
  u1 = runif(n)
  u2 = runif(n)
  sqrt(-2*log(u1))*cos(2*pi*u2)
}

#'Conversion of test statistic to t-density
#'
#'Converts a test statistic x to the area under the t-density for values >=|x|.
#'
#'@export
#'
#'@param t The test statistic.
#'@param n The number of degrees of freedom.
#'
#'@return The area under the t-density for values >=|x|.
#'
#'@examples
#'twoSidedT(3,8)
#'
twoSidedT=function(t,n){
  t = abs(t)
  1-(pt(t,n)-pt(-t,n))
}

#'Conversion of test statistic to z-density
#'
#'Converts a test statistic z to the area under the z-density for values >=|x|.
#'
#'@export
#'
#'@param z The test statistic.
#'
#'@return The area under the z-density for values >=|x|.
#'
#'@examples
#'twoSidedZ(3)
#'
twoSidedZ=function(z){
  z = abs(z)
  1-(pnorm(z)-pnorm(-z))
}

#'Effect size calculation
#'
#'Calculates the effect size (Cohen's d) for a variable with two groups.
#'
#'@export
#'
#'@param x A numeric vector of a variable.
#'@param q A corresponding logical vector (0 or 1) indicating whether each of the elements of vector x belongs to group 0 or 1.
#'
#'@return The effect size of the varible between the two groups.
#'
#'@examples
#'x=c(runif(10,5,6),runif(10,7,8))
#'q=c(rep(0,10),rep(1,10))
#'effectSize(x,q)
#'
effectSize=function(x,q){
  abs(mean(x[q==0])-mean(x[q==1]))/sd(x)
}

#'Welch t-test
#'
#'Calculates t-statistic, degrees of freedom and associated p-value, for two samples. Uses the satterwaithe approximation for cases where the number of samples in the two vectors differ.
#'
#'@export
#'
#'@param x A numeric vector from first sample.
#'@param y A numeric vector from second sample.
#'
#'@return A list of named variables 't', 'df' and 'p', corresponding to t-statistic, degrees of freedom, and associated p-value, respectively.
#'
#'@examples
#'x=rnorm(30)+1
#'y=rnorm(35)+3
#'welchT(x,y)
#'
welchT=function(x,y){
  t=abs(mean(x)-mean(y))/sqrt((sd(x)^2/length(x))+sd(y)^2/length(y)) #using the Satterwaithe approximation
  df=((((sd(x)^2)/length(x))+((sd(y)^2)/length(y)))^2)/(((1/(length(x)-1))*((sd(x)^2)/length(x))^2)+((1/(length(y)-1))*((sd(y)^2)/length(y))^2))
  p=twoSidedT(t,df)
  return(list(t=t,df=df,p=p))
}

#'Map effect size to minimum sample size
#'
#'Given a power of 80%, calculates the minimum sample size required to uncover an assumed effect size or Cohen's d value.
#'
#'@export
#'
#'@param d The assumed effect size (or Cohen's d).
#'
#'@return The minimum sample size required to show the assumed Cohen's d, with power of 0.80.
#'
#'@examples
#'minimumN(0.6)
#'
minimumN=function(d){
  n=power.t.test(delta=d,power=0.8)
  ceiling(n$n)
}

#'Chi-squared test
#'
#'Takes in a tibble of counts and returns a list with the tibble, and result of the Chi-squared test.
#'
#'@export
#'
#'@param tib The tibble of counts.
#'
#'@return A list containing the table tib, and the result of the Chi-squared test (including X-squared, degrees of freedom df, and p-value of the test).
#'
#'@examples
#'chiSquareCounts(as_tibble(cbind(c(21,3),c(14,10))))
#'
chiSquareCounts=function(tib){
  temp=chisq.test(tib,correct=F)
  list(table=tib,result=temp)
}

#'Map Cohen's d and sample size to power
#'
#'This takes in a measured effect size or Cohen's d, and total sample size used, and returns the power of the analysis, in decimal value (i.e. 0.7 would be 70% power).
#'
#'@export
#'
#'@param d The effect size measured or Cohen's d
#'@param n1 Sample size from first group.
#'@param n2 Sample size from second group.
#'
#'@return A decimal number between 0 and 1, indicating the power of the analysis.
#'
#'@examples
#'postHocPower(0.6,20,25)
#'
postHocPower=function(d,n1,n2){
  pw_percent=double(1000)
  for (i in 1:1000){
    temp=t.test(rnorm(n1,0),rnorm(n2,d))
    pw_percent[i]=temp$p.value
  }
  sum(pw_percent<0.05)/length(pw_percent)
}

#'p-value thresholding with Bonferroni-Holm correction
#'
#'Adjusts a set of p-values using the Bonferroni-Holm correction, and returns a logical vector indicating whether each corresponding adjusted p-value is below 0.05 (alpha of 0.05).
#'
#'@export
#'
#'@param p A numeric vector of p-values from an analysis.
#'
#'@return A logical vector indicating whether or not each of the p-values satisfy an alpha of 0.05, following Bonferroni-Holm correction (0 is no, 1 is yes).
#'
#'@examples
#'p=seq(0.0025,0.0250,0.0025)
#'names(p)=paste('Test',LETTERS[seq(1,length(p))])
#'bhAdjust(p)
#'
bhAdjust = function(p){
  alpha=0.05
  # number of tests
  m = length(p)
  # pre-allocate an output vector
  out = logical(m)
  names(out) = names(p)
  # loop over sorted values
  sorted = sort(p)
  for(i in seq(m)){
    if(sorted[i]<(alpha/(m-i+1))){
      out[i] = TRUE
    }else
      break
  }
  # undo the sort - this bizarre construction restores the initial order.
  out[order(order(p))]
}

#'p-value threhsolding with false discovery rate correction
#'
#'Adjusts a set of p-values using the false discovery rate (FDR) correction, and returns a logical vector indicating whether each corresponding adjusted p-value is below 0.05 (alpha of 0.05).
#'
#'@export
#'
#'@param p A numeric vector of p-values from an analysis.
#'
#'@return A logical vector indicating whether or not each of the p-values satisfy an alpha of 0.05, following FDR correction (0 is no, 1 is yes).
#'
#'@examples
#'p=seq(0.0025,0.0250,0.0025)
#'names(p)=paste('Test',LETTERS[seq(1,length(p))])
#'fdrAdjust(p)
#'
fdrAdjust = function(p){
  alpha=0.05
  p_orig = p
  # For a given alpha, find the largest k such that p ≤ alpha * k / m
  p = sort(p)
  m = length(p)
  k = 0
  for(i in seq(m)){
    if(p[i]<=alpha*i/m) k = i
  }
  # Reject H1-Hk. To do this, return all false if k is 0,
  # otherwise compare the original vector to the p value corresponding
  # to the maximum value of k.
  if(k==0){logical(length(p))}
  else{p_orig<=p[k]}
}

#'R-squared calculation
#'
#'Calculates the R-squared for a given model, i.e. the measure of the model fit.
#'
#'@export
#'
#'@param pred A numeric vector of variable values predicted by the model.
#'@param truth A numeric vector of variable values measured.
#'
#'@return The R-squared value for that model.
#'
#'@examples
#'data=read.csv('https://jlucasmckay.bmi.emory.edu/global/bmi585/credit.csv')
#'lim_inccards=lm(Limit~Income+Cards,data)
#'R_squared_calc(data$Limit,lim_inccards$fitted.values)
#'
r2=function(pred,truth){
  RSS=sum((truth-pred)^2)
  TSS=sum((truth-mean(truth))^2)

  1-(RSS/TSS)
}

#'Adjusted R-squared calculation
#'
#'Calculates the adjusted R-squared for a given model, i.e. the measure of the model fit, adjusted for the number of parameters used.
#'
#'@export
#'
#'@param pred A numeric vector of variable values predicted by the model.
#'@param truth A numeric vector of variable values measured.
#'@param d The number of parameters used.
#'
#'@return
#'
#'@examples
#'data=read.csv('https://jlucasmckay.bmi.emory.edu/global/bmi585/credit.csv')
#'lim_inccards=lm(Limit~Income+Cards,data)
#'R_squared_calc(data$Limit,lim_inccards$fitted.values,2)
#'
adjR2=function(pred,truth,d){
  RSS=sum((truth-pred)^2)
  TSS=sum((truth-mean(truth))^2)

  1-((RSS/(length(truth)-d-1))/(TSS/(length(truth)-1)))
}
