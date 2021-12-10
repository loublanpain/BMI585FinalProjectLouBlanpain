
#'Unscaling and uncentering an object
#'
#'Takes in an object that was returned by the scale function, and reverses the scaling and centering, outputting the original matrix.
#'
#'@export
#'
#'@param x A numerical object from scale function.
#'
#'@return A numerical matrix that is unscaled and uncentered.
#'
#'@examples
#'example_mat=matrix(rnorm(25),nrow=5)
#'example_mat_scaled=scale(example_mat)
#'example_mat_unscaled=unscale(example_mat_scaled)
#'
unscale=function(x){
  temp=t(t(x)*attributes(x)$`scaled:scale`+attributes(x)$`scaled:center`)
  attributes(temp)[c(2,3)]=NULL
  temp
}

#'Approximating data with principal components
#'
#'Takes in a tibble or data frame of observations (rows) of several numeric variables (columns), and return an approximation of the data using a given number of first principal components.
#'
#'@export
#'
#'@param x A tibble or data frame containing the data to be approximated.
#'@param npc The numbers of first principal components to use when approximating the data.
#'
#'@return Approximated data with the first npc principal components.
#'
#'@examples
#'data=read.csv('https://jlucasmckay.bmi.emory.edu/global/bmi585/heart.csv')
#'data=data[,c('Age','RestBP','Chol','MaxHR','Oldpeak')]
#'pcApprox(data,3)
#'
pcApprox=function(x,npc){
  dim_analysis=prcomp(x,center=T,scale.=T) #run principal component analysis, with centering and scaling
  temp=dim_analysis$x[,1:npc]%*%t(dim_analysis$rotation[,1:npc]) #convert back to centered and scaled data, approximated with the first npc principal components
  t(t(temp)*analysis$scale+analysis$center) #unscale
}

#'Lollipop plotting
#'
#'Creates a set of lollipop plots for up to the first 5 principal components of a dataset.
#'
#'(some code taken from https://www.r-graph-gallery.com/300-basic-lollipop-plot.html)
#'
#'@export
#'
#'@param x The data frame or tibble containing the dataset.
#'
#'@return A list of (up to 5) plots (from ggplot2) objects; each plot is a lollipop plot of the loadings of each of the (up to 5) principal components for each variable. Morover, running this function will also display the plots in a 1-column grid.
#'
#'@examples
#'data=read.csv('https://jlucasmckay.bmi.emory.edu/global/bmi585/heart.csv')
#'data=data[,c('Age','RestBP','Chol','MaxHR','Oldpeak')]
#'plots=pcLollipop(data)
#'
pcLollipop=function(x){
  num_pcs=dim(x)[2] #identify maximum number of principal components
  if (num_pcs>5){num_pcs=5} #if more than 5 pcs possible limit the number of PCs represented to 5

  dim_analysis=prcomp(x,center=T,scale.=T) #run principal component analysis on x, unsing centering and scaling

  plots=list()
  for (i in 1:num_pcs){
    plots[[i]]=ggplot(cbind(Variable=dimnames(dim_analysis$rotation)[[1]],as.data.frame(dim_analysis$rotation)),aes_string(x='Variable',y=paste0('PC',i),color='Variable'))+geom_point()+geom_segment( aes_string(x='Variable', xend='Variable', y=0, yend=paste0('PC',i)))+scale_x_discrete(labels=as.factor(dimnames(dim_analysis$rotation)[[1]]))+ylim(-1,1)+xlab('')+theme(legend.position = 'none')
    if (i==num_pcs){plots[[i]]=plots[[i]]+xlab('Variable')}
  }

  do.call(grid.arrange,c(plots,ncol=1))

  plots

}
