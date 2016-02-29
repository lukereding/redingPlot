#cat("Plotting functions:\nstrip -- show the average as a point with option SE / CI, shows the data\nbar -- barplot, still needs some work\nscatter -- scatterplot\nmod -- modified box plot\nhistogram -- self-explanatory\nsimple -- draws a line for the mean of each group and plots the data points\nbeeStrip -- like simple, but plotted using the beeswarm package so no alpha is needed\naddAlpha -- add transparancy to any color. pass the color and alpha value in [0,1]\n\nStats functions:\nmonte_unpaired -- monte carlo permutation test, two unpaired samples\nmonte_paired -- monte carlo permutation test, two paired samples\n")
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"wesanderson" %in% installed.packages()) install.packages("wesanderson")
if (!"magrittr" %in% installed.packages()) install.packages("magrittr")
if (!"beeswarm" %in% installed.packages()) install.packages("beeswarm")
if (!"viridis" %in% installed.packages()) install.packages("viridis")
library(beeswarm)
library(dplyr)
library(wesanderson)
library(magrittr)
library(viridis)

# some attractive colors:
ruby <-rgb(202/255,53/255,7/255,1)
slate <- rgb(137/255,157/255,164/255,1)
mint <- rgb(73/255,191/255,150/255,1)
golden <- rgb(218/255,165/255,32/255,1)
orange <- rgb(227/255,121/255,46/255,1)
sky <- rgb(95/255,188/255,212/255,1)

cols<-c(ruby,mint,golden,slate,orange,sky)

# some nice colors maps:
# viridis(10)
# colorRampPalette(brewer.pal(9, "GnBu"))



# things to look into:
# alluvial diagrams: http://www.r-bloggers.com/alluvial-diagrams/
# parcoord in MASS
# stars()


#' line for mean + data
#'
#' plots each vector in your list as a group with the data jittered; draws a line for the mean
#'
#' @param data input your data as a list where length(list) = number of groups
#' @param lab labels for groups
#' @param point_size size of points
#' @param line_color color of line for mean
#' @param line_width width of line
#' @param jitter defaults to T
#' @param point_col defeaults to viridis colors
#' @param ylim y limits
#' @param median plot median? defaults to false
#' @param rug plot 1-D rug plot?
#' @param sample_size defaults to true
#' @param IQR include line for IQR? defaults to false
#'
#' @return none
#'
#' @examples 
#' x <- list(rnorm(40,40,5),rnorm(20,35,2),rnorm(25,41,2)) ; simple(x,main="simple() defaults") # using the defaults
#' simple(x,jitter=F) # without jitter doesn't look as good
#' simple(x,line_col="black",point_col=c(ruby,mint,slate),ylab="measurement",xlab="group",lab=c("A","B","C"),rug=T)
#' x<-list(rnorm(50,50,5),rnorm(30,40,6),rnorm(10,60,2),rnorm(60,50,10),rnorm(30,39,4))
#' simple(x,point_col=viridis(5),line_color="black",median=T,main="simple() dressed up")
#' simple(list(rnorm(24,10,2),rchisq(20,5),rexp(40,1/5),runif(40,5,15)),lab=c("normal","chi-squared","exponetial","uniform"),point_col=c(ruby,mint,slate,"goldenrod"),line_color="black",median=T)
#' simple(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="simple()",xlab="species")
#' simple(iris$Sepal.Length,iris$Species) # the simpler way
#'
#' @export

simple<-function(data,grouping,lab=NA,point_size=1.2,line_color="red",line_width=3.0,jitter=T,point_col=NA,median=FALSE,rug=TRUE,sample_size=T,...){
  
  # if response is missing, assume data is a list
  if(missing(grouping)){
    if(is.list(data)==FALSE){
      stop("enter your data either as a list or as a response variable and factor within a dataframe")
    }
  }
  
  if(is.list(data)){
    print("list")
    # if its a list, turn it into a dataframe
    # this is a stupid way to make this data frame, but whatever:
    if(missing(lab)){
      lab <- letters[1:length(data)]
    }
    
    labels <- c()
    sizes <- sapply(data,length)
    for(i in 1:length(data)){
      labels <- c(labels,rep(lab[i], sizes[i]))
    }
    df <- data.frame(matrix(unlist(data), nrow=length(unlist(data)), byrow=T),labels)
    data <- df[,1]
    grouping <- df[,2] %>% factor
  }
  else{
    print("not a list")
    df <- data.frame(data,grouping)
    print(df)
    if(missing(lab)){
      lab <- levels(grouping)
    }
  }
  
  #######################
  
  number_groups<-nlevels(grouping)
  
  if(missing(point_col)){
    point_col=viridis(number_groups+2)[1:number_groups]
  }
  
  # to get nice x values for plotting, we'll use those generate by barplot()
  x_values<-barplot(rep(1,number_groups),plot=F) %>% as.vector

  # create the plot
  plot(c(x_values[1]*0.4,x_values[length(x_values)]*1.2), xlim=c(x_values[1]*0.4,x_values[length(x_values)]*1.2),ylim=c(min(data,na.rm=T)*0.95, max(data,na.rm=T)*1.05),type="n",xaxt="n",yaxt="n",bty="l",cex.axis=1.2,cex.lab=1.3,...)

  # create y axis with the numbers the correct direction
  axis(2,las=2)

  # find range of x:
  ifelse(length(data)==1,xRange <- 0.5,xRange <- range(x_values,na.rm=T)[2]-range(x_values,na.rm=T)[1])

  # plot the mean as a line
  for(i in 1:number_groups){
    lines(x=c(x_values[i]-0.2,x_values[i]+0.2),y=c(rep(mean(df[df[,2] %in% lab[i],1]),2)),col=line_color,lwd=line_width)
  }

  # create x axis
  axis(side=1,at=x_values,labels=lab)

  # make point_col length equal to number of groups
  if(length(point_col==1)){
    point_col = rep(point_col,number_groups)
  }

  # draw data points
  if(jitter==T){
    for(i in 1:number_groups){
      points(x=rep(x_values[i], nrow(df[df[,2] %in% lab[i],])) %>% jitter(amount = 0.05), y=df[df[,2] %in% lab[i],1], pch=16, col=point_col[i], cex=point_size)
    }
  }
  else{
    for(i in 1:number_groups){
      points(x=rep(x_values[i],length(df[df[,2] %in% lab[i],1])), y=df[df[,2] %in% lab[i],1], pch=16,col=point_col[i],cex=point_size)
    }
  }

  # for rug plotting:
  if(rug==T){
    for(i in 1:number_groups){
      rug(df[df[,2] %in% lab[i],1],side=4,col=addAlpha(point_col[i],0.4),lwd=4)
    }
  }

  # for plotting the median as a diamond:
  if(median==T){
    for(i in 1:number_groups){
      points(x=x_values[i],y=median(df[df[,2] %in% lab[i],1]), pch=5, col=line_color, cex=1.5, lwd=3)
    }
  }

  # for plotting sample size below each group
  if(sample_size==TRUE){
    for(i in 1:number_groups){
      text(x_values[i],par("usr")[3]*1.03,paste("n = ", length(df[df[,2] %in% lab[i],1]),sep=""),col="grey20",pos=3)
    }
  }
}

# similar to simple() above, but plot using beeswarm

# examples
## beeStrip(list(rnorm(50,50,5),rnorm(30,40,6),rnorm(10,60,2),rnorm(60,50,10),rnorm(30,39,4)),point_col=wes_palette(5, name = "Zissou"),line_color="black",median=T)
# beeStrip(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),point_col=viridis(3),line_color="black",IQR=T,lab=c("setosa","versicolor","virginica"),xlab="species",ylab="sepal length")
# beeStrip(list(rnorm(50,50,5),rnorm(30,40,6),rnorm(10,60,2),rnorm(60,50,10),rnorm(30,39,4)),point_col=wes_palette(5, name = "Zissou"),line_color="grey30",IQR=T)
# beeStrip(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="beeStrip()",xlab="species")


#' beeStrip
#'
#' plots each vector in your list as a group with the data forming a histogram; draws a line at the mean for eaach group
#'
#' @param data list of groups you want to plot where length(data) = number of groups
#' @param lab labels for each group
#' @param point_size size of the individual data points. default is 1.4
#' @param beeMethod see beeswarm(). default is "center"
#' @param line_color color of line drawn at the mean for each group. default is "red"
#' @param line_width width of the line drawn at the mean. default is 3.0
#' @param jitter logical. should the data be jittered? defaults to TRUE
#' @param point_col vector of colors. defaults to viridis colors
#' @param y_limits you know this one
#' @param median logical. should a diamond for the median be plotted?
#' @param rug logical. should a rug plot be plotted on the right side of the plot?
#' @param sample_size logical. should the sample size be plotted beneath each group?
#' @param IQR logical. Should a line representing the IQR for each group be plotted?
#' @param ... other arguments to par()
#'
#' @return None
#'
#' @examples
#' beeStrip(list(rnorm(50,50,5),rnorm(30,40,6),rnorm(10,60,2),rnorm(60,50,10),rnorm(30,39,4)),point_col=wes_palette(5, name = "Zissou"),line_color="black",median=T)
#' beeStrip(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="beeStrip()",xlab="species")
#'
#' @export

beeStrip<-function(data,lab=rep(c(),length(data)),point_size=1.4,beeMethod="center",line_color="red",line_width=3.0,jitter=T,point_col=viridis(length(data)+2)[1:length(data)],y_limits=c(min(unlist(data),na.rm=T),max(unlist(data),na.rm=T)),median=FALSE,rug=TRUE,sample_size=T,IQR=F,side=-1,...){

  # make sure the data is a list; in the future I need to change the code to accomodate other types of data input (e.g. dataframes with y~x)
  if(is.list(data)!=TRUE){
    stop("input your data as a list")
  }

  number_groups<-length(data)

  # create the plot
  beeswarm(data,method=beeMethod,priority="density",pch=16,col=point_col,cex=point_size,side = side,bty='l',yaxt="n",labels=lab,...)
  # create y axis with the numbers the correct direction
  axis(2,las=2)

  x_values = 1:number_groups

  # find range of x:
  xRange <- length(data)

  # plot the mean as a line
  for(i in 1:length(data)){
    scale = 0.05
    lines(x=c(x_values[i]-(scale*xRange),x_values[i]+(scale*xRange)),y=c(rep(mean(data[[i]],na.rm=T),2)),col=line_color,lwd=line_width)
  }

  # to plot the IQR:
  if(IQR==TRUE){
    p<-boxplot(data,plot=F)
    for(i in 1:length(data)){
      lines(x=c(x_values[i],x_values[i]),y=c(p$stats[2,i],p$stats[4,i]),col=line_color,lwd=line_width)
    }
  }

  # create x axis
  axis(side=1,at=x_values,labels=lab)

  # make point_col length equal to number of groups
  if(length(point_col==1)){
    point_col = rep(point_col,number_groups)
  }

  # for rug plotting:
  if(rug==T){
    for(i in 1:length(data)){
      rug(data[[i]],side=4,col=addAlpha(point_col[i],0.4),lwd=4)
    }
  }

  # for plotting the median as a diamond:
  if(median==T){
    points(x=x_values,y=unlist(lapply(data,median)),pch=5,col=line_color,cex=1.5,lwd=3)
  }

  # for plotting sample size below each group
  if(sample_size==TRUE){
    for(i in 1:length(data)){
      text(x_values[i],y_limits[1]*0.99,paste("n = ",length(data[[i]]),sep=""),col="grey20")
    }
  }
}



#' plot categorical x quantitative data jittered
#'
#' plots each vector in your list as a group with the data jittered, draws a point at the mean. very similar to simple()
#'
#' @param data list of vectors you want to plot
#' @param lab labels for each group
#' @param type what kind of uncertainty do you want to show around the mean? options are "se" for standard error, "sd" for standard deviation, or "ci" for a 95 percent confidence interval
#' @param jitter logical. should the data be jittered?
#' @param points type of plotting character. defaults to solid point, i.e., 16
#' @param xlab label for x axis
#' @param ymin minimum y value.
#' @param ymax maximum y value
#' @param point_size size of the plotting characters. defaults to 1.2
#' @param mean_col color of the point for the mean. defaults to "red"
#' @param cols vector of colors for the groups. defaults to viridis colors
#' @param ... other arguments to pass to be par()
#'
#' @return None
#'
#' @examples
#' strip(list(rnorm(50,5),rnorm(20,10)))
#' strip(list(rpois(50,5),rpois(20,3),rnorm(100,7)),type="ci",lab=c("a","b","c"),xlab="group",ylab="response")
#' strip(list(rpois(50,5),rpois(20,3),rnorm(100,7),rnorm(45,2),rnorm(90,9)),type="ci",lab=c("a","b","c","d","e"),xlab="group",ylab="value",mean_col="black",col=viridis(7)[1:5] %>% addAlpha(0.5))
#' strip(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="strip()",xlab="species",mean_col="black",point_size=1.4,type="ci")
#'
#' @export

strip<-function(data,lab=rep(c(),length(data)),type="se",jitter=T,points=16,xlab="",ymin="determine",ymax="determine",point_size=1.2,mean_col = "red",cols = viridis(length(data)+2)[1:length(data)],...){
  par(bty="l")

  # error checking:
  # if there are NAs in your dataset, throw an error
  if(length(Filter(is.na,data))>0){
    stop("there are NAs in your list")
  }

  number_groups<-length(data)
  factor_names<-lab


  if(ymin=="determine"&ymax=="determine"){
    ymin <- min(unlist(data))
    ymax <- max(unlist(data))
  }

  maximum_value=ymax*1.05
  minimum_value=ymin*0.95

  # use lapply to extract the means
  means<-lapply(data,mean)

  # use barplot in base R to get good x axis values
  x_values <- barplot(unlist(means), plot=F) %>% as.vector


  plot(c(0,x_values[number_groups]+0.2),c(minimum_value,maximum_value),type="n",xaxt="n",cex.axis=1.2,cex.lab=1.3,yaxt="n",xlab=xlab,...)
  # create y axis with the numbers the correct direction
  axis(2,las=2)

  offset<- 0.15

  if(type=="se"){
    std_dev<-lapply(data,sd)
    for(i in 1:length(data)){
      std_error <- std_dev[[i]] / sqrt(length(data[[i]]))
      arrows(x_values[i]+offset,means[[i]]-std_error,x_values[i]+offset,means[[i]]+std_error,angle=90,code=3,length=0,lwd=2)
    }
  }

  if(type=="sd"){
    std_dev<-lapply(data,sd)
    for(i in 1:length(data)){
      arrows(x_values[i]+offset,means[[i]]-std_dev[[i]],x_values[i]+offset,means[[i]]+std_dev[[i]],angle=90,code=3,length=0,lwd=2)
    }
  }

  if(type=="ci"){
    std_dev<-lapply(data,sd)
    for(i in 1:length(data)){
      std_error <- std_dev[[i]] / sqrt(length(data[[i]]))
      arrows(x_values[i]+offset,t.test(data[[i]])$conf[1],x_values[i]+offset,t.test(data[[i]])$conf[2],angle=90,code=3,length=0,lwd=2)
    }
  }

  # plot the means
  for(i in 1:length(data)){
    points(x=x_values[i]+offset,y=means[[i]],cex=point_size*1.1,pch=16,col=mean_col)
  }


  axis(side=1,at=x_values,labels=lab)


  if(jitter==F){
    # now to draw the points
    for(i in 1:number_groups){
      points(x=rep(x_values[i]-offset,length(data[[i]])),y=data[[i]],pch=points,col=cols[i])
    }
  }

  else if(jitter==T){
    for(i in 1:number_groups){
      points(x=rep(x_values[i]-offset,length(data[[i]])) %>% jitter(amount=0.1),y=data[[i]],pch=points,col=cols[i],cex=point_size)
    }
  }
  par(bty="o",lwd=1)
}



# for making credible intervals
# used in strip() above
credible_intervals<-function(x,n=100000){
  means<-vector(length=n)
  for(i in 1:n){
    means[i]<-mean(sample(x,size=length(x),replace=T))
  }
  #print("first result is 5th percentile, second is 95th")
  results<-quantile(means,c(0.05,0.95))
  print(results)
  #hist(means)
  return(results)
}

##############################################

#' plot categorical x quantitative data as bar plot
#'
#' plots each vector in your list as a group as a bar plot with the data on top, jittered
#'
#' @param sim list of vectors you want to plot
#' @param lab labels for each group
#' @param CI logical. should a 95 percent confidence interval be drawn for each group? defaults to F
#' @param SE logical. should the dtandard errors for each group be drawn? deafults to F
#' @param bar_color color of the bars. can be a vector where length(bar_color) = number of groups
#' @param jitter logical. show the data jittered? defaults to TRUE
#' @param point_col color of the jittered data points
#' @param y_limits limits of the y axis
#' @param median logical. draw median for each group
#' @param point_size size of data points. defaults to 1.0. increase to make the data points bigger.
#' @param sample_size logical. plot sample size under each bar?
#' @param ... other arguments to be passed to par()
#'
#'
#' @return matrix showing x axis positions of the bars
#'
#' @examples
#' bar(list(rnorm(20,5),rnorm(15,5)),lab=c("A","B"),xlab="group",ylab="response",jitter=F)
#' bar(list(rnorm(20,5),rnorm(15,5)),CI=T,lab=c("A","B"),xlab="group",ylab="response",jitter=F,bar_color=viridis(3)[1:2])
#' bar(list(rnorm(20,5),rnorm(15,5),rnorm(200,50),rnorm(50,1),rnorm(34,19)),CI=T,bar_color=viridis(7)[1:5])
#' bar(list(rnorm(20,5),rnorm(15,5)),CI=T,lab=c("A","B"),xlab="group",ylab="response",jitter=T,bar_color=viridis(10)[c(3,7)],point_size=1.5)
#'
#' @export

bar<-function(sim,lab=rep(c(),length(sim)),CI=F,SE=F,bar_color="grey80",jitter=T,point_col="#00000080",y_limits=NA,median=FALSE,point_size=1.0,sample_size=TRUE,...){
  par(lwd = 1,family = 'Helvetica')

  if(is.list(sim)==F){
    stop("the first argument needs to be a list of the vectors you want to plot")
  }

  means<-vector(length=length(sim))
  for(i in 1:length(sim)){
    means[i]<-mean(sim[[i]])
  }

  max_value = 0
  for(i in 1:length(sim)){
    if(max(sim[[i]],na.rm=TRUE) >= max_value){
      max_value <- max(sim[[i]])
    }
  }

  min_value = 0
  for(i in 1:length(sim)){
    if(min(sim[[i]],na.rm=TRUE) <= min_value){
      min_value <- min(sim[[i]])
    }
  }

  if(is.na(y_limits)==T){
    (p<-barplot(means,bty="l",ylim=c(floor(min_value),ceiling(max_value)+(0.1*max_value)),axes=F,col=bar_color,border=NA,...))
  }
  else{
    (p<-barplot(means,bty="l",ylim=y_limits,axes=F,col=bar_color,border=NA,cex.axis=1.2,cex.lab=1.3,...))
  }
  #(p<-barplot(means,bty="l",space=0.4,ylim=ifelse(y_limits==NA,c(floor(min_value),ceiling(max_value)+(0.1*max_value)),y_limits),axes=F,col=bar_color,border=NA,...))
  axis(1,at=p,lwd=1,cex=1.5,labels=lab,tick=F)
  axis(2,cex=1.5,lwd=1,las=2)

  if(sample_size==TRUE){
    text(x=p,y=0, pos=3, labels=paste("n = ",lapply(sim, length) ) )
  }

  if(SE==T & CI ==T){
    stop("can't plot both a conf. interval and a standard error. choose only one.")
  }

  # for 95% CIs:
  # why doesn't this work?
  if(CI==T){
    for(i in 1:length(sim)){
      # find the stardard error
      arrows(x0=p[i],y0=t.test(sim[[i]])$conf[1],x1=p[i],y1=t.test(sim[[i]])$conf[2],col="grey50",angle=90,code=3,lwd=2,length=0)
      #lines(rep(p[i],2),y=c(mean(sim[[i]]),mean(sim[[i]])-(se*1.96)),lwd=5,col="white")
      #lines(rep(p[i],2),y=c(mean(sim[[i]]),mean(sim[[i]])+(se*1.96)),lwd=5)
    }
  }

  if(SE==T){
    for(i in 1:length(sim)){
      # find the stardard error
      se<-sd(sim[[i]])/sqrt(length(sim[[i]]))
      arrows(x0=p[i],y0=mean(sim[[i]])-se,x1=p[i],y1=mean(sim[[i]])+se,col="grey50",angle=90,code=3,lwd=2,length=0)
      #lines(rep(p[i],2),y=c(mean(sim[[i]]),mean(sim[[i]])-se),lwd=5,col="white")
      #lines(rep(p[i],2),y=c(mean(sim[[i]]),mean(sim[[i]])+se),lwd=5)
    }
  }

  if(jitter==T){
    x_vals = p + 0.2
    for(i in 1:length(sim)){
      points(x=rep(x_vals[i],length(sim[[i]]))+rnorm(length(sim[[i]]),0,0.05),y=sim[[i]],pch=16,col=point_col,cex=point_size)
    }
  }

  # plot median plots
  if(median==T){
    medians<-vector(length=length(sim))
    for(i in 1:length(sim)){
      medians[i]<-median(sim[[i]],na.rm=T)
    }
    points(p,medians,pch=5)
  }

  return(p)
}

########################################################
#scatter


# helper function that will draw line of best fit, but not extend beyond the reaches of the data
line<-function(x,y,color="black",...){
  slope <- summary(lm(y~x))$coefficients[2,1]
  intercept <- summary(lm(y~x))$coefficients[1,1]
  lines(x=c(min(x),max(x)),y=c(transform(min(x),m=slope,b=intercept),transform(max(x),m=slope,b=intercept)),col=color,...)
}

# returns y given x along a line
transform <- function(x,m,b){
  return((m*x)+b)
}




# example
## scatter(rnorm(50,50,10),rnorm(50,30,3))
## scatter(trees[,1],trees[,2],xlab="tree girth (in.)",ylab="tree height (ft.)",main="scatter() example")



#' plot quantitative x quantitative data as scatter plot
#'
#' scatter plot with nice defaults. automatically tests for a linear association between your two variables, draws regression lines, prints stats to the plot, etc.
#'
#' @param x vector of data for the x axis
#' @param y vector of data for the y axis
#' @param xlab x label
#' @param ylab y label
#' @param line logical. draw regression line? defaults to T
#' @param stats logical. compute sample size, p-value for regression slope, and r squared value and print to the corner of the graph? defaults to TRUE
#' @param color color of data points. defaults to "black"
#' @param line_col color of regression line. defaults to "red"
#' @param confidenceInterval logical. plot confidence bands about the slope?
#' @param plottingCharacter type of plotting character. defaults to 16, i.e., solid point
#' @param rug logical. plot a rug plot along the x and y axes? defaults to TRUE
#' @param ... other arguments to par()
#'
#' @return None
#'
#' @examples
#' scatter(rnorm(50,50,10),rnorm(50,30,3))
#' scatter(trees[,1],trees[,2],xlab="tree girth (in.)",ylab="tree height (ft.)",main="scatter() example")
#'
#'
#' @export

scatter<-function(x,y,xlab="",ylab="",line=T,stats=TRUE,color="black",line_col="red",confidenceInterval=T,plottingCharacter=16,rug = T,...){
  op <- par(no.readonly = TRUE)
  par(lwd=1,cex=1,bg="white",xpd=FALSE)

  # error checking
  if(length(x)!=length(y)){
    stop("x and y lengths differ")
  }

  # do the actual plotting
  plot(x,y,xlab=xlab,ylab=ylab,pch=plottingCharacter,yaxt='n',bty="l",cex.axis=1.2,cex.lab=1.3,col=color,cex.lab=1.2,...)
  axis(2, las=2)

  p.value<-summary(lm(y~x))$coefficients[2,4]
  c<-summary(lm(y~x))$coefficients[2,1]
  r_squared <- summary(lm(y~x))$r.squared %>% round(3)
  sample_size <- length(x)

  # draw the line of best fit
  if(line==T){
    if(p.value<=0.05){
      line(x,y,lwd=2,color=line_col)
    }
    else{
      line(x,y,lwd=2,lty=2,color=line_col)
    }
  }

  # adding confidence intervals around the regression line
  if(confidenceInterval==T){
    model<-lm(y~x)
    xVals<-seq(min(x),max(x),.1)
    conf<-predict(model,data.frame(x=xVals),interval="confidence",level=0.95)
    lines(xVals,conf[,2],col="#00000050",lty=2,lwd=2)
    lines(xVals,conf[,3],col="#00000050",lty=2,lwd=2)
  }

  # adding 1-d 'rug plots' to the bottom and right of the plot to show distribution of each variable
  if(rug==T){
    rug(x,side=1,col="#00000070",lwd=2)
    rug(y,side=2,col="#00000070",lwd=2)
  }

  # add important stats to the plot
  if(stats==T){
    rp = vector('expression',3)
    rp[1] = substitute(expression(r^2 == r_squared),list(r_squared = format(r_squared,dig=3)))[2]
    rp[3] = substitute(expression(p == p.value), list(p.value = format(p.value, digits = 2)))[2]
    rp[2] = substitute(expression(n == sample_size), list(sample_size = format(sample_size, digits = 2)))[2]

    if(c>0){
      legend(x="bottomright",bty="n",legend=rp, y.intersp=1.2,cex=0.9,inset = 0.01)
    }
    else{
      legend(x="bottomleft",bty="n",legend=rp,y.intersp=0.8,inset = 0.05)
    }
  }

  # reset the par values
  on.exit(par(op))
}




# code adapted from from http://www.r-bloggers.com/example-10-3-enhanced-scatterplot-with-marginal-histograms/

#' plot quantitative x quantitative data as scatter plot with marginal histograms
#'
#' scatter plot with nice defaults. automatically tests for a linear association between your two variables, draws regression lines, prints stats to the plot,plots marginal histograms, etc.
#'
#' @param x vector of data for the x axis
#' @param y vector of data for the y axis
#' @param title main title
#' @param xlab x label
#' @param ylab y label
#' @param line logical. draw regression line? defaults to T
#' @param stats logical. compute sample size, p-value for regression slope, and r squared value and print to the corner of the graph? defaults to TRUE
#' @param color color of data points. defaults to "black"
#' @param line_col color of regression line. defaults to "red"
#' @param confidenceInterval logical. plot confidence bands about the slope?
#' @param plottingCharacter type of plotting character. defaults to 16, i.e., solid point
#' @param rug logical. plot a rug plot along the x and y axes? defaults to FALSE
#' @param ... other arguments to par()
#'
#' @return None
#'
#' @examples
#' scatter(rnorm(50,50,10),rnorm(50,30,3))
#' scatter(trees[,1],trees[,2],xlab="tree girth (in.)",ylab="tree height (ft.)",title="scatter_hist() example")
#'
#'
#' @export

scatter_hist<-function(x,y,xlab="",ylab="",title = "",line=T,stats=TRUE,color="black",line_col="red",confidenceInterval=T,plottingCharacter=16,rug = F,...){
  op <- par(no.readonly = TRUE)
  par(lwd=1,cex=1,bg="white",xpd=FALSE)
  zones <- matrix(c(1,1,1, 
                    0,5,0, 
                    2,6,4, 
                    0,3,0), ncol = 3, byrow = TRUE)
  layout(zones, widths=c(0.3,4,0.8), heights = c(1,2.3,8,.75))
  
  # error checking
  if(length(x)!=length(y)){
    stop("x and y lengths differ")
  }
  
  # get histograms
  xhist <- hist(x,plot=FALSE,breaks=10)
  yhist <- hist(y, plot=FALSE, breaks=10)
  top <- max(c(xhist$counts, yhist$counts))
  
  par(xaxt="n", yaxt="n",bty="n",  mar = c(.3,2,.3,0) +.05)
  
  # main title
  plot(x=1,y=1,type="n",ylim=c(-1,1), xlim=c(-1,1))
  text(0,0,paste(title), cex=2)
  
  # y label
  plot(x=1,y=1,type="n",ylim=c(-1,1), xlim=c(-1,1))
  text(0,0,paste(ylab), cex=1.5, srt=90)
  
  # x label
  plot(x=1,y=1,type="n",ylim=c(-1,1), xlim=c(-1,1))
  text(0,0,paste(xlab), cex=1.5)
  
  # y histogram
  par(mar = c(2,0,1,1))
  barplot(yhist$counts, axes = FALSE, xlim = c(0, top),space = 0, horiz = TRUE, col="grey50", border="grey50")
  
  # x histogram
  par(mar = c(0,2,1,1))
  barplot(xhist$counts, axes = FALSE, ylim = c(0, top), space = 0, col="grey50", border="grey50")
  
  # do the actual plotting
  par(mar = c(2,2,.5,.5), xaxt="s", yaxt="s", bty="n")
  plot(x,y,xlab=xlab,ylab=ylab,pch=plottingCharacter,yaxt='n',bty="l",col=color,cex.lab=1.2,...)
  axis(2, las=2)
  
  p.value<-summary(lm(y~x))$coefficients[2,4]
  c<-summary(lm(y~x))$coefficients[2,1]
  r_squared <- summary(lm(y~x))$r.squared %>% round(3)
  sample_size <- length(x)
  
  # draw the line of best fit
  if(line==T){
    if(p.value<=0.05){
      line(x,y,lwd=2,color=line_col)
    }
    else{
      line(x,y,lwd=2,lty=2,color=line_col)
    }
  }
  
  # adding confidence intervals around the regression line
  if(confidenceInterval==T){
    model<-lm(y~x)
    xVals<-seq(min(x),max(x),.1)
    conf<-predict(model,data.frame(x=xVals),interval="confidence",level=0.95)
    lines(xVals,conf[,2],col="#00000050",lty=2,lwd=2)
    lines(xVals,conf[,3],col="#00000050",lty=2,lwd=2)
  }
  
  # adding 1-d 'rug plots' to the bottom and right of the plot to show distribution of each variable
  if(rug==T){
    rug(x,side=1,col="#00000070",lwd=2)
    rug(y,side=2,col="#00000070",lwd=2)
  }
  
  # add important stats to the plot
  if(stats==T){
    rp = vector('expression',3)
    rp[1] = substitute(expression(r^2 == r_squared),list(r_squared = format(r_squared,dig=3)))[2]
    rp[3] = substitute(expression(p == p.value), list(p.value = format(p.value, digits = 2)))[2]
    rp[2] = substitute(expression(n == sample_size), list(sample_size = format(sample_size, digits = 2)))[2]
    
    if(c>0){
      legend(x="bottomright",bty="n",legend=rp, y.intersp=1.2,cex=0.9,inset = 0.01)
    }
    else{
      legend(x="bottomleft",bty="n",legend=rp,y.intersp=0.8,inset = 0.05)
    }
  }
  
  # reset the par values
  on.exit(par(op))
}


##############
### beeStrip + mod
##################
#data(iris)
# beeStripMod(iris$Sepal.Length,iris$Species,xlab="species",ylab="sepal length",main="beeStripMod() example")


#' plot qualitative x quantitative as Tufte's boxplot + histogram of data in each group
#'
#' plots a modified boxplot from Tufte for each group alongside the data as arranged as a histogram
#'
#' @param data either a column in a dataframe containing the raw data you wish to plot, or a list, where len(your_list) == number of groups you're plotting
#' @param group if `data` is a column in a dataframe, `group` is a column in the same dataframe giving the group that each observation in `data` belongs to. If `data` is a list, don't worry about this argument.
#' @param lab labels for the groups
#' @param line_width width of the lines for the modified boxplots
#' @param point_col color of the data points for each group. defaults to viridis colors
#' @param y_limits limits of the y axis
#' @param sample_size logical. If TRUE, print the sample size under each group
#' @param stats logical. print overall p value from anova?
#' @param red_median logical. Should the median be red? If FALSE, defaults to the color of the group.
#' @param ... other arguments to par()
#'
#' @return None
#'
#' @examples
#' data(iris)
#' beeStripMod(iris$Sepal.Length,iris$Species,xlab="species",ylab="sepal length",main="beeStripMod() example")
#'
#'
#' @export

beeStripMod<-function(data,group,lab=rep(c(),length(data)),point_size=1.4,beeMethod="center",line_width=3.0,point_col=ifelse(is.list(data) %>% rep(20),viridis(length(data)+1)[1:length(data)],viridis(nlevels(group)+1)[1:nlevels(group)]),y_limits=c(ifelse(is.list(data),min(unlist(data)),min(data)),ifelse(is.list(data),max(unlist(data),max(data)))),sample_size=T,side=-1,red_median=T,stats=T,...){

  # if response is missing, assume data is a list
  if(missing(group)){
    if(is.list(data)==FALSE){
      stop("enter your data either as a list or as a response variable and factor within a dataframe")
    }
  }

  if(is.list(data)){
    print("data are a list")
    number_groups<-length(group)
    # get statistics you need to plot
    boxplot_table<-boxplot(data,plot=F)
    # create the plot
    beeswarm(data,method=beeMethod,priority="density",pch=16,col=point_col,cex=point_size,side = side,bty='l',yaxt="n",cex.axis=1.2,cex.lab=1.3,labels=lab,...)
    # create y axis with the numbers the correct direction
    axis(2,las=2)
  }

  else{
    number_groups<-nlevels(group)
    boxplot_table<-boxplot(data~group,plot=F)
    # create the plot
    beeswarm(data~group,method=beeMethod,priority="density",pch=16,col=point_col,cex=point_size,side = side,bty='l',yaxt="n",cex.axis=1.2,cex.lab=1.3,labels=lab,...)
    # create y axis with the numbers the correct direction
    axis(2,las=2)

  }
  
    if(stats==T){
      print(TukeyHSD(aov(data~group)))
      x = summary(aov(data~group))[[1]][["Pr(>F)"]][1] %>% round(4)
      if(x ==0){
        x = "p < 0.001"
      }
      else{
        x = paste("p = ",x,sep="")
      }
      text(x=(median(1:number_groups)),y=max(data),labels=paste("anova, ",x,sep=""),pos=1)
    }

  x_values = 1:number_groups
  point_col = point_col[1:number_groups]
  scale = 0.05

  # plot the mean as a point
  for(i in 1:number_groups){
    points(x=x_values[i]+(scale*number_groups), y=boxplot_table$stats[3,i],cex=1.1,pch=16,col=ifelse(red_median==T,"red",point_col[i]))
  }
  # draw lines representing 1 and fourth quartiles
  for(i in 1:number_groups){
    lines(x=rep(x_values[i]+(scale*number_groups),2),y=c(boxplot_table$stats[1,i],boxplot_table$stats[2,i]),lwd=2)
    lines(x=rep(x_values[i]+(scale*number_groups),2),y=c(boxplot_table$stats[4,i],boxplot_table$stats[5,i]),lwd=2)
  }


  # for plotting sample size below each group
  if(sample_size==TRUE){
      text(x=x_values+(scale*number_groups),y=min(data)*0.99,paste("n = ",boxplot_table$n,sep=""),col="grey20")
  }
}


##############
### beeStrip + boxplot
##################
#data(iris)
# beeStripBox(iris$Sepal.Length,iris$Species,xlab="species",ylab="sepal length",main="beeStripBox() example")

#' plot quantitative x categorical data
#'
#' plots a histogram of your individual data points alongside a boxplot for each group
#'
#' @param data list of groups you want to plot, or, if group is not left blank, a data frame
#' @param group if you gave a dataframe for data, group is the name of the column containing group identities
#' @param lab labels for groups
#' @param point_size size of plotting characters. defaults to 1.4
#' @param beeMethod see beeswarm. defaults to "center"
#' @param line_width width of the line
#' @param point_col color of the each group. defaults to viridis colors
#' @param y_limits limits of the y axis
#' @param mean does nothing right now
#' @param sample_size logical. plot sample size under each group?
#' @param side defaults to -1. determines whether the histogram are stacked to the right or the left
#' @param stats logical. do an ANOVA and print the p-value to the plot?
#' @param box_thickness thickness of the lines that form the boxplot. defaults to 0.2
#' @param box_color defaults to FALSE. color of the lines that form the boxes
#' @param ... other arguments to pass to par()
#'
#'
#'
#' @return ANOVA results
#'
#' @examples
#' data(iris); beeStripBox(iris$Sepal.Length,iris$Species,xlab="species",ylab="sepal length",main="beeStripBox() example")
#'
#' @export

beeStripBox<-function(data,group,lab=rep(c(),length(data)),point_size=1.4,beeMethod="center",line_width=3.0,point_col=ifelse(is.list(data) %>% rep(20),viridis(length(data)+1)[1:length(data)],viridis(nlevels(group)+1)[1:nlevels(group)]),y_limits=c(ifelse(is.list(data),min(unlist(data)),min(data)),ifelse(is.list(data),max(unlist(data),max(unlist(data))))),mean=FALSE,sample_size=T,side=-1,stats=T,box_thickness = 0.2,box_color=FALSE,...){

  # if response is missing, assume data is a list
  if(missing(group)){
    if(is.list(data)==FALSE){
      stop("enter your data either as a list or as a response variable and factor within a dataframe")
    }
  }

  if(is.list(data)){
    print("data are a list")
    number_groups<-length(data)
    point_col = point_col[1:number_groups]
    # get statistics you need to plot
    boxplot_table<-boxplot(data,plot=F)
    # create the plot
    beeswarm(data,method=beeMethod,priority="density",pch=16,col=point_col,ylim=y_limits,cex=point_size,cex.lab=1.2,side = side,bty='l',yaxt="n",cex.lab=1.3,,cex.axis=1.2,labels=lab,...)
    # create y axis with the numbers the correct direction
    axis(2,las=2)
    boxplot(data, main = "", axes = FALSE,at = 1:number_groups+0.2, xlab=" ", ylab=" ", border = ifelse(box_color==T,point_col,"black"), add=TRUE ,boxwex = box_thickness,pars = list(medlty = 1, whisklty = c(1, 1), medcex = 0.7, outcex = 0, staplelty = "blank"))
  }

  else{
    number_groups<-nlevels(group)
    boxplot_table<-boxplot(data~group,plot=F)
    point_col = point_col[1:number_groups]
    # create the plot
    beeswarm(data~group,method=beeMethod,priority="density",pch=16,yaxt="n",col=point_col,cex=point_size,side = side,bty='l',cex.axis=1.2,cex.lab=1.3,labels=lab,...)
    # create y axis with the numbers the correct direction
    axis(2,las=2)
    boxplot(data~group, add=TRUE, main = "",border = ifelse(box_color==T,point_col,"black"), at = 1:number_groups+0.2, axes = FALSE, xlab=" ", ylab=" ", boxwex = box_thickness, pars = list(medlty = 1, whisklty = c(1, 1), medcex = 0.7, outcex = 0, staplelty = "blank"))

    if(stats==T){
      print(TukeyHSD(aov(data~group)))
      x = summary(aov(data~group))[[1]][["Pr(>F)"]][1] %>% round(4)
      if(x ==0){
        x = "p < 0.001"
      }
      else{
        x = paste("p = ",x,sep="")
      }
      text(x=(median(1:number_groups)),y=max(unlist(data)),labels=paste("anova, ",x,sep=""),pos=1)
    }

  }

  x_values = 1:number_groups
  scale = 0.05

  # for plotting sample size below each group
  if(sample_size==TRUE){
    for(i in 1:number_groups){
      text(x=x_values[i]+(scale*number_groups),y=min(unlist(data))*0.99,paste("n = ",boxplot_table$n[i],sep=""),col="grey20")
    }
  }
}




## modified boxplot
## example:
### x<-data.frame(c(rnorm(20,5),rnorm(30,10,1.5),rnorm(25,12,1.6),rnorm(20,20,3)),c(rep("A",20),rep("B",30),rep("C",25),rep("D",20)))
### mod(x[,1],x[,2])
mod<-function(response,group,lab=levels(group),...){

  # make sure group is a factor; if not, convert it to one
  if(is.factor(group)!=TRUE){
    warning("'group' is not a factor: converting now")
    group<-factor(group)
  }

  number_groups<-nlevels(group)

  # get statistics you need to plot
  boxplot_table<-boxplot(response~group,plot=F)

  # create the plot
  plot(c(0,1),c(min(response),max(response)),type="n",xaxt="n",yaxt="n",...)
  # create y axis with the numbers the correct direction
  axis(2,las=2)
  x_values<-seq(0.7,0.3,length.out=number_groups)

  # plot the median
  points(x=x_values, y=boxplot_table$stats[3,1:number_groups],cex=1.1,pch=16,col="red")

  # creare x axis
  axis(side=1,at=x_values,labels=lab)

  # draw lines representing 1 and fourth quartiles
  for(i in 1:number_groups){
    lines(x=rep(x_values[i],2),y=c(boxplot_table$stats[1,i],boxplot_table$stats[2,i]),lwd=2)
    lines(x=rep(x_values[i],2),y=c(boxplot_table$stats[4,i],boxplot_table$stats[5,i]),lwd=2)
  }
}

# histogram
# histogram(rnorm(100))
histogram=function(x,color="grey50",bor="grey50",rug = TRUE,...){
  hist(x,col=color,border=bor,lwd=2,cex.lab=1.2,yaxt="n",...)
  # create y axis with the numbers the correct direction
  axis(2,las=2)
  if(rug == TRUE){
    rug(x, side = 1)
  }
}






# boxplot with point for s.e.m. / ci + data jittered

#' plot quantitative x categorical data various ways
#'
#' boxplot + mean with fences for CIs or s.e.m.s + data jittered
#'
#' @param y either a list of the data you wish to plot, where length(data) == # of groups, or a column of a dataframe containing y axis values
#' @param x NA if y is a list. Otherwise a column in a dataframe containing the group labels for each observation in y
#' @param lab labels for groups
#' @param SEM draw s.e.m.'s as fences? defaults to FALSE
#' @param CI draw 95 percent CIs as fences? defaults to TRUE
#' @param box_thickness thickness of the boxes of your boxplots
#' @param plot_data logical. plot the data jittered alongside the boxplot? Defaults to TRUE
#' @param colors colors of the data points. defaults to viridis colors
#' @param ... other arguments to pass to par()
#'
#'
#' @return none
#'
#' @examples
#' data(iris); cats_meow(iris$Sepal.Length,iris$Species, ylab="sepal length", xlab = "species")
#'
#' @export

cats_meow <- function(y, x=NA, lab=NA, SEM=FALSE, CI=TRUE, box_thickness = 0.2, plot_data=T, colors = viridis(5)[1:4], ...){
  
  # if the data are entered as a list, coerse to a dataframe
  if(missing(x)){
    
    if(is.list(y)){
      # if its a list, turn it into a dataframe
      # this is a stupid way to make this data frame, but whatever:
      if(missing(lab)){
        lab <- letters[1:length(y)]
      }
      
      labels <- c()
      sizes <- sapply(y,length)
      for(i in 1:length(y)){
        labels <- c(labels,rep(lab[i], sizes[i]))
      }
      df <- data.frame(matrix(unlist(y), nrow=length(unlist(y)), byrow=T),labels)
      names(df)<-c("dat","groups")
      y <- df$dat
      x <- df$groups
    }
    
  }
  
  
  # plot boxes
  (stats<-boxplot(y~x, boxwex = box_thickness, bty='l', cex.axis=1.2,cex.lab=1.3,pars = list(medlty = 2, medlwd=1, boxlty=2, whisklty = c(2, 2), medcex = 1, outcex = 0, staplelty = "blank"), ...))
  
  # run t-test to get CIs and means later
  tests <- by(y,x,t.test)
  
  CIs_lower <- c()
  CIs_upper <- c()
  means <- c()
  
  # get CIs and means
  for(i in 1:length(tests)){
    means <- c(means, tests[[i]]$estimate)
    CIs_lower <- c(CIs_lower, tests[[i]]$conf.int[1])
    CIs_upper <- c(CIs_upper, tests[[i]]$conf.int[2])
  }
  sems <- by(y,x,sd) %>% divide_by(sqrt(by(y,x,length))) %>% as.vector
  
  # plot sems
  if(SEM == TRUE){
    for(i in 1:(stats$n %>% length)){
      lines(x=c(i,i), y=c(means[i], means[i]+sems[i]), lwd=1.4, col="black")
      lines(x=c(i,i), y=c(means[i], means[i]-sems[i]), lwd=1.4, col="black")
    }
  }
  
  # plot CIs
  if(CI == TRUE){
    for(i in 1:(stats$n %>% length)){
      lines(x=c(i,i), y=c(means[i], CIs_lower[i]), lwd=1.4, col="black")
      lines(x=c(i,i), y=c(means[i], CIs_upper[i]), lwd=1.4, col="black")
    }
  }
  
  # plot the data
  if(plot_data==TRUE){
    for(i in 1:(stats$n %>% length)){
      points(x=rep(i, stats$n[i]) %>% jitter(amount = 0.07) + 0.25, y=by(y,x,c)[[i]], pch=16, col=colors[i], cex=1.1)
    }
  }
  
  # plot the means
  points(1:(stats$n %>% length),means,pch=16,cex=1.3,col="black")
  
}




#' plot paired data
#'
#' boxplot + paired data connected with points
#'
#' @param y either a list of the data you wish to plot, where length(data) == # of groups, or a column of a dataframe containing y axis values
#' @param x NA if y is a list. Otherwise a column in a dataframe containing the group labels for each observation in y
#' @param lab labels for groups
#' @param SEM draw s.e.m.'s as fences? defaults to FALSE
#' @param CI draw 95 percent CIs as fences? defaults to TRUE
#' @param box_thickness thickness of the boxes of your boxplots
#' @param plot_data logical. plot the data jittered alongside the boxplot? Defaults to TRUE
#' @param colors colors of the data points. defaults to viridis colors
#' @param ... other arguments to pass to par()
#'
#'
#' @return none
#'
#' @examples
#' paired(rnorm(10,5), rnorm(10,7), lab = c("X", "X_paired"), ylab = "measurement", main="paired() example")
#'
#' @export

paired <- function(x, y, lab=NA, box_thickness = 0.2, plot_points=T, colors = c("#440154FF", "#21908CFF") %>% addAlpha(0.1), line_color = "grey20", ...){
  
  # if the data are entered as a list, coerse to a dataframe
  if(missing(x) | missing(y)){
    stop("you must supply two vectors to plot")
  }
  
  if(length(x) != length(y)){
    stop("the number of observations in the two vectors are unequal. these cannot be paired data")
  }
  
  
  # plot boxes
  (stats<-boxplot(list(x,y), boxwex = box_thickness, bty='l', names=lab, cex.axis=1.2,cex.lab=1.3, ..., pars = list(medlty = 1, medlwd=1, boxlty=1, whisklty = c(1, 1), medcex = 1, outcex = 0, staplelty = "blank")))
  
  # plot the data
  if(plot_points==TRUE){
    for(i in 1:length(x)){
      points(rep(1.2, length(x)), x, col=colors[1], pch=16)
      points(rep(1.8, length(x)), y, col = colors[2], pch=16)
    }
  }
  
  # plot the lines connecting paired data points
  for(i in 1:length(x)){
    lines(x = c(1.22,1.78), y=c(x[i], y[i]), col=line_color)
  }
}








### some non-parametric stats functions
#############################################
### monte carlo, two-samples, unpaired #####
###########################################
# the idea here is to randomize associations of the data with the group
# then calculate the difference in the group means under the null hypothesis
# then compare it to the observed difference in means

## should do some more error checking to make sure this works, but looks good, 14 may 2014


monte_unpaired<-function(group_a,group_b,n=9999,null=0,table=TRUE){
  values<-vector(length=n)
  crit = mean(group_a)-mean(group_b)
  diff<-abs(crit-null)

  # make a data frame
  dat<-data.frame(c(group_a,group_b),c(rep("group_a",length(group_a)),rep("group_b",length(group_b))))
  names(dat)<-c("values","group")

  for(i in 1:n){
    dat$group<-sample(dat$group,replace=F)
    x<-tapply(dat$value,dat$group,mean)
    values[i]<-x[[1]]-x[[2]]
  }

  lower_bound <- quantile(values,0.025)[[1]]
  upper_bound <- quantile(values,0.975)[[1]]

  p <- ((length(values[values<=null-diff])+length(values[values>=null+diff]))+1)/(n+1)
  # why add the +1's? see Ruxton and Neuha¨user, methods in ecology and evolution, 2013, doi:10.1111/2041-210X.12102

  # plot
  par(lwd = 3,family = 'Helvetica',cex.lab=1.3,cex.lab=1.3)
  (plot<-hist(values,cex.lab=1.3,xlab="Simulated Differences",main="Monte Carlo Simulation",col="#99999940",breaks=20))
  text(mean(values),max(plot$counts),paste("observed = ",round(crit,3)),pos=4)
  segments(crit,0,crit,n,lty=2)
  segments(lower_bound,0,lower_bound,n,lty=3,lwd=2)
  segments(upper_bound,0,upper_bound,n,lty=3,lwd=2)

  if(table==TRUE){
    g<-c(round(lower_bound,2),round(upper_bound,2),round(p,3))
    h<-c("lower_bound","upper_bound","p-value")
    result<-data.frame(h,g)
    colnames(result)<-c(" "," ")
    print(paste("based on ",n+1," iterations"))
    # I construct a confidence interval for the p-value based on the same paper above:
    # Ruxton and Neuha¨user, methods in ecology and evolution, 2013, doi:10.1111/2041-210X.12102
    ci <- 1.96*sqrt((p*(1-p))/(n+1))
    print(paste("lower confidence level for p-value:",round(p-ci,3),". Upper confidence interval: ",round(p+ci,3)))
    print(result)
    #return(values)
  }
  else{
    print(paste("based on",n,"iterations"))
    return(p)
  }
}


#######################################################
# monte carlo method for a two-sample paired t-test ##
#####################################################
# example of use:
#monte_paired(df,n=9999)

monte_paired<-function(data,n=9999,table=TRUE,diff_col=3,one_sided=F){
  x<-vector(length=n)
  if(!is.data.frame(data)){
    stop("please enter a data frame as the first argument")
  }

  if(one_sided==T){
    crit <- (mean(data[,diff_col]))
  }
  else{
    crit <- abs(mean(data[,diff_col]))
  }


  for(i in 1:n){
    data1<-data.frame(t(apply(df[,1:2],1,sample)))
    data1$diff<-data1[,1]-data1[,2]
    if(one_sided==T){
      x[i]<-(mean(data1$diff))
    }
    else{
      x[i]<-abs(mean(data1$diff))
    }
  }


  extreme <- length(x[x>=crit])+1
  p <- extreme/(n+1)
  # why add the +1's? see Ruxton and Neuha¨user, methods in ecology and evolution, 2013, doi:10.1111/2041-210X.12102
  par(lwd = 3,family = 'Helvetica',cex.lab=1.3,cex.lab=1.3)

  (plot<-hist(x,cex.lab=1.3,xlab="Simulated Differences",main="MC Simulation",col="#99999940"))
  text(mean(x),max(plot$counts),paste("observed = ",round(crit,1)),pos=4)
  segments(crit,0,crit,n,lty=2)

  if(table==TRUE){
    g<-c(crit,round(extreme,0),p)
    h<-c("observed diff","num_extreme","p-value")
    result<-data.frame(h,g)
    colnames(result)<-c(" "," ")
    print(result)
    #return(x)
  }
  else{
    return(x)
  }
}

# calculate pooled SD from two sample (x and y) as defined by Cohen 1988
# pooled_sd(c(21,30,29),c(42,41,40,39))
pooled_sd <- function(x,y){
  num <- ((length(x)-1)*sd(x)) + ((length(y)-1)*sd(y))
  denom <- length(x) + length(y) - 2
  return(sqrt(num/denom))
}


# calculate cohen's d, a measure of effect size
# cohens_d(rnorm(50),rnorm(50))

cohens_d <- function(x,y){
  d<- ifelse(mean(x) > mean(y),(mean(x)-mean(y))/pooled_sd(x,y), (mean(y)-mean(x))/pooled_sd(x,y))
  return(d)
}

## Add an alpha value to a colour
# from http://www.magesblog.com/2013/04/how-to-change-alpha-value-of-colours-in.html


#' add transparency to a color
#'
#' from http://www.magesblog.com/2013/04/how-to-change-alpha-value-of-colours-in.html
#'
#' @param col the color, or vector of colors, you want to add transparency to
#' @param alpha the amount of transparency. ranges from 0 to 1 inclusive, where 0 is completely transparent and 1 is completely opaque
#'
#' @return color value with transparency added
#'
#' @examples
#' "red" %>% addAlpha(0.5)
#' viridis(5) %>% addAlpha(0.7)
#' compare:
#' x <- list(rnorm(50,5),rnorm(50,5),rnorm(50,5),rnorm(50,5),rnorm(50,5),rnorm(50,5),rnorm(50,5),rnorm(50,5),rnorm(50,5))
#' simple(x, point_col = plasma(10))
#' simple(x, point_col = plasma(10) %>% addAlpha(0.5))
#'
#' @export

addAlpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}
