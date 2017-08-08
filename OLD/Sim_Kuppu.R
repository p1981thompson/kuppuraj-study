#-------------------------------------------------------------------------
# Simulation of Kuppu data
#-------------------------------------------------------------------------


#setwd('/Users/dorothybishop/Dropbox/SL and WM paper/Data and Analysis')

# Remember! you will need to download these packages if they aren't already downloaded
library(MASS) #for mvrnorm function to make multivariate normal distributed vars
library(gridExtra) #for plotting output in a grid
library(grid) #https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html

options(scipen=999) #disable scientific notation.
plot.new()
#-------------------------------------------------------------------------
# Specify parameters to create a correlation matrix
# We are going to simulate 7 variables (but you could change this number)
#-------------------------------------------------------------------------

nVar<-4 #number of simulated variables (arbitrary but useful for figure that will fit on screen)
# We will treat variable 1-6 as predictors and variable 7 as dependent variable (DV)
myM<-0 # Mean score for all variables in the sample - we're using z scores for simplicity
myVar<-1 #Remember variance is SD^2. For z-scores, SD is 1, so variance is also 1
myN<-30 #set sample size per group (You can vary this to see the effect)
myCorr<-0 #correlation between 4  variables

mynSims <- 1 # Set number of simulated datasets


myCov<-matrix(rep(myCorr,nVar*nVar),nrow=nVar) 
# Look at myCov after running this line; you have 7 x 7 matrix with all entries myCorr
# Now we change the final row and column to match myCorr2
myCov[nVar,]<-myCorr2  #note that we specify nVar to indicate last row, but leave 
#blank after the comma: this means we want all columns
myCov[,nVar]<-myCorr2 #note that we leave blank before comma to indicate we want 
#all rows; nVar indicates we want last column

# Look again at myCov after these two steps to check you understand the commands

# We need to fix our matrix so that the diagonal values correspond to variance of each variable
diag(myCov)<-rep(myVar,nVar) # now look at myCov again


# NB for simplicity, we've made all intercorrelations the same, but you could hand craft your matrix
# to have different correlations between different variables. 

#----------------------------------------------------------------------------------------
#for (i in 1:mynSims){ # 'for loop' command: runs loop mynSims times
  # Each time the code between { and } is run, the index, i, increases by one
  #N.B. if running code one line at a time, any line with { in it will 
  # not execute until a corresponding } has been found (or entered on console)
  
  #----------------------------------------------------------------------------------------
  # Generate a sample from a multivariate normal distribution with the specified correlation matrix
  #----------------------------------------------------------------------------------------
  
  mydata<-data.frame(mvrnorm(n = myN*2, rep(myM,nVar), myCov))
 
  adrange<-1:myN
  chrange<-(myN+1):(2*myN)
  mydata$group[adrange]<-0
  mydata$group[chrange]<-1
  colnames(mydata)<-c('D1P1','D1P2','D2P1','D2P2','grp')
  myES<-matrix(c(-.6,0,0,.6,
                 -.8,-.2,0,1),nrow=2,byrow=TRUE)
  colnames(myES)<-colnames(mydata)
  
  #Add effect sizes to simulated data
  for (j in 1: 2){
    myrange<-adrange
  if (j == 2){myrange<-chrange}
   for (i in 1:4){
     mydata[myrange,i]<-mydata[myrange,i]+myES[j,i]
   }
  }

aggregate(mydata, by=list(mydata$grp),
FUN=mean, na.rm=TRUE)


aggregate(mydata, by=list(mydata$grp),
                    FUN=sd, na.rm=TRUE)


 
  