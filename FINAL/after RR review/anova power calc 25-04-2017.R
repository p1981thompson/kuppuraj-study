# K.Sengottuvel adjusted the script originally from R bloggers 
# link "https://www.r-bloggers.com/power-and-sample-size-for-repeated-measures-anova-with-r/".

#Amendments: P.Thompson 16/01/2017 - adjusted the model error setup to correct model and changed the "runTest" function to allow for multiple power calculations based on 5 different hypotheses.

#Amendment: P. Thompson 31/03/2017 - 
# Comments added and minor amendments, DBishop 18/4/2017

#Amendments: P Thompson 25/04/2017 - removed group (adults only)
##########################

#simulates and calculates power for Adults, 2 (dependency types as within sub factor),
#2 (Predictability as within Subject factors)

#set.seed(1) #comment this out to ensure same result with every run

#simulate raw and z-score data from estimated values
#-------------------------------------------------------------------------
# Simulation of Kuppu data for adults 
#-------------------------------------------------------------------------


#setwd("~/Dropbox/SL and WM paper/Data and Analysis")
library(MASS) #for mvrnorm function to make multivariate normal distributed vars

options(scipen=999) #disable scientific notation.
set.seed(1981)
#-------------------------------------------------------------------------
# Specify parameters to create a correlation matrix
# We are going to simulate 4 variables
#-------------------------------------------------------------------------

nVar<-4 #number of simulated variables
#these are mean learning scores for D1P1,D1P2,D2P1,D2P2

# NEED TO PLUG IN HERE REALISTIC ESTIMATES OF BLOCK1-BLOCK5 RT BY CONDITION AND GROUP
#estimates for adults
myM1<-c(100,60,133,82) # Mean score for all variables in the sample - start with raw scores (RT change)
myVar1<-10000 #SD is sqrt var so 100 for each variable

myCorr<-.4 #correlation between 4  variables (assume same for all)

myN <- 1000 # Set number of cases per group to simulate (need large N to get accurate estimate)
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Generate data from a multivariate normal distribution with the specified means and covariance matrix
#----------------------------------------------------------------------------------------
# Covariance = Correlation * SD1 * SD2
# Here, variances are equal so SD1*SD2 is same as the Variance

myCov1<-matrix(rep(myCorr*myVar1,nVar*nVar),nrow=nVar) 
diag(myCov1)<-rep(myVar1,nVar) 
mydata1<-data.frame(mvrnorm(n = myN, myM1, myCov1)) #simulated data for adults

mydata<-mydata1
 #adults are group 0 and children are group 1
mydataz<-mydata #initialise data frame for z scores

for(j in 1:(myN)){
  thisvec<-as.numeric(mydata[j,1:4])
  mydataz[j,1:4]<-(thisvec-mean(thisvec))/sd(thisvec) #zscores done row by row
}

colnames(mydataz)<-c('D1P1','D1P2','D2P1','D2P2')
colnames(mydata)<-colnames(mydataz)
#Now can inspect means/SDs and covariances/correlations for raw data and for z-scores
# These can be plugged into power analysis

mymeansraw<-colMeans(mydata[1:4])
mymeansz<-colMeans(mydataz[1:4])

mysdsraw<-colMeans(mydata[1:4])
mysdsz<-colMeans(mydataz[1:4])
#covariances/correlations for adults for zscores and raw data
mycovz1<-cov(mydataz[1:myN,1:4])
mycorz1<-cor(mydataz[1:myN,1:4])
mycovraw1<-cov(mydata[1:myN,1:4])
mycorraw1<-cor(mydata[1:myN,1:4])


#--------------------------------------------------------------------
#Now run simulation with realistic estimate of sample size
#--------------------------------------------------------------------
nPerGroup <- 84 # total participants per group 
nDepType  <- 2 #levels in the first within sub factor 
nPredictability<-2 #levels in the 2nd within sub factor 
#--------------------------------------------------------------------

# zscore estimates from section above
RTAdult <-as.numeric(mymeansz) #row1 has adult means

SDAdults<-as.numeric(mysdsz)

CovsAdult<-mycovz1

#----------------------------------------------------------------------------------

# alternative version with raw scores: comment these lines out if you want zscores
RTAdult <-as.numeric(mymeansraw) #row1 has adult means

SDAdults<-as.numeric(mysdsraw)

CovsAdult<-mycovraw1

#----------------------------------------------------------------------------------
nSim      <- 500 # number of simulations, can vary. 
# set up the indep var data
Subject <- factor(1:(nPerGroup))
DepType <- factor(1:nDepType, labels = c("Adj", "NAdj"))
Predictability <- factor(1:nPredictability, labels = c("1", ".5"))
theData <- expand.grid(DepType, Predictability,Subject) # This create a grid or matrix of potential values for the independent variables, then creates unique combinations.
names(theData) <- c("DepType", "Predictability","Subject")


# can run it through once to check that it works

tmp1 <- mvrnorm(nPerGroup, mu = RTAdult, Sigma = CovsAdult)

theData$RT <- as.vector(t(tmp1))


aovComp <- aov(RT ~ DepType*Predictability + Error(Subject/(DepType*Predictability)), theData)
summary(aovComp)
# some descriptive statistics and graphs
print(model.tables(aovComp, "means"), digits = 3) 


with(theData, interaction.plot(DepType, Predictability, RT,type =  "b"))



###############################################
# for power estimate run the below

#First lets check for the main effects
#This should give the power to detect 

runTest <- function(){
  tmp1 <- mvrnorm(nPerGroup, mu = RTAdult, Sigma = CovsAdult)
  
  theData$RT <- as.vector(t(tmp1))
  aovComp <- aov(RT ~ DepType*Predictability+ Error(Subject/(DepType*Predictability)), theData) #brackets missing on error, so added to correct output.
  b1 <- summary(aovComp)$'Error: Subject'[[1]][1,5]
  b2 <- summary(aovComp)$'Error: Subject:DepType'[[1]][1,5] 
  b3 <- summary(aovComp)$'Error: Subject:Predictability'[[1]][1,5]
  #b4 <- summary(aovComp)$'Error: Subject:Blocks'[[1]][1,5]
  b5 <- summary(aovComp)$'Error: Subject:DepType:Predictability'[[1]][1,5]
  b <- c(b1<0.05,b2<0.05,b3< 0.05,b5<0.05)
  return(b)
}
# here is estimate of power for given nPerGroup
pow.kuppu<-rowMeans(replicate(nSim,runTest()))

cat("adjust the N to ensure that all powers are above 0.8, \n 
    or reduce if all far above .8, to minimise the sample so \n 
    that you don't have to collect more than you need", "\n")
cat("POWER - main effect of Dependency = ", pow.kuppu[2],"\n")
cat("POWER - main effect of Predictability = ", pow.kuppu[3],"\n") 
cat("POWER - Dependency x Predictability = ", pow.kuppu[3],"\n") 
