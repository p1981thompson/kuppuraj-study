# K.Sengottuvel adjusted the script originally from R bloggers link "https://www.r-bloggers.com/power-and-sample-size-for-repeated-measures-anova-with-r/".

#Amendments: P.Thompson 16/01/2017 - adjusted the model error setup to correct model and changed the "runTest" function to allow for multiple power calculations based on 5 different hypotheses.


##########################

#simulates and calculates power for 2(groups as within sub factor), 3 (dependency types as within sub factor),
#3 (Predictability as within Subject factors)

#??????????????it still doesnt have a way to include effect size in to it. What we have is eta squared values of about .3. Which could be small to medium effect size
#ref: 10.1111/lang.12137. The study exployed approach similar to the present one. However, the it was a within subject design of 2x3x6. On an average it showed an effect size of .3
# we used that for the present study. 

nPerGroup <- 20 # total participants per group 
nDepType  <- 3 #levels in the first within sub factor 
nPredictability<-3 #levels in the first within sub factor 
RTAdult   <- c(144,30,-50,137,18,-81,69,-41,-12) # means taken from the Pilot data of 5 adults-in the order DEpType:Predic, 1 1, 1 2, 1 3,2 1, 2 2, 2 3, 3 1, 3 2, 3 3  
RTChild    <- c(80,10,-90,180,40,-150, -80,-120,-80) # means taken from pilot data of 11 children 
stdevs    <- rep(60,9) # It is the average of deviations across the 18 values mentioned above,
#kept it constant to avoid the Sigma being negative-which gives error. 
stdiff    <- 60  #Difference of SD between values of two groups. 
nSim      <- 1000 # number of simulations, can vary. 
# set up the indep var data
Subject <- factor(1:(nPerGroup*2))
DepType <- factor(1:nDepType, labels = c("Adj", "Scnd", "NAdj"))
Predictability <- factor(1:nPredictability, labels = c("1", ".5", ".3"))
theData <- expand.grid(DepType, Predictability,Subject)
names(theData) <- c("DepType", "Predictability","Subject")

tmp <- rep(c("Adult", "Child"), each = nPerGroup *nDepType*nPredictability)
theData$Group <- factor(tmp)
theData

# to set up variance-covariance matrix
ones <- rep(1, nDepType*nPredictability)
A <- stdevs^2 %o% ones
B <- (A + t(A) + (stdiff^2)*(diag(nDepType*nPredictability) - ones %o% ones))/2

# can run it through once to check that it works
library(MASS)
tmp1 <- mvrnorm(nPerGroup, mu = RTAdult, Sigma = B)
tmp1
tmp2 <- mvrnorm(nPerGroup, mu = RTChild, Sigma = B)
tmp2
theData$RT <- c(as.vector(t(tmp1)), as.vector(t(tmp2)))
theData

aovComp <- aov(RT ~ DepType*Predictability*Group + Error(Subject/(DepType*Predictability)), theData)
summary(aovComp)
# some descriptive statistics and graphs
print(model.tables(aovComp, "means"), digits = 3) 
boxplot(RT ~ DepType, data = theData)              
boxplot(RT ~ Group, data = theData)            
boxplot(RT ~ DepType*Group, data = theData)
with(theData, interaction.plot(Group, DepType, RT,type =  "b"))

boxplot(RT ~ Predictability*Group, data = theData)  
with(theData, interaction.plot(Group, Predictability, RT,type =  "b"))

boxplot(RT ~ DepType*Predictability, data = theData)  
with(theData, interaction.plot(DepType, Predictability, RT,type =  "b"))

boxplot(RT ~ DepType*Predictability*Group, data = theData)  

###############################################
# for power estimate run the below
# don't forget to set up theData and var-cov
library(MASS)
#First lets check for the main effects
#This should give the power to detect 

runTest <- function(){
  tmp1 <- mvrnorm(nPerGroup, mu = RTAdult, Sigma = B)
  tmp2 <- mvrnorm(nPerGroup, mu = RTChild, Sigma = B)
  theData$RT <- c(as.vector(t(tmp1)), as.vector(t(tmp2)))
  aovComp <- aov(RT ~ DepType*Predictability*Group+ Error(Subject/(DepType*Predictability)), theData) #brackets missing on error, so added to correct output.
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
cat("POWER - Prediction 1 : No main effect of group = ", pow.kuppu[1],"\n")
cat("POWER - Prediction 2: No main effect of Type = ", pow.kuppu[2],"\n")
cat("POWER - Prediction 3: No main effect of predictability = ", pow.kuppu[3],"\n") 
#cat("POWER - Prediction 4: No main effect of Blocks Prediction = ", pow.kuppu[4],"\n") 

cat("POWER - 5: No interaction between Type X predictability", pow.kuppu[4],"\n")
