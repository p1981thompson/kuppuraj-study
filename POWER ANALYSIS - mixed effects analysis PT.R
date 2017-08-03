#################################################################################
#
# Power calculation for linear mixed effects model (kuppu)
#
#################################################################################

# Created by Paul Thompson, 03-05-2017


#http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12504/full

#https://cran.r-project.org/web/packages/simr/vignettes/fromscratch.html

#################################################################################

install.packages("simr")
library(simr)

mixed3<-lmer(RT~ 1 + Block + TP + (Block-1|ID), data=mm_data)

pc1<-powerSim(mixed3, test=fixed("Block"),nsim=500, seed=1981)
pc1<-powerSim(mixed3, test=fixed("TPPR"),nsim=500, seed=1981)

#simulate additional subjects based on the pilot data
mixed3ex<-extend(mixed3,along="ID",n=50)

#repeat the power calc based on the new N.
pc2<-powerSim(mixed3ex, test=fixed("Block"),nsim=100, seed=1981)

#plot the power curve

## I think there was a bug in the simr package, so needed this fix:

# simr:::getDefaultXname<-function (obj) 
# {
#   rhs <- formula(obj)[[3]]
#   a <- all.vars(rhs)[[1]]
#   b<-ifelse(stringr::str_trim(stringr::str_split(deparse(rhs), stringr::fixed("+"))[[1]][1])==1,stringr::str_trim(stringr::str_split(deparse(rhs), stringr::fixed("+"))[[1]][2]),stringr::str_trim(stringr::str_split(deparse(rhs), stringr::fixed("+"))[[1]][1]))
#   if (a != b) 
#     stop("Couldn't automatically determine a default fixed effect for this model.")
#   return(a)
# }

#to fix in package use: fixInNamespace("getDefaultXname","simr")


PC1<-powerCurve(mixed3ex,along="ID")
plot(PC1)


mixed3ex<-extend(mixed3,along="ID",n=500)

pc2<-powerCurve(mixed3ex,test=fixed("TPPR"),along="ID")
