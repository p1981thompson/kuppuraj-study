#################################################################################
#
# Power calculation for linear mixed effects model (kuppu) - version 2
#
#################################################################################

# Created by Paul Thompson, 05-05-2017


#http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12504/full

#https://cran.r-project.org/web/packages/simr/vignettes/fromscratch.html

#################################################################################
install.packages(c("devtools","nlme","car","lmerTest"))

library(devtools)
install_github("lme4", user = "lme4")
library(lme4)
library(car)
library(nlme)
library(lmerTest)

#run model for Kuppu's pilot data using NLME and LME4. Need both as lme function produces p-values but lmer does not.

#mixed3<-lmer(RT~ 1 + Block+ TP + (Block-1|ID), data=mm_data)
#mixed4<-lme(RT~ 1 + Block + TP , random=~Block|ID, data=mm_data)

#Simulate data based on N participants, 5 blocks and 4 types (ad_DT, nd_DT, ad_PR, nd_PR). These will be then recoded as in the orginal data. 
N=10
nsim<-500
expdat <- expand.grid(ID = factor(1:N), Block = factor(1:5), Type=factor(1:4))

#Recode into familiar factors for Kuppu/DB hypotheses.
expdat$Type<-as.numeric(expdat$Type)
expdat$Block<-as.numeric(expdat$Block)

expdat$Dis_TP<-car::recode(expdat$Type,"1='nd_DT';2='ad_DT';3='nd_PR';4='ad_PR'")
expdat$Dist<-car::recode(expdat$Type,"1='nd';2='ad';3='nd';4='ad'")
expdat$TP<-car::recode(expdat$Type,"1='DT';2='DT';3='PR';4='PR'")

expdat$Dis_TP<-as.factor(expdat$Dis_TP)
expdat$Dis_TP<-factor(expdat$Dis_TP,levels=c("ad_DT", "nd_DT", "ad_PR", "nd_PR"))

expdat$Dist<-as.factor(expdat$Dist)
expdat$TP<-as.factor(expdat$TP)


#Extract parameters for simulating data.
newparams <- list(
  beta = fixef(mixed3), #fixed effects (intercept, Block, TPPR)
  theta = getME(mixed3, "theta"),#Random slope
  sigma = getME(mixed3, "sigma"))#random slope variance

# Simulate:

ss <- simulate(~ 1 + Block + TP + (Block-1|ID), nsim = nsim, newdata = expdat, newparams = newparams,family=gaussian)#syntax "block-1" means we don't use random intercepts for block.

#expdat$RT <- ss[, 1]#assigns simulated data to RT variable

#check that lmer and lme give comparable results for same model.
#mixed3new<-lmer(RT~ 1 + Block + TP + (Block-1|ID), data=expdat)

#mixed4<-lmer(RT~ 1 + Block + TP +(Block-1|ID), data=expdat)


# ctrl <- lmeControl(opt='optim');#make sure estimator has enough iterations avoiding error.
# 
# #Function to fit model multiple times to different simulated data.
# fitsim <- function(i) {
#   
#   expdat$RT <- ss[, i]
#   new1<-lmer(RT~ 1+TP+Block +(Block-1|ID),data=expdat)
#   #coef1<-coef(summary(new1))["Block", ]
#   i.new<-findInterval(0, c(confint.merMod(new1)["Block",][[1]],confint.merMod(new1)["Block",][[2]]))
#   i.report<-ifelse(i.new==1,0,1)
# }
# 
# 
# 
# #apply statement rather than a loop as it is faster in this case.
# 
# fitAll <- laply(seq(nsim), function(i) fitsim(i))
# 
# mean(fitAll)
# 



################################################################


#second part to test the "TP" factor. Looking at the boxplots, we should see an effect here. If you also look at the "DIST" boxplots, there is no expected effect in the pilot.

# fitsim2 <- function(i) {
#   
#   expdat$RT <- ss[, i]
#   new1<-lmer(RT~ 1+TP+Block +(Block-1|ID),data=expdat)
#   #coef1<-coef(summary(new1))["Block", ]
#   i.new<-findInterval(0, c(confint.merMod(new1)["TPPR",][[1]],confint.merMod(new1)["TPPR",][[2]]))
#   i.report<-ifelse(i.new==1,0,1)
# }
# 
# fitAll2 <- laply(seq(nsim), function(i) fitsim2(i))
# 
# mean(fitAll2)



#####################################################################


fitsim3 <- function(i) {
  
  expdat$RT <- ss[, i]
  new1<-lmer(RT~ 1+TP+Block +(Block-1|ID),data=expdat)
  coef1<-coef(summary(new1))
  CI<-confint.merMod(new1,quiet = TRUE)
  i.new<-findInterval(0, c(CI["Block",][[1]],CI["Block",][[2]]))
  i.report<-ifelse(i.new==1,0,1)
  #
  i.new2<-findInterval(0, c(CI["TPPR",][[1]],CI["TPPR",][[2]]))
  i.report2<-ifelse(i.new2==1,0,1)
 
  
  return(c(block.coef=coef1["Block",5],CI_count_Block=i.report,TPPR.coef=coef1["TPPR", 5],CI_count_TPPR=i.report2))
}

fitAll3 <- laply(seq(nsim), function(i) fitsim3(i))

fitAll3A<-as.data.frame(fitAll3)

block_pwr1<-mean(unlist(fitAll3A[,1])< 0.05)
block_pwr2<-mean(unlist(fitAll3A[,2]))

TPPR_pwr1<-mean(unlist(fitAll3A[,3])< 0.05)
TPPR_pwr2<-mean(unlist(fitAll3A[,4]))

pwrs<-c(block_pwr1=block_pwr1,block_pwr2=block_pwr2,TPPR_pwr1=TPPR_pwr1,TPPR_pwr2=TPPR_pwr2)
pwrs
