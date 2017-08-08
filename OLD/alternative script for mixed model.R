#################################################################################
#
# Power calculation for linear mixed effects model (kuppu) - version 6
#
#################################################################################

# Created by Paul Thompson, 12-05-2017
#editied after review by Paul thompson 20-06-2017

#################################################################################
#install.packages(c("devtools","nlme","car","lmerTest"))

#library(devtools)
#install_github("lme4", user = "lme4")
library(lme4)
library(car)
library(lmerTest)
library(foreach)

#run model for Kuppu's pilot data using NLME and LME4. Need both as lme function produces p-values but lmer does not.

mixed3new<-lmer(RT~ 1 + Block + Dis_TP + (1+Block+Block:Dis_TP|ID), data=mm_data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
#mixed4<-lme(RT~ 1 + Block + TP , random=~Block|ID, data=mm_data)

#Simulate data based on N participants, 5 blocks and 4 types (ad_DT, nd_DT, ad_PR, nd_PR). These will be then recoded as in the orginal data. 
N=84
nsim<-500 
expdat <- expand.grid(ID = factor(1:N), Block = factor(1:5), Dis_TP=factor(1:5))

#Recode into familiar factors for Kuppu/DB hypotheses.
#expdat$Type<-as.numeric(expdat$Type)
expdat$Block<-scale(as.numeric(expdat$Block))

expdat$Dis_TP<-car::recode(expdat$Dis_TP,"1='nonD';2='adjD';3='nonP';4='adjP';5='rand'")


#expdat$Dis_TP<-as.factor(expdat$Dis_TP)
expdat$Dis_TP<-factor(expdat$Dis_TP,levels=c("rand","adjD", "nonD", "adjP", "nonP"))


#Extract parameters for simulating data. (artificially reduce the effects of block and TP to see if power is working. Are the estimates reasonable)
newparamsA <- list(
  beta = getME(mixed3new,"beta"), #fixed effects (intercept, Block, TPPR)
  theta = getME(mixed3new, "theta"),#Random slope
  sigma = getME(mixed3new, "sigma"))#random slope variance


# Simulate:

ss <- simulate(~ Block + Dis_TP + (1+Block+Block:Dis_TP|ID), nsim = nsim, newdata = expdat, newparams = newparamsA,family=gaussian)#syntax "block-1" means we don't use random intercepts for block.


lmer(RT ~ Block + Dis_TP +(1 + Block + Block:Dis_TP|ID),data=expdat, REML = FALSE, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
#####################################################################


fitsim3 <- function(i) {
  
  N=84
  nsim<-500 
  expdat <- expand.grid(ID = factor(1:N), Block = factor(1:5), Dis_TP=factor(1:5))
  expdat$Block<-scale(as.numeric(expdat$Block))
  expdat$Dis_TP<-car::recode(expdat$Dis_TP,"1='nonD';2='adjD';3='nonP';4='adjP';5='rand'")
  expdat$Dis_TP<-factor(expdat$Dis_TP,levels=c("rand","adjD", "nonD", "adjP", "nonP"))

  # Simulate:
  
  expdat$RT <- unlist(simulate(~ Block + Dis_TP + (1 + Block + Block:Dis_TP|ID), nsim = 1, newdata = expdat, newparams = newparams2A,family=gaussian))
  
  #new1<-lmer(RT ~ Block + Dis_TP + Block:Dis_TP +(1 + Block + Block:Dis_TP|ID),data=expdat, REML = FALSE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  new1<-lmer(RT ~ Block + Dis_TP +(1 + Block + Block:Dis_TP|ID),data=expdat, REML = FALSE, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  print(summary(new1))
  
  coef1<-coef(summary(new1))
  
  data.frame(i=i,int1.coef=coef1["Block:Dis_TPadjD",5],int2.coef=coef1["Block:Dis_TPnonD", 5],int3.coef=coef1["Block:Dis_TPadjP", 5],int4.coef=coef1["Block:Dis_TPnonP", 5])
}

fitAll3 <- foreach(i=1:500,.combine=rbind, .packages=c('lme4','optimx','lmerTest','car')) %dopar% {fitsim3(i)}

fitAll3A<-as.data.frame(fitAll3)

int1_pwr1<-mean(unlist(fitAll3A[,2])< 0.05)
#int1_pwr2<-mean(unlist(fitAll3A[,2]))

int2_pwr1<-mean(unlist(fitAll3A[,3])< 0.05)
#TPPR_pwr2<-mean(unlist(fitAll3A[,4]))

int3_pwr1<-mean(unlist(fitAll3A[,4])< 0.05)

int4_pwr1<-mean(unlist(fitAll3A[,5])< 0.05)

pwrs<-c(int1_pwr1=int1_pwr1,int2_pwr1=int2_pwr1,int3_pwr1=int3_pwr1,int4_pwr1=int4_pwr1)
pwrs
