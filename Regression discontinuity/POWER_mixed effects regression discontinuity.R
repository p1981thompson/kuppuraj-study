library(lme4)
library(plyr)

  N=84
  nsim<-500 
  
  expdat_rd<-expand.grid(Type=factor(1:5),SetInd_c=unique(main.data2$SetInd_c),ID=factor(1:N))
  
  #Recode into familiar factors for Kuppu/DB hypotheses.
  #expdat$Type<-as.numeric(expdat$Type)
  
  expdat_rd$Type<-car::recode(expdat_rd$Type,"1='Non_D';2='Adj_D';3='Non_P';4='Adj_P';5='rand'")
  
  expdat_rd$Type<-factor(expdat_rd$Type,levels=c("rand","Adj_D", "Non_D", "Adj_P", "Non_P"))
  
  expdat_rd$broke1<-ifelse(expdat_rd$SetInd_c %in% unique(main.data2$SetInd_c)[1:24],1,0) 
expdat_rd$broke2<-ifelse(expdat_rd$SetInd_c %in% unique(main.data2$SetInd_c)[33:40],1,0) 

  
  #Extract parameters for simulating data. (artificially reduce the effects of setInd and Type to see if power is working. Are the estimates reasonable)
  newparams_rd <- list(
    beta = getME(mod1d2,"beta"), #fixed effects (intercept, Block, TPPR)
    theta = getME(mod1d2, "theta"),#Random slope
    sigma = getME(mod1d2, "sigma"))#random slope variance
  
  bp1=0.3896708 #cutpoint 1
bp2=1.082419 #cutpoint 2
#
b1 <- function(x, bp1) ifelse(x < bp1, bp1 - x, 0)
b2 <- function(x, bp1, bp2) ifelse(x >= bp1 & x < bp2, x - bp1, 0)
b3 <- function(x, bp2) ifelse(x < bp2, 0, x - bp2)
  
  # Simulate:
  
  ss <- simulate(~ Type + b1(SetInd_c, bp1) + b2(SetInd_c, bp1,bp2) + b3(SetInd_c,bp2) + Type*b1(SetInd_c, bp1) + Type*b2(SetInd_c, bp1,bp2) + Type*b3(SetInd_c, bp2) +(broke1+broke2+b1(SetInd_c, bp1) + b2(SetInd_c, bp1,bp2) + b3(SetInd_c, bp2)| ID), nsim = nsim, newdata = expdat_rd, newparams = newparams_rd, family=gaussian)
  

  #####################################################################
  
  
  fitsim3 <- function(i) {
    #print(paste0("iteration",i))
    if(i %% 10==0) {
      # Print on the screen some message
      cat(paste0("iteration: ", i, "\n"))
    }
    
    expdat_rd$TargetRT <- ss[, i]
    new1<- lmer(TargetRT ~ Type + b1(SetInd_c, bp1) + b2(SetInd_c, bp1,bp2) + b3(SetInd_c,bp2) + Type*b1(SetInd_c, bp1) + Type*b2(SetInd_c, bp1,bp2) + Type*b3(SetInd_c, bp2) +(broke1+broke2+b1(SetInd_c, bp1) + b2(SetInd_c, bp1,bp2) + b3(SetInd_c, bp2)| ID), data = expdat_rd, REML = TRUE)
    coef1<-coef(summary(new1))
 
    return(c(int1.coef=coef1["TypeAdj_D:b1(SetInd, bp1)",5],int2.coef=coef1["TypeAdj_P:b1(SetInd, bp1)", 5],int3.coef=coef1["TypeNon_D:b1(SetInd, bp1)", 5],int4.coef=coef1["TypeNon_P:b1(SetInd, bp1) ", 5]))
  }
  
  fitAll3 <- laply(seq(nsim), function(i) fitsim3(i))
  
  fitAll3A<-as.data.frame(fitAll3)
  
  int1_pwr1<-mean(unlist(fitAll3A[,1])< 0.05)

  int2_pwr1<-mean(unlist(fitAll3A[,2])< 0.05)
  
  int3_pwr1<-mean(unlist(fitAll3A[,3])< 0.05)
  
  int4_pwr1<-mean(unlist(fitAll3A[,4])< 0.05)
  
  pwrs<-c(int1_pwr1=int1_pwr1,int2_pwr1=int2_pwr1,int3_pwr1=int3_pwr1,int4_pwr1=int4_pwr1)
  pwrs
