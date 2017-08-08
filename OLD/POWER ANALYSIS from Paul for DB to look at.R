  #################################################################################
  #
  # Power calculation for linear mixed effects model (kuppu) - version 6
  #
  #################################################################################
  
  # Created by Paul Thompson, 12-05-2017
  
  
  #################################################################################
  #install.packages(c("devtools","nlme","car","lmerTest"))
  
  #library(devtools)
  #install_github("lme4", user = "lme4")
  library(lme4)
  library(car)
  library(lmerTest)
  
  #run model for Kuppu's pilot data using NLME and LME4. Need both as lme function produces p-values but lmer does not.
  
  mixed3<-lmer(RT~ Block + Dis_TP + Block:Dis_TP + (1+Block|ID), data=mm_data)
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
  newparams <- list(
    beta = getME(mixed3,"beta"), #fixed effects (intercept, Block, TPPR)
    theta = getME(mixed3, "theta"),#Random slope
    sigma = getME(mixed3, "sigma"))#random slope variance
  
  newparams2<-structure(list(beta = c(0.3, -0.2, 
                                      -0.1, -0.1, -0.1, -0.1, 
                                      0.16, 0.16, -0.16, 0.16), theta = structure(c(0.945877531114169, 0.133090048245979, 
                                                                               0.207882178463665), .Names = c("ID.(Intercept)", "ID.Block.(Intercept)", 
                                                                                                              "ID.Block")), sigma = 0.688988861940449), .Names = c("beta", 
                                                                                                                                                                   "theta", "sigma"))
  
  
  # Simulate:
  
  ss <- simulate(~ Block + Dis_TP + Block:Dis_TP + (1+Block|ID), nsim = nsim, newdata = expdat, newparams = newparams2,family=gaussian)#syntax "block-1" means we don't use random intercepts for block.
  
  
  
  #####################################################################
  
  
  fitsim3 <- function(i) {
    #print(paste0("iteration",i))
    if(i %% 10==0) {
      # Print on the screen some message
      cat(paste0("iteration: ", i, "\n"))
    }
    
    expdat$RT <- ss[, i]
    new1<-lmer(RT~ Block + Dis_TP + Block:Dis_TP +(1+Block|ID),data=expdat, REML = FALSE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
    coef1<-coef(summary(new1))
  #  CI<-confint.merMod(new1,quiet = TRUE,method="Wald")
  #  i.new<-findInterval(0, c(CI["Block:Dis_TPadjD",][[1]],CI["Block:Dis_TPadjD",][[2]]))
  #  i.report<-ifelse(i.new==1,0,1)
    #
  #  i.new2<-findInterval(0, c(CI["Block:Dis_TPnonD",][[1]],CI["Block:Dis_TPnonD",][[2]]))
  #  i.report2<-ifelse(i.new2==1,0,1)
    #
  #  i.new3<-findInterval(0, c(CI["Block:Dis_TPadjP",][[1]],CI["Block:Dis_TPadjP",][[2]]))
  #  i.report3<-ifelse(i.new3==1,0,1)
    #
  #  i.new4<-findInterval(0, c(CI["Block:Dis_TPnonP",][[1]],CI["Block:Dis_TPnonP",][[2]]))
  #  i.report4<-ifelse(i.new4==1,0,1)
    
   #print(c(coef1["Block",5],i.report,coef1["TPPR", 5],i.report2)) 
    #return(c(int1.coef=coef1["Block:Dis_TPadjD",5],CI_count_int1=i.report,int2.coef=coef1["Block:Dis_TPnonD", 5],CI_count_int2=i.report2,int3.coef=coef1["Block:Dis_TPadjP", 5],CI_count_int3=i.report3,int4.coef=coef1["Block:Dis_TPnonP", 5],CI_count_int4=i.report4))
    return(c(int1.coef=coef1["Block:Dis_TPadjD",5],int2.coef=coef1["Block:Dis_TPnonD", 5],int3.coef=coef1["Block:Dis_TPadjP", 5],int4.coef=coef1["Block:Dis_TPnonP", 5]))
  }
  
  fitAll3 <- laply(seq(nsim), function(i) fitsim3(i))
  
  fitAll3A<-as.data.frame(fitAll3)
  
  int1_pwr1<-mean(unlist(fitAll3A[,1])< 0.05)
  #int1_pwr2<-mean(unlist(fitAll3A[,2]))
  
  int2_pwr1<-mean(unlist(fitAll3A[,2])< 0.05)
  #TPPR_pwr2<-mean(unlist(fitAll3A[,4]))
  
  int3_pwr1<-mean(unlist(fitAll3A[,3])< 0.05)
  
  int4_pwr1<-mean(unlist(fitAll3A[,4])< 0.05)
  
  pwrs<-c(int1_pwr1=int1_pwr1,int2_pwr1=int2_pwr1,int3_pwr1=int3_pwr1,int4_pwr1=int4_pwr1)
  pwrs
