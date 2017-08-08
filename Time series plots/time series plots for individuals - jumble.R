#pilot data analysis and tentative analysis script for the learning task
setwd("C:/Users/pthompson/Dropbox/SL and WM paper/Revision/Pilot_Data_Revised task/Version_2/more_participants")
#required packages

install.packages("optimx")
library(optimx)
library(lme4)
library(ggplot2)
library(doBy)

namelist=c('002','003','004','005','006','007')#,'008','009' )
nsubs=length(namelist)

main.data<-data.frame(ID=factor(), SetInd=integer(), Type=integer(), TargetRT=integer())

for (i in 1:nsubs){
  
  myname=namelist[i]
  mycsv=paste(myname,".csv",sep="")
  mydata= read.csv(mycsv)  # read csv file 
  #get rid of RTs of inaacurate responses and any RT greater than 3000 ms:replace with NA. 
  Rwdata=mydata
  rawdata=Rwdata[ ,c(1,10,12,26)]
  #in the original analysis the following section will be removed 
  
  #######this bit ensures inaccurate becomes, so not eliminated in next section, and at least RT can be extracted. 
  rawdata$TargetRT[rawdata$Type==19 & rawdata$TargetRT>3000]<-2999
  ##############
  rawdata$TargetRT[rawdata$TargetACC==0]<-NA
  rawdata$TargetRT[rawdata$TargetRT<100]<-NA
  rawdata$TargetRT[rawdata$TargetRT>3000]<-NA
  
  RWdata<-rawdata
  
  #rename the types so median can be taken for each block 
  RWdata$Type[RWdata$Type==1]<- "Adj_D"
  RWdata$Type[RWdata$Type==2]<- "Adj_D"
  RWdata$Type[RWdata$Type==3]<- "Adj_D"
  RWdata$Type[RWdata$Type==4]<- "Adj_P"
  RWdata$Type[RWdata$Type==5]<- "Adj_P"
  RWdata$Type[RWdata$Type==6]<- "Adj_P"
  RWdata$Type[RWdata$Type==7]<- "Adj_P"
  RWdata$Type[RWdata$Type==8]<- "Adj_P"
  RWdata$Type[RWdata$Type==9]<- "Adj_P"
  RWdata$Type[RWdata$Type==10]<- "Non_D"
  RWdata$Type[RWdata$Type==11]<- "Non_D"
  RWdata$Type[RWdata$Type==12]<- "Non_D"
  RWdata$Type[RWdata$Type==13]<- "Non_P"
  RWdata$Type[RWdata$Type==14]<- "Non_P"
  RWdata$Type[RWdata$Type==15]<- "Non_P"
  RWdata$Type[RWdata$Type==16]<- "Non_P"
  RWdata$Type[RWdata$Type==17]<- "Non_P"
  RWdata$Type[RWdata$Type==18]<- "Non_P"
  RWdata$Type[RWdata$Type==19]<- "rand"
  
  RWdata$ID<-substring(RWdata$ID,7,7)
  RWdata$Type<-as.factor(RWdata$Type)
  RWdata$Type<-factor(RWdata$Type,levels=c("rand", "Adj_D", "Adj_P", "Non_D", "Non_P"))
  
  detaildata<- summaryBy(TargetRT ~ SetInd+Type,  data=RWdata,
                         FUN=c(median), na.rm=TRUE)
  
  detaildata$ID<-rep(RWdata$ID[4],length(detaildata[,1]))
  
  names(detaildata)<-c("SetInd", "Type", "TargetRT", "ID")
  
  main.data<-rbind(main.data,detaildata)
  
}



main.data$ID<-as.factor(main.data$ID)

main.data.jumble<-main.data

library(car)

# main.data.jumble$SetInd<-recode(main.data.jumble$SetInd,
# "1=9;
# 2=10;
# 3=11;
# 4=12;
# 5=13;
# 6=14;
# 7=15;
# 8=16;
# 9=25;
# 10=26;
# 11=27;
# 12=28;
# 13=29;
# 14=30;
# 15=31;
# 16=32;
# 17=1;
# 18=2;
# 19=3;
# 20=4;
# 21=5;
# 22=6;
# 23=7;
# 24=8;
# 25=33;
# 26=34;
# 27=35;
# 28=36;
# 29=37;
# 30=38;
# 31=39;
# 32=40;
# 33=17;
# 34=18;
# 35=19;
# 36=20;
# 37=21;
# 38=22;
# 39=23;
# 40=24")

#main.data.jumble<-main.data

# main.data.jumble$SetInd<-recode(main.data.jumble$SetInd,
# "1=9;
# 2=10;
# 3=11;
# 4=12;
# 5=13;
# 6=14;
# 7=15;
# 8=16;
# 9=25;
# 10=26;
# 11=27;
# 12=28;
# 13=29;
# 14=30;
# 15=31;
# 16=32;
# 17=33;
# 18=34;
# 19=35;
# 20=36;
# 21=37;
# 22=38;
# 23=39;
# 24=40;
# 25=1;
# 26=2;
# 27=3;
# 28=4;
# 29=5;
# 30=6;
# 31=7;
# 32=8;
# 33=17;
# 34=18;
# 35=19;
# 36=20;
# 37=21;
# 38=22;
# 39=23;
# 40=24")

#main.data.jumble<-main.data

# main.data.jumble$SetInd<-recode(main.data.jumble$SetInd,
# "1=33;
# 2=34;
# 3=35;
# 4=36;
# 5=37;
# 6=38;
# 7=39;
# 8=40;
# 9=1;
# 10=2;
# 11=3;
# 12=4;
# 13=5;
# 14=6;
# 15=7;
# 16=8;
# 17=25;
# 18=26;
# 19=27;
# 20=28;
# 21=29;
# 22=30;
# 23=31;
# 24=32;
# 25=9;
# 26=10;
# 27=11;
# 28=12;
# 29=13;
# 30=14;
# 31=15;
# 32=16;
# 33=17;
# 34=18;
# 35=19;
# 36=20;
# 37=21;
# 38=22;
# 39=23;
# 40=24")

main.data.jumble<-main.data

main.data.jumble$SetInd<-recode(main.data.jumble$SetInd,
"1=17;
2=18;
3=19;
4=20;
5=21;
6=22;
7=23;
8=24;
9=33;
10=34;
11=35;
12=36;
13=37;
14=38;
15=39;
16=40;
17=1;
18=2;
19=3;
20=4;
21=5;
22=6;
23=7;
24=8;
25=25;
26=26;
27=27;
28=28;
29=29;
30=30;
31=31;
32=32;
33=9;
34=10;
35=11;
36=12;
37=13;
38=14;
39=15;
40=16")

  #28/07/2017
  
main.data.jumble.t1<-subset(main.data.jumble,SetInd<=25)
main.data.jumble.t2<-subset(main.data.jumble,SetInd>25&SetInd<=32)
main.data.jumble.t3<-subset(main.data.jumble,SetInd>32)
  
  
  
  mod1B <- lmer(TargetRT ~ Type + SetInd + Type*SetInd + (SetInd | ID), data = main.data.jumble.t1, REML = FALSE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  mod2B <- lmer(TargetRT ~ Type + SetInd + Type*SetInd + (SetInd | ID), data = main.data.jumble.t2, REML = FALSE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  mod3B <- lmer(TargetRT ~ Type + SetInd + Type*SetInd + (SetInd | ID), data = main.data.jumble.t3, REML = FALSE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  
  newdat1.jumble<-expand.grid(Type=unique(main.data.jumble$Type),SetInd=1:24,ID=unique(main.data.jumble$ID))
  newdat2.jumble<-expand.grid(Type=unique(main.data.jumble$Type),SetInd=25:32,ID=unique(main.data.jumble$ID))
  newdat3.jumble<-expand.grid(Type=unique(main.data.jumble$Type),SetInd=33:40,ID=unique(main.data.jumble$ID))
  
  library(RColorBrewer)
  
  ggplot(main.data.jumble, aes(x = SetInd, y = TargetRT,color=Type)) + 
  geom_point(alpha=0.35) + 
  geom_vline(aes(xintercept = 25), color = 'grey', size = 1, linetype = 'dashed') + 
  geom_vline(aes(xintercept = 33), color = 'grey', size = 1, linetype = 'dashed') +
  geom_line(data=newdat1.jumble,aes(y=predict(mod1B,newdata=newdat1.jumble)),size = .75)+
  geom_line(data=newdat2.jumble,aes(y=predict(mod2B,newdata=newdat2.jumble)),size = .75)+
  geom_line(data=newdat3.jumble,aes(y=predict(mod3B,newdata=newdat3.jumble)),size = .75)+theme_bw()+facet_grid(~ID)+ scale_fill_brewer(palette="Set1")+theme(legend.position = "top",strip.text=element_text(size=12),axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))
  


##################################################################

    ggplot(main.data, aes(x = SetInd, y = TargetRT)) + 
      geom_point(shape = 21,alpha=0.5) + 
      geom_vline(aes(xintercept = 25), color = 'grey', size = 1, linetype = 'dashed') + 
      geom_vline(aes(xintercept = 32), color = 'grey', size = 1, linetype = 'dashed') +
      geom_line()+
     theme_bw()+facet_grid(ID~Type)+ylim(0,1500)
    
    
  ggplot(main.data, aes(x = SetInd, y = TargetRT,color=ID)) + 
    geom_point(shape = 21,alpha=0.5) + 
    geom_vline(aes(xintercept = 25), color = 'grey', size = 1, linetype = 'dashed') + 
    geom_vline(aes(xintercept = 32), color = 'grey', size = 1, linetype = 'dashed') +
    geom_line()+
    theme_bw()+facet_grid(~Type)
    
 #########################
    
pdf("c:/Users/pthompson/Desktop/pirate_plots.pdf")
  #do it for all conditions 
  all_cond=pirateplot(formula = TargetRT ~ SetInd + Type,
                      data = main.data,
                      xlab = "Type",
                      ylab = "RT",
                      main = "all_participants/all_conditions")
  
  abline(v=25,col="red",lwd=2)
  abline(v=32,col="red",lwd=2)
  ###
  main.data2<-main.data
  main.data2$SetInd<-as.factor(main.data2$SetInd)
  
  ggplot(main.data2,aes(y=TargetRT,x=SetInd))+geom_boxplot()+facet_grid(~Type)+
  geom_vline(aes(xintercept = 25), color = 'red', size = 1, linetype = 'dashed') + 
    geom_vline(aes(xintercept = 32), color = 'red', size = 1, linetype = 'dashed') +theme_bw()
  
###
  
  #do it by condition 
  con_2_data=subset(main.data, main.data$Type=='Adj_P')
  
  cond_2_AP=pirateplot(formula = TargetRT ~ SetInd+Type,
                       data = con_2_data,
                       xlab = "Type",
                       ylab = "RT",
                       main = "all_participants/AP")
  
  abline(v=25,col="red",lwd=2)
  abline(v=32,col="red",lwd=2)
  
  
  con_1_data=subset(main.data, main.data$Type=='Adj_D')
  
  cond_1_AD=pirateplot(formula = TargetRT ~ SetInd+Type,
                       data = con_1_data,
                       xlab = "Type",
                       ylab = "RT",
                       main = "all_participants/AD")
  
  abline(v=25,col="red",lwd=2)
  abline(v=32,col="red",lwd=2)
  
  
  con_3_data=subset(main.data, main.data$Type=='Non_D')
  
  cond_3_ND=pirateplot(formula = TargetRT ~ SetInd+Type,
                       data = con_3_data,
                       xlab = "Type",
                       ylab = "RT",
                       main = "all_participants/ND")
  
  abline(v=25,col="red",lwd=2)
  abline(v=32,col="red",lwd=2)
  
  
  con_4_data=subset(main.data, main.data$Type=='Non_P')
  
  cond_4_NP=pirateplot(formula = TargetRT ~ SetInd+Type,
                       data = con_4_data,
                       xlab = "Type",
                       ylab = "RT",
                       main = "all_participants/NP")
  
  abline(v=25,col="red",lwd=2)
  abline(v=32,col="red",lwd=2)
  
  
  con_5_data=subset(main.data, main.data$Type=='rand')
  
  cond_5_RR=pirateplot(formula = TargetRT ~ SetInd+Type,
                       data = con_5_data,
                       xlab = "Type",
                       ylab = "RT",
                       main = "all_participants/R")
  
  abline(v=25,col="red",lwd=2)
  abline(v=32,col="red",lwd=2)
  
  dev.off()
  
  
  
  #############################################################
  
  install.packages("forecast")
  library(forecast)
  
  
  windows(record=T)
  pdf("c:/Users/pthompson/Desktop/decompose.pdf")
  my_ids<-as.numeric(names(table(main.data$ID)))
  type_name<-c(names(table(main.data$Type)))
  
  for(i in 1:6){
    for(j in 1:5)
    {
  # trend1<-ma(as.numeric(main.data$TargetRT[main.data$ID==2 & main.data$Type=="Adj_D"]),order = 4, centre = T)
  # plot(as.ts(as.numeric(main.data$TargetRT[main.data$ID==2 & main.data$Type=="Adj_D"])))
  # lines(trend1)
  # 
  # detrend1 = as.ts(as.numeric(main.data$TargetRT[main.data$ID==2 & main.data$Type=="Adj_D"])) - trend1
  # plot(as.ts(detrend1))
  # 
  
  ts1 = ts(as.numeric(main.data$TargetRT[main.data$ID==my_ids[i] & main.data$Type==type_name[j]]), frequency = 2)
  decompose1 = decompose(ts1, "additive")

  plot(decompose1)
    }
  }
  dev.off()
  
  ggplot(main.data, aes(x = SetInd, y = TargetRT)) + 
    geom_point(shape = 21,alpha=0.5) + 
    geom_vline(aes(xintercept = 25), color = 'grey', size = 1, linetype = 'dashed') + 
    geom_vline(aes(xintercept = 32), color = 'grey', size = 1, linetype = 'dashed') +
    geom_line()+
    theme_bw()+facet_grid(ID~Type)+ylim(0,1500)
  
  