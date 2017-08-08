
setwd("C:/Users/pthompson/Dropbox/SL and WM paper/Revision/Pilot_Data_Revised task/Version_2")


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

detaildata<- summaryBy(TargetRT ~ SetInd+Type,  data=RWdata,FUN=c(median), na.rm=TRUE)

detaildata$ID<-rep(RWdata$ID[4],length(detaildata[,1]))

names(detaildata)<-c("SetInd", "Type", "TargetRT", "ID")

main.data<-rbind(main.data,detaildata)
}

main.data$ID<-as.factor(main.data$ID)

#28/07/2017
main.data.t1<-subset(main.data,SetInd<=25)
main.data.t2<-subset(main.data,SetInd>25&SetInd<=32)
main.data.t3<-subset(main.data,SetInd>32)
mod1 <- lmer(TargetRT ~ Type + SetInd + Type*SetInd + (SetInd | ID), data = main.data.t1, REML = FALSE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
mod2 <- lmer(TargetRT ~ Type + SetInd + Type*SetInd + (SetInd | ID), data = main.data.t2, REML = FALSE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
mod3 <- lmer(TargetRT ~ Type + SetInd + Type*SetInd + (SetInd | ID), data = main.data.t3, REML = FALSE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
# ggplot(main.data, aes(x = SetInd, y = TargetRT,group=ID)) +
# geom_point(shape = 21,alpha=0.1) +
# geom_vline(aes(xintercept = 25), color = 'grey', size = 1, linetype = 'dashed') +
# geom_vline(aes(xintercept = 33), color = 'grey', size = 1, linetype = 'dashed') +
# geom_smooth(data=main.data.t1,aes(y=TargetRT,x=SetInd,color=ID),method="lm",se=F)+
# geom_smooth(data=main.data.t2,aes(y=TargetRT,x=SetInd,color=ID),method="lm",se=F)+
# geom_smooth(data=main.data.t3,aes(y=TargetRT,x=SetInd,color=ID),method="lm",se=F)+theme_bw()+facet_grid(~ Condition_type)
#
##################################################################

ggplot(main.data, aes(x = SetInd, y = TargetRT)) +
geom_point(shape = 21,alpha=0.5) +
geom_vline(aes(xintercept = 25), color = 'grey', size = 1, linetype = 'dashed') +
geom_vline(aes(xintercept = 33), color = 'grey', size = 1, linetype = 'dashed') +
geom_line()+
theme_bw()+facet_grid(ID~Type)


#do it for all conditions
all_cond=pirateplot(formula = TargetRT ~ Set + Type,
data = main.data,
xlab = "Type",
ylab = "RT",
main = "all_participants/all_conditions")

#do it by condition
con_2_data=subset(main.data, main.data$Type=='Adj_P')
cond_2_AP=pirateplot(formula = TargetRT ~ Set+Type,
data = con_2_data,
xlab = "Type",
ylab = "RT",
main = "all_participants/AP")

con_1_data=subset(main.data, main.data$Type=='Adj_D')

cond_1_AD=pirateplot(formula = TargetRT ~ Set+Type,
data = con_1_data,
xlab = "Type",
ylab = "RT",
main = "all_participants/AD")

con_3_data=subset(main.data, main.data$Type=='Non_D')

cond_3_ND=pirateplot(formula = TargetRT ~ Set+Type,
data = con_3_data,
xlab = "Type",
ylab = "RT",
main = "all_participants/ND")

con_4_data=subset(main.data, main.data$Type=='Non_P')

cond_4_NP=pirateplot(formula = TargetRT ~ Set+Type,
data = con_4_data,
xlab = "Type",
ylab = "RT",
main = "all_participants/NP")

con_5_data=subset(main.data, main.data$Type=='rand')

cond_5_RR=pirateplot(formula = TargetRT ~ Set+Type,
data = con_5_data,
xlab = "Type",
ylab = "RT",
main = "all_participants/R")

#############################################################################
windows(record=T)
par(mfrow=c(5,1))
library(psych)

#dat002
dat_002<-read.csv("002.csv")

hist(dat_002$TargetRT[dat_002$SetInd<=8 & !dat_002$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_002$TargetRT[dat_002$SetInd>8 & dat_002$SetInd<=16 & !dat_002$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_002$TargetRT[dat_002$SetInd>16 & dat_002$SetInd<=24 & !dat_002$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_002$TargetRT[dat_002$SetInd>24 & dat_002$SetInd<=32 & !dat_002$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_002$TargetRT[dat_002$SetInd>32 & dat_002$SetInd<=40 & !dat_002$Type==19],xlim=c(0,2500),breaks=40)

describe(dat_002$TargetRT[dat_002$SetInd<=8 & !dat_002$Type==19])
describe(dat_002$TargetRT[dat_002$SetInd>8 & dat_002$SetInd<=16 & !dat_002$Type==19])
describe(dat_002$TargetRT[dat_002$SetInd>16 & dat_002$SetInd<=24 & !dat_002$Type==19])
describe(dat_002$TargetRT[dat_002$SetInd>24 & dat_002$SetInd<=32 & !dat_002$Type==19])
describe(dat_002$TargetRT[dat_002$SetInd>32 & dat_002$SetInd<=40 & !dat_002$Type==19])
##

#dat003
dat_003<-read.csv("003.csv")

hist(dat_003$TargetRT[dat_003$SetInd<=8 & !dat_003$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_003$TargetRT[dat_003$SetInd>8 & dat_003$SetInd<=16 & !dat_003$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_003$TargetRT[dat_003$SetInd>16 & dat_003$SetInd<=24 & !dat_003$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_003$TargetRT[dat_003$SetInd>24 & dat_003$SetInd<=32 & !dat_003$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_003$TargetRT[dat_003$SetInd>32 & dat_003$SetInd<=40 & !dat_003$Type==19],xlim=c(0,2500),breaks=40)


describe(dat_003$TargetRT[dat_003$SetInd<=8 & !dat_003$Type==19])
describe(dat_003$TargetRT[dat_003$SetInd>8 & dat_003$SetInd<=16 & !dat_003$Type==19])
describe(dat_003$TargetRT[dat_003$SetInd>16 & dat_003$SetInd<=24 & !dat_003$Type==19])
describe(dat_003$TargetRT[dat_003$SetInd>24 & dat_003$SetInd<=32 & !dat_003$Type==19])
describe(dat_003$TargetRT[dat_003$SetInd>32 & dat_003$SetInd<=40 & !dat_003$Type==19])


#dat004
dat_004<-read.csv("004.csv")

hist(dat_004$TargetRT[dat_004$SetInd<=8 & !dat_004$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_004$TargetRT[dat_004$SetInd>8 & dat_004$SetInd<=16 & !dat_004$Type==19],xlim=c(0,2500),breaks=60)
hist(dat_004$TargetRT[dat_004$SetInd>16 & dat_004$SetInd<=24 & !dat_004$Type==19],xlim=c(0,2500),breaks=50)
hist(dat_004$TargetRT[dat_004$SetInd>24 & dat_004$SetInd<=32 & !dat_004$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_004$TargetRT[dat_004$SetInd>32 & dat_004$SetInd<=40 & !dat_004$Type==19],xlim=c(0,2500),breaks=80)

describe(dat_004$TargetRT[dat_004$SetInd<=8 & !dat_004$Type==19])
describe(dat_004$TargetRT[dat_004$SetInd>8 & dat_004$SetInd<=16 & !dat_004$Type==19])
describe(dat_004$TargetRT[dat_004$SetInd>16 & dat_004$SetInd<=24 & !dat_004$Type==19])
describe(dat_004$TargetRT[dat_004$SetInd>24 & dat_004$SetInd<=32 & !dat_004$Type==19])
describe(dat_004$TargetRT[dat_004$SetInd>32 & dat_004$SetInd<=40 & !dat_004$Type==19])


#dat005
dat_005<-read.csv("005.csv")

hist(dat_005$TargetRT[dat_005$SetInd<=8 & !dat_005$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_005$TargetRT[dat_005$SetInd>8 & dat_005$SetInd<=16 & !dat_005$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_005$TargetRT[dat_005$SetInd>16 & dat_005$SetInd<=24 & !dat_005$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_005$TargetRT[dat_005$SetInd>24 & dat_005$SetInd<=32 & !dat_005$Type==19],xlim=c(0,2500),breaks=50)
hist(dat_005$TargetRT[dat_005$SetInd>32 & dat_005$SetInd<=40 & !dat_005$Type==19],xlim=c(0,2500),breaks=200)

describe(dat_005$TargetRT[dat_005$SetInd<=8 & !dat_005$Type==19])
describe(dat_005$TargetRT[dat_005$SetInd>8 & dat_005$SetInd<=16 & !dat_005$Type==19])
describe(dat_005$TargetRT[dat_005$SetInd>16 & dat_005$SetInd<=24 & !dat_005$Type==19])
describe(dat_005$TargetRT[dat_005$SetInd>24 & dat_005$SetInd<=32 & !dat_005$Type==19])
describe(dat_005$TargetRT[dat_005$SetInd>32 & dat_005$SetInd<=40 & !dat_005$Type==19])


#dat006
dat_006<-read.csv("006.csv")

hist(dat_006$TargetRT[dat_006$SetInd<=8 & !dat_006$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_006$TargetRT[dat_006$SetInd>8 & dat_006$SetInd<=16 & !dat_006$Type==19],xlim=c(0,2500),breaks=50)
hist(dat_006$TargetRT[dat_006$SetInd>16 & dat_006$SetInd<=24 & !dat_006$Type==19],xlim=c(0,2500),breaks=30)
hist(dat_006$TargetRT[dat_006$SetInd>24 & dat_006$SetInd<=32 & !dat_006$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_006$TargetRT[dat_006$SetInd>32 & dat_006$SetInd<=40 & !dat_006$Type==19],xlim=c(0,2500),breaks=50)

describe(dat_006$TargetRT[dat_006$SetInd<=8 & !dat_006$Type==19])
describe(dat_006$TargetRT[dat_006$SetInd>8 & dat_006$SetInd<=16 & !dat_006$Type==19])
describe(dat_006$TargetRT[dat_006$SetInd>16 & dat_006$SetInd<=24 & !dat_006$Type==19])
describe(dat_006$TargetRT[dat_006$SetInd>24 & dat_006$SetInd<=32 & !dat_006$Type==19])
describe(dat_006$TargetRT[dat_006$SetInd>32 & dat_006$SetInd<=40 & !dat_006$Type==19])



#dat007
dat_007<-read.csv("007.csv")

hist(dat_007$TargetRT[dat_007$SetInd<=8 & !dat_007$Type==19],xlim=c(0,2500),breaks=40)
hist(dat_007$TargetRT[dat_007$SetInd>8 & dat_007$SetInd<=16 & !dat_007$Type==19],xlim=c(0,2500),breaks=50)
hist(dat_007$TargetRT[dat_007$SetInd>16 & dat_007$SetInd<=24 & !dat_007$Type==19],xlim=c(0,2500),breaks=70)
hist(dat_007$TargetRT[dat_007$SetInd>24 & dat_007$SetInd<=32 & !dat_007$Type==19],xlim=c(0,2500),breaks=50)
hist(dat_007$TargetRT[dat_007$SetInd>32 & dat_007$SetInd<=40 & !dat_007$Type==19],xlim=c(0,2500),breaks=50)

describe(dat_007$TargetRT[dat_007$SetInd<=8 & !dat_007$Type==19])
describe(dat_007$TargetRT[dat_007$SetInd>8 & dat_007$SetInd<=16 & !dat_007$Type==19])
describe(dat_007$TargetRT[dat_007$SetInd>16 & dat_007$SetInd<=24 & !dat_007$Type==19])
describe(dat_007$TargetRT[dat_007$SetInd>24 & dat_007$SetInd<=32 & !dat_007$Type==19])
describe(dat_007$TargetRT[dat_007$SetInd>32 & dat_007$SetInd<=40 & !dat_007$Type==19])

##############################################################################################
##############################################################################################
##############################################################################################


#############################################################################



#dat002

dat_002$Type[dat_002$Type==1]<- "Adj_D"
dat_002$Type[dat_002$Type==2]<- "Adj_D"
dat_002$Type[dat_002$Type==3]<- "Adj_D"
dat_002$Type[dat_002$Type==4]<- "Adj_P"
dat_002$Type[dat_002$Type==5]<- "Adj_P"
dat_002$Type[dat_002$Type==6]<- "Adj_P"
dat_002$Type[dat_002$Type==7]<- "Adj_P"
dat_002$Type[dat_002$Type==8]<- "Adj_P"
dat_002$Type[dat_002$Type==9]<- "Adj_P"
dat_002$Type[dat_002$Type==10]<- "Non_D"
dat_002$Type[dat_002$Type==11]<- "Non_D"
dat_002$Type[dat_002$Type==12]<- "Non_D"
dat_002$Type[dat_002$Type==13]<- "Non_P"
dat_002$Type[dat_002$Type==14]<- "Non_P"
dat_002$Type[dat_002$Type==15]<- "Non_P"
dat_002$Type[dat_002$Type==16]<- "Non_P"
dat_002$Type[dat_002$Type==17]<- "Non_P"
dat_002$Type[dat_002$Type==18]<- "Non_P"
dat_002$Type[dat_002$Type==19]<- "rand"

#dat003

dat_003$Type[dat_003$Type==1]<- "Adj_D"
dat_003$Type[dat_003$Type==2]<- "Adj_D"
dat_003$Type[dat_003$Type==3]<- "Adj_D"
dat_003$Type[dat_003$Type==4]<- "Adj_P"
dat_003$Type[dat_003$Type==5]<- "Adj_P"
dat_003$Type[dat_003$Type==6]<- "Adj_P"
dat_003$Type[dat_003$Type==7]<- "Adj_P"
dat_003$Type[dat_003$Type==8]<- "Adj_P"
dat_003$Type[dat_003$Type==9]<- "Adj_P"
dat_003$Type[dat_003$Type==10]<- "Non_D"
dat_003$Type[dat_003$Type==11]<- "Non_D"
dat_003$Type[dat_003$Type==12]<- "Non_D"
dat_003$Type[dat_003$Type==13]<- "Non_P"
dat_003$Type[dat_003$Type==14]<- "Non_P"
dat_003$Type[dat_003$Type==15]<- "Non_P"
dat_003$Type[dat_003$Type==16]<- "Non_P"
dat_003$Type[dat_003$Type==17]<- "Non_P"
dat_003$Type[dat_003$Type==18]<- "Non_P"
dat_003$Type[dat_003$Type==19]<- "rand"

#dat004

dat_004$Type[dat_004$Type==1]<- "Adj_D"
dat_004$Type[dat_004$Type==2]<- "Adj_D"
dat_004$Type[dat_004$Type==3]<- "Adj_D"
dat_004$Type[dat_004$Type==4]<- "Adj_P"
dat_004$Type[dat_004$Type==5]<- "Adj_P"
dat_004$Type[dat_004$Type==6]<- "Adj_P"
dat_004$Type[dat_004$Type==7]<- "Adj_P"
dat_004$Type[dat_004$Type==8]<- "Adj_P"
dat_004$Type[dat_004$Type==9]<- "Adj_P"
dat_004$Type[dat_004$Type==10]<- "Non_D"
dat_004$Type[dat_004$Type==11]<- "Non_D"
dat_004$Type[dat_004$Type==12]<- "Non_D"
dat_004$Type[dat_004$Type==13]<- "Non_P"
dat_004$Type[dat_004$Type==14]<- "Non_P"
dat_004$Type[dat_004$Type==15]<- "Non_P"
dat_004$Type[dat_004$Type==16]<- "Non_P"
dat_004$Type[dat_004$Type==17]<- "Non_P"
dat_004$Type[dat_004$Type==18]<- "Non_P"
dat_004$Type[dat_004$Type==19]<- "rand"

#dat005

dat_005$Type[dat_005$Type==1]<- "Adj_D"
dat_005$Type[dat_005$Type==2]<- "Adj_D"
dat_005$Type[dat_005$Type==3]<- "Adj_D"
dat_005$Type[dat_005$Type==4]<- "Adj_P"
dat_005$Type[dat_005$Type==5]<- "Adj_P"
dat_005$Type[dat_005$Type==6]<- "Adj_P"
dat_005$Type[dat_005$Type==7]<- "Adj_P"
dat_005$Type[dat_005$Type==8]<- "Adj_P"
dat_005$Type[dat_005$Type==9]<- "Adj_P"
dat_005$Type[dat_005$Type==10]<- "Non_D"
dat_005$Type[dat_005$Type==11]<- "Non_D"
dat_005$Type[dat_005$Type==12]<- "Non_D"
dat_005$Type[dat_005$Type==13]<- "Non_P"
dat_005$Type[dat_005$Type==14]<- "Non_P"
dat_005$Type[dat_005$Type==15]<- "Non_P"
dat_005$Type[dat_005$Type==16]<- "Non_P"
dat_005$Type[dat_005$Type==17]<- "Non_P"
dat_005$Type[dat_005$Type==18]<- "Non_P"
dat_005$Type[dat_005$Type==19]<- "rand"


#dat006

dat_006$Type[dat_006$Type==1]<- "Adj_D"
dat_006$Type[dat_006$Type==2]<- "Adj_D"
dat_006$Type[dat_006$Type==3]<- "Adj_D"
dat_006$Type[dat_006$Type==4]<- "Adj_P"
dat_006$Type[dat_006$Type==5]<- "Adj_P"
dat_006$Type[dat_006$Type==6]<- "Adj_P"
dat_006$Type[dat_006$Type==7]<- "Adj_P"
dat_006$Type[dat_006$Type==8]<- "Adj_P"
dat_006$Type[dat_006$Type==9]<- "Adj_P"
dat_006$Type[dat_006$Type==10]<- "Non_D"
dat_006$Type[dat_006$Type==11]<- "Non_D"
dat_006$Type[dat_006$Type==12]<- "Non_D"
dat_006$Type[dat_006$Type==13]<- "Non_P"
dat_006$Type[dat_006$Type==14]<- "Non_P"
dat_006$Type[dat_006$Type==15]<- "Non_P"
dat_006$Type[dat_006$Type==16]<- "Non_P"
dat_006$Type[dat_006$Type==17]<- "Non_P"
dat_006$Type[dat_006$Type==18]<- "Non_P"
dat_006$Type[dat_006$Type==19]<- "rand"

#dat007

dat_007$Type[dat_007$Type==1]<- "Adj_D"
dat_007$Type[dat_007$Type==2]<- "Adj_D"
dat_007$Type[dat_007$Type==3]<- "Adj_D"
dat_007$Type[dat_007$Type==4]<- "Adj_P"
dat_007$Type[dat_007$Type==5]<- "Adj_P"
dat_007$Type[dat_007$Type==6]<- "Adj_P"
dat_007$Type[dat_007$Type==7]<- "Adj_P"
dat_007$Type[dat_007$Type==8]<- "Adj_P"
dat_007$Type[dat_007$Type==9]<- "Adj_P"
dat_007$Type[dat_007$Type==10]<- "Non_D"
dat_007$Type[dat_007$Type==11]<- "Non_D"
dat_007$Type[dat_007$Type==12]<- "Non_D"
dat_007$Type[dat_007$Type==13]<- "Non_P"
dat_007$Type[dat_007$Type==14]<- "Non_P"
dat_007$Type[dat_007$Type==15]<- "Non_P"
dat_007$Type[dat_007$Type==16]<- "Non_P"
dat_007$Type[dat_007$Type==17]<- "Non_P"
dat_007$Type[dat_007$Type==18]<- "Non_P"
dat_007$Type[dat_007$Type==19]<- "rand"

type_choice<-c("Adj_D","Adj_P","Non_D","Non_P")

windows(record=T)
par(mfrow=c(5,1))

for(i in 1:4)
{

hist(dat_002$TargetRT[dat_002$SetInd<=8 & !dat_002$Type=="rand" & dat_002$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("002: ",type_choice[i]," block 1"),xlab="RT")
hist(dat_002$TargetRT[dat_002$SetInd>8 & dat_002$SetInd<=16 & !dat_002$Type==19 & dat_002$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("002: ",type_choice[i]," block 2"),xlab="RT")
hist(dat_002$TargetRT[dat_002$SetInd>16 & dat_002$SetInd<=24 & !dat_002$Type==19 & dat_002$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("002: ",type_choice[i]," block 3"),xlab="RT")
hist(dat_002$TargetRT[dat_002$SetInd>24 & dat_002$SetInd<=32 & !dat_002$Type==19 & dat_002$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("002: ",type_choice[i]," block 4"),xlab="RT")
hist(dat_002$TargetRT[dat_002$SetInd>32 & dat_002$SetInd<=40 & !dat_002$Type==19 & dat_002$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("002: ",type_choice[i]," block 5"),xlab="RT")

##

#dat003

hist(dat_003$TargetRT[dat_003$SetInd<=8 & !dat_003$Type==19 & dat_003$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("003: ",type_choice[i]," block 1"),xlab="RT")
hist(dat_003$TargetRT[dat_003$SetInd>8 & dat_003$SetInd<=16 & !dat_003$Type==19 & dat_003$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("003: ",type_choice[i]," block 2"),xlab="RT")
hist(dat_003$TargetRT[dat_003$SetInd>16 & dat_003$SetInd<=24 & !dat_003$Type==19 & dat_003$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("003: ",type_choice[i]," block 3"),xlab="RT")
hist(dat_003$TargetRT[dat_003$SetInd>24 & dat_003$SetInd<=32 & !dat_003$Type==19 & dat_003$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("003: ",type_choice[i]," block 4"),xlab="RT")
hist(dat_003$TargetRT[dat_003$SetInd>32 & dat_003$SetInd<=40 & !dat_003$Type==19 & dat_003$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("003: ",type_choice[i]," block 5"),xlab="RT")

#dat004

hist(dat_004$TargetRT[dat_004$SetInd<=8 & !dat_004$Type==19 & dat_004$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("004: ",type_choice[i]," block 1"),xlab="RT")
hist(dat_004$TargetRT[dat_004$SetInd>8 & dat_004$SetInd<=16 & !dat_004$Type==19 & dat_004$Type==type_choice[i]],xlim=c(0,2500),breaks=60,main=paste0("004: ",type_choice[i]," block 2"),xlab="RT")
hist(dat_004$TargetRT[dat_004$SetInd>16 & dat_004$SetInd<=24 & !dat_004$Type==19 & dat_004$Type==type_choice[i]],xlim=c(0,2500),breaks=50,main=paste0("004: ",type_choice[i]," block 3"),xlab="RT")
hist(dat_004$TargetRT[dat_004$SetInd>24 & dat_004$SetInd<=32 & !dat_004$Type==19 & dat_004$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("004: ",type_choice[i]," block 4"),xlab="RT")
hist(dat_004$TargetRT[dat_004$SetInd>32 & dat_004$SetInd<=40 & !dat_004$Type==19 & dat_004$Type==type_choice[i]],xlim=c(0,2500),breaks=80,main=paste0("004: ",type_choice[i]," block 5"),xlab="RT")


#dat005

hist(dat_005$TargetRT[dat_005$SetInd<=8 & !dat_005$Type==19 & dat_005$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("005: ",type_choice[i]," block 1"),xlab="RT")
hist(dat_005$TargetRT[dat_005$SetInd>8 & dat_005$SetInd<=16 & !dat_005$Type==19 & dat_005$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("005: ",type_choice[i]," block 2"),xlab="RT")
hist(dat_005$TargetRT[dat_005$SetInd>16 & dat_005$SetInd<=24 & !dat_005$Type==19 & dat_005$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("005: ",type_choice[i]," block 3"),xlab="RT")
hist(dat_005$TargetRT[dat_005$SetInd>24 & dat_005$SetInd<=32 & !dat_005$Type==19 & dat_005$Type==type_choice[i]],xlim=c(0,2500),breaks=50,main=paste0("005: ",type_choice[i]," block 4"),xlab="RT")
hist(dat_005$TargetRT[dat_005$SetInd>32 & dat_005$SetInd<=40 & !dat_005$Type==19 & dat_005$Type==type_choice[i]],xlim=c(0,2500),breaks=200,main=paste0("005: ",type_choice[i]," block 5"),xlab="RT")


#dat006

hist(dat_006$TargetRT[dat_006$SetInd<=8 & !dat_006$Type==19 & dat_006$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("006: ",type_choice[i]," block 1"),xlab="RT")
hist(dat_006$TargetRT[dat_006$SetInd>8 & dat_006$SetInd<=16 & !dat_006$Type==19 & dat_006$Type==type_choice[i]],xlim=c(0,2500),breaks=50,main=paste0("006: ",type_choice[i]," block 2"),xlab="RT")
hist(dat_006$TargetRT[dat_006$SetInd>16 & dat_006$SetInd<=24 & !dat_006$Type==19 & dat_006$Type==type_choice[i]],xlim=c(0,2500),breaks=30,main=paste0("006: ",type_choice[i]," block 3"),xlab="RT")
hist(dat_006$TargetRT[dat_006$SetInd>24 & dat_006$SetInd<=32 & !dat_006$Type==19 & dat_006$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("006: ",type_choice[i]," block 4"),xlab="RT")
hist(dat_006$TargetRT[dat_006$SetInd>32 & dat_006$SetInd<=40 & !dat_006$Type==19 & dat_006$Type==type_choice[i]],xlim=c(0,2500),breaks=50,main=paste0("006: ",type_choice[i]," block 5"),xlab="RT")


#dat007

hist(dat_007$TargetRT[dat_007$SetInd<=8 & !dat_007$Type==19 & dat_007$Type==type_choice[i]],xlim=c(0,2500),breaks=40,main=paste0("007: ",type_choice[i]," block 1"),xlab="RT")
hist(dat_007$TargetRT[dat_007$SetInd>8 & dat_007$SetInd<=16 & !dat_007$Type==19 & dat_007$Type==type_choice[i]],xlim=c(0,2500),breaks=50,main=paste0("007: ",type_choice[i]," block 2"),xlab="RT")
hist(dat_007$TargetRT[dat_007$SetInd>16 & dat_007$SetInd<=24 & !dat_007$Type==19 & dat_007$Type==type_choice[i]],xlim=c(0,2500),breaks=70,main=paste0("007: ",type_choice[i]," block 3"),xlab="RT")
hist(dat_007$TargetRT[dat_007$SetInd>24 & dat_007$SetInd<=32 & !dat_007$Type==19 & dat_007$Type==type_choice[i]],xlim=c(0,2500),breaks=50,main=paste0("007: ",type_choice[i]," block 4"),xlab="RT")
hist(dat_007$TargetRT[dat_007$SetInd>32 & dat_007$SetInd<=40 & !dat_007$Type==19 & dat_007$Type==type_choice[i]],xlim=c(0,2500),breaks=50,main=paste0("007: ",type_choice[i]," block 5"),xlab="RT")

}


############################################################################################

#Density


windows(record=T)
par(mfrow=c(5,1))

for(i in 1:4)
{
  
  plot(density(dat_002$TargetRT[dat_002$SetInd<=8 & !dat_002$Type=="rand" & dat_002$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("002: ",type_choice[i]," block 1"),xlab="RT")
  abline(v=median(dat_002$TargetRT[dat_002$SetInd<=8 & !dat_002$Type=="rand" & dat_002$Type==type_choice[i]]),col="red")
  plot(density(dat_002$TargetRT[dat_002$SetInd>8 & dat_002$SetInd<=16 & !dat_002$Type==19 & dat_002$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("002: ",type_choice[i]," block 2"),xlab="RT")
  abline(v=median(dat_002$TargetRT[dat_002$SetInd>8 & dat_002$SetInd<=16 & !dat_002$Type==19 & dat_002$Type==type_choice[i]]),col="red")
  plot(density(dat_002$TargetRT[dat_002$SetInd>16 & dat_002$SetInd<=24 & !dat_002$Type==19 & dat_002$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("002: ",type_choice[i]," block 3"),xlab="RT")
  abline(v=median(dat_002$TargetRT[dat_002$SetInd>16 & dat_002$SetInd<=24 & !dat_002$Type==19 & dat_002$Type==type_choice[i]]),col="red")
  plot(density(dat_002$TargetRT[dat_002$SetInd>24 & dat_002$SetInd<=32 & !dat_002$Type==19 & dat_002$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("002: ",type_choice[i]," block 4"),xlab="RT")
  abline(v=median(dat_002$TargetRT[dat_002$SetInd>24 & dat_002$SetInd<=32 & !dat_002$Type==19 & dat_002$Type==type_choice[i]]),col="red")
  plot(density(dat_002$TargetRT[dat_002$SetInd>32 & dat_002$SetInd<=40 & !dat_002$Type==19 & dat_002$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("002: ",type_choice[i]," block 5"),xlab="RT")
  abline(v=median(dat_002$TargetRT[dat_002$SetInd>32 & dat_002$SetInd<=40 & !dat_002$Type==19 & dat_002$Type==type_choice[i]]),col="red")
  ##
  
  #dat003
  
  plot(density(dat_003$TargetRT[dat_003$SetInd<=8 & !dat_003$Type==19 & dat_003$Type==type_choice[i]]),xlim=c(0,2500),breaks=40,main=paste0("003: ",type_choice[i]," block 1"),xlab="RT")
  abline(v=median(dat_003$TargetRT[dat_003$SetInd<=8 & !dat_003$Type==19 & dat_003$Type==type_choice[i]]),col="red")
  plot(density(dat_003$TargetRT[dat_003$SetInd>8 & dat_003$SetInd<=16 & !dat_003$Type==19 & dat_003$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("003: ",type_choice[i]," block 2"),xlab="RT")
  abline(v=median(dat_003$TargetRT[dat_003$SetInd>8 & dat_003$SetInd<=16 & !dat_003$Type==19 & dat_003$Type==type_choice[i]]),col="red")
  plot(density(dat_003$TargetRT[dat_003$SetInd>16 & dat_003$SetInd<=24 & !dat_003$Type==19 & dat_003$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("003: ",type_choice[i]," block 3"),xlab="RT")
  abline(v=median(dat_003$TargetRT[dat_003$SetInd>16 & dat_003$SetInd<=24 & !dat_003$Type==19 & dat_003$Type==type_choice[i]]),col="red")
  plot(density(dat_003$TargetRT[dat_003$SetInd>24 & dat_003$SetInd<=32 & !dat_003$Type==19 & dat_003$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("003: ",type_choice[i]," block 4"),xlab="RT")
  abline(v=median(dat_003$TargetRT[dat_003$SetInd>24 & dat_003$SetInd<=32 & !dat_003$Type==19 & dat_003$Type==type_choice[i]]),col="red")
  plot(density(dat_003$TargetRT[dat_003$SetInd>32 & dat_003$SetInd<=40 & !dat_003$Type==19 & dat_003$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("003: ",type_choice[i]," block 5"),xlab="RT")
  abline(v=median(dat_003$TargetRT[dat_003$SetInd>32 & dat_003$SetInd<=40 & !dat_003$Type==19 & dat_003$Type==type_choice[i]]),col="red")
  
  #dat004
  
  plot(density(dat_004$TargetRT[dat_004$SetInd<=8 & !dat_004$Type==19 & dat_004$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("004: ",type_choice[i]," block 1"),xlab="RT")
  abline(v=median(dat_004$TargetRT[dat_004$SetInd<=8 & !dat_004$Type==19 & dat_004$Type==type_choice[i]]),col="red")
  plot(density(dat_004$TargetRT[dat_004$SetInd>8 & dat_004$SetInd<=16 & !dat_004$Type==19 & dat_004$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("004: ",type_choice[i]," block 2"),xlab="RT")
  abline(v=median(dat_004$TargetRT[dat_004$SetInd>8 & dat_004$SetInd<=16 & !dat_004$Type==19 & dat_004$Type==type_choice[i]]),col="red")
  plot(density(dat_004$TargetRT[dat_004$SetInd>16 & dat_004$SetInd<=24 & !dat_004$Type==19 & dat_004$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("004: ",type_choice[i]," block 3"),xlab="RT")
  abline(v=median(dat_004$TargetRT[dat_004$SetInd>16 & dat_004$SetInd<=24 & !dat_004$Type==19 & dat_004$Type==type_choice[i]]),col="red")
  plot(density(dat_004$TargetRT[dat_004$SetInd>24 & dat_004$SetInd<=32 & !dat_004$Type==19 & dat_004$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("004: ",type_choice[i]," block 4"),xlab="RT")
  abline(v=median(dat_004$TargetRT[dat_004$SetInd>24 & dat_004$SetInd<=32 & !dat_004$Type==19 & dat_004$Type==type_choice[i]]),col="red")
  plot(density(dat_004$TargetRT[dat_004$SetInd>32 & dat_004$SetInd<=40 & !dat_004$Type==19 & dat_004$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("004: ",type_choice[i]," block 5"),xlab="RT")
  abline(v=median(dat_004$TargetRT[dat_004$SetInd>32 & dat_004$SetInd<=40 & !dat_004$Type==19 & dat_004$Type==type_choice[i]]),col="red")
  
  #dat005
  
  plot(density(dat_005$TargetRT[dat_005$SetInd<=8 & !dat_005$Type==19 & dat_005$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("005: ",type_choice[i]," block 1"),xlab="RT")
  abline(v=median(dat_005$TargetRT[dat_005$SetInd<=8 & !dat_005$Type==19 & dat_005$Type==type_choice[i]]),col="red")
  plot(density(dat_005$TargetRT[dat_005$SetInd>8 & dat_005$SetInd<=16 & !dat_005$Type==19 & dat_005$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("005: ",type_choice[i]," block 2"),xlab="RT")
  abline(v=median(dat_005$TargetRT[dat_005$SetInd>8 & dat_005$SetInd<=16 & !dat_005$Type==19 & dat_005$Type==type_choice[i]]),col="red")
  plot(density(dat_005$TargetRT[dat_005$SetInd>16 & dat_005$SetInd<=24 & !dat_005$Type==19 & dat_005$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("005: ",type_choice[i]," block 3"),xlab="RT")
  abline(v=median(dat_005$TargetRT[dat_005$SetInd>16 & dat_005$SetInd<=24 & !dat_005$Type==19 & dat_005$Type==type_choice[i]]),col="red")
  plot(density(dat_005$TargetRT[dat_005$SetInd>24 & dat_005$SetInd<=32 & !dat_005$Type==19 & dat_005$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("005: ",type_choice[i]," block 4"),xlab="RT")
  abline(v=median(dat_005$TargetRT[dat_005$SetInd>24 & dat_005$SetInd<=32 & !dat_005$Type==19 & dat_005$Type==type_choice[i]]),col="red")
  plot(density(dat_005$TargetRT[dat_005$SetInd>32 & dat_005$SetInd<=40 & !dat_005$Type==19 & dat_005$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("005: ",type_choice[i]," block 5"),xlab="RT")
  abline(v=median(dat_005$TargetRT[dat_005$SetInd>32 & dat_005$SetInd<=40 & !dat_005$Type==19 & dat_005$Type==type_choice[i]]),col="red")
  
  #dat006
  
  plot(density(dat_006$TargetRT[dat_006$SetInd<=8 & !dat_006$Type==19 & dat_006$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("006: ",type_choice[i]," block 1"),xlab="RT")
  abline(v=median(dat_006$TargetRT[dat_006$SetInd<=8 & !dat_006$Type==19 & dat_006$Type==type_choice[i]]),col="red")
  plot(density(dat_006$TargetRT[dat_006$SetInd>8 & dat_006$SetInd<=16 & !dat_006$Type==19 & dat_006$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("006: ",type_choice[i]," block 2"),xlab="RT")
  abline(v=median(dat_006$TargetRT[dat_006$SetInd>8 & dat_006$SetInd<=16 & !dat_006$Type==19 & dat_006$Type==type_choice[i]]),col="red")
  plot(density(dat_006$TargetRT[dat_006$SetInd>16 & dat_006$SetInd<=24 & !dat_006$Type==19 & dat_006$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("006: ",type_choice[i]," block 3"),xlab="RT")
  abline(v=median(dat_006$TargetRT[dat_006$SetInd>16 & dat_006$SetInd<=24 & !dat_006$Type==19 & dat_006$Type==type_choice[i]]),col="red")
  plot(density(dat_006$TargetRT[dat_006$SetInd>24 & dat_006$SetInd<=32 & !dat_006$Type==19 & dat_006$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("006: ",type_choice[i]," block 4"),xlab="RT")
  abline(v=median(dat_006$TargetRT[dat_006$SetInd>24 & dat_006$SetInd<=32 & !dat_006$Type==19 & dat_006$Type==type_choice[i]]),col="red")
  plot(density(dat_006$TargetRT[dat_006$SetInd>32 & dat_006$SetInd<=40 & !dat_006$Type==19 & dat_006$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("006: ",type_choice[i]," block 5"),xlab="RT")
  abline(v=median(dat_006$TargetRT[dat_006$SetInd>32 & dat_006$SetInd<=40 & !dat_006$Type==19 & dat_006$Type==type_choice[i]]),col="red")
  
  #dat007
  
  plot(density(dat_007$TargetRT[dat_007$SetInd<=8 & !dat_007$Type==19 & dat_007$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("007: ",type_choice[i]," block 1"),xlab="RT")
  abline(v=median(dat_007$TargetRT[dat_007$SetInd<=8 & !dat_007$Type==19 & dat_007$Type==type_choice[i]]),col="red")
  plot(density(dat_007$TargetRT[dat_007$SetInd>8 & dat_007$SetInd<=16 & !dat_007$Type==19 & dat_007$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("007: ",type_choice[i]," block 2"),xlab="RT")
  abline(v=median(dat_007$TargetRT[dat_007$SetInd>8 & dat_007$SetInd<=16 & !dat_007$Type==19 & dat_007$Type==type_choice[i]]),col="red")
  plot(density(dat_007$TargetRT[dat_007$SetInd>16 & dat_007$SetInd<=24 & !dat_007$Type==19 & dat_007$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("007: ",type_choice[i]," block 3"),xlab="RT")
  abline(v=median(dat_007$TargetRT[dat_007$SetInd>16 & dat_007$SetInd<=24 & !dat_007$Type==19 & dat_007$Type==type_choice[i]]),col="red")
  plot(density(dat_007$TargetRT[dat_007$SetInd>24 & dat_007$SetInd<=32 & !dat_007$Type==19 & dat_007$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("007: ",type_choice[i]," block 4"),xlab="RT")
  abline(v=median(dat_007$TargetRT[dat_007$SetInd>24 & dat_007$SetInd<=32 & !dat_007$Type==19 & dat_007$Type==type_choice[i]]),col="red")
  plot(density(dat_007$TargetRT[dat_007$SetInd>32 & dat_007$SetInd<=40 & !dat_007$Type==19 & dat_007$Type==type_choice[i]]),xlim=c(0,2500),main=paste0("007: ",type_choice[i]," block 5"),xlab="RT")
  abline(v=median(dat_007$TargetRT[dat_007$SetInd>32 & dat_007$SetInd<=40 & !dat_007$Type==19 & dat_007$Type==type_choice[i]]),col="red")
}