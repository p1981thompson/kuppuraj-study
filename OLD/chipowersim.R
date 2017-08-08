require(pwr)

#assume 4 conditions with 2 x 2 for distance (1 or 2) and probability (1 or 2)
# 16 possible patterns for D1P1-D1P2-D2P1-D2P2
mygroupN <-25 #N per group
myNsim<-1
myprobs<-matrix(c(.9,.8, #Prob of passing D1P1 for adult and child
                  .8,.7, #Prob of passing D1P2
                  .8,.4, #Prob of passing D2P1
                  .7,.2),ncol=2)  #Prob of passing D2P2
thispatt<-matrix(rep(0,mygroupN*2),nrow=mygroupN) #initialise matrix to  hold N each pattern by group
totsum<-data.frame(matrix(rep(0,32),nrow=16)) #initialise frame to hold summary data
rownames(totsum)<-c(0000,0001,0010,0011,0100,0101,0110,0111,1000,1001,1010,1011,1100,1101,1110,1111)
for (j in 1:2){#2 groups
for (i in 1:mygroupN){
  for (mycond in 1:4){
  
    critprob<-myprobs[mycond,j]
     if (runif(1)<critprob){thispatt[i,j]<-thispatt[i,j]+(mycond-1)^2}
    #using logic of binary system here!
  }
  thisindex<-thispatt[i,j]+1
  totsum[thisindex,j]<- totsum[thisindex,j]+1
    }
}
totsum$allpatt<-totsum[,1]+totsum[,2] #how many subjects with this pattern
mynodata<-which(totsum$allpatt==0) #find patterns with no data
forchi<-totsum[-mynodata,1:2] #create table with only patterns with 1+ case
chisq.test(forchi)

#the power test for chi square uses omegahat for effect size
# This is described here:
#http://www.statmethods.net/stats/power.html
# I'm confused by this as it looks like square root of chi square,
# but it can't be as it takes values 0 to 1

pwr.chisq.test(w=NULL,N=50,df=nrow(forchi)-1,sig.level=.05,power=.8)

