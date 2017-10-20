
library(lme4)

w=NULL
s=NULL
ew=NULL
es=NULL
design=NULL
wp=NULL
for (i in 1:100)
{
  #whole plot type
  tw=floor(i/34)
  #specific whole plot error, each whole plot is N(0,2)
  tew=rnorm(1,0,2) 
  for (j in 1:5)
  {   #whole plot type
    w=rbind(w,tw) 
    
    s=rbind(s,j)
    ew=rbind(ew,tew)      
    es=rbind(es,rnorm(1,0,3))
    #whole plot type is 0,1,2 but each 0 needs to be distinct and is label wp.
    wp=rbind(i,wp)
  }
}

#mdoel is wp(0,1,2)+ew(n(0,2^2) changes every 5 rows)+s(1,2,3,4,5)+es(n(0,3^2) changes with each new row)
design=cbind(wp,w,ew,s,es)
#print(design)
y=w+ew+s+es



mbig=lmer(y~s+w+(1|wp), REML=FALSE)
msmall=lmer(y~s+(1|wp), REML=FALSE)
testcase=lmer(y~design[,c(2,4)]+(1|wp),REML=FALSE)

#this is the code to use likelihood theory to perform the test instead of REML. See REML = False above.
anova(mbig,msmall)
drop1(mbig,test="Chisq")


mresult

