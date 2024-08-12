### Panel Data Examples 
####Relation between beer tax and fatalities
# install.packages("plm")
library(plm)
# install.packages("AER")
library(AER)
# install.packages('pder')
library(pder)
?Fatalities

data("Fatalities", package = "AER")
Fatalities$frate = Fatalities$fatal/Fatalities$pop*10000
fm = frate~beertax

pool = plm(fm, data= Fatalities,model = 'pooling')
fixed = plm(fm, data= Fatalities,model = 'within')
Random = plm(fm, data= Fatalities,model = 'random')

summary(pool)
summary(fixed)
summary(Random)

# pooldata = c(coef(summary(pool))['beertax',c('Estimate','Std. Error', 't-value')])
# pooldata = coef(summary(pool))['beertax',c('Estimate','Std. Error')]


P.models = list(Pool=pool,Fixed = fixed,Random = Random)
sapply(P.models,function(x)coef(summary(x))['beertax',c('Estimate','Std. Error', 't-value')])
sapply(P.models, function(x)summary(x)$r.squared[2])


pFtest(fixed,pool)


plmtest(pool,effect = 'individual')
plmtest(pool,effect = 'time')
plmtest(pool,effect = 'twoways')

phtest(fixed ,Random)

#############################################

fm= frate ~beertax+year

pool = plm(fm, data= Fatalities,model = 'pooling')
fixed = plm(fm, data= Fatalities,model = 'within')
Between = plm(fm, data= Fatalities,model = 'between')

summary(pool)
summary(fixed)
summary(Between)

P.models = list(Pool=pool,Fixed = fixed,Between = Between)
sapply(P.models,function(x)coef(summary(x))['beertax',c('Estimate','Std. Error', 't-value')])
sapply(P.models, function(x)summary(x)$r.squared[2])


pFtest(fixed,pool)
pFtest(Between,pool)


plmtest(pool,effect = 'individual')
plmtest(pool,effect = 'time')
plmtest(pool,effect = 'twoways')

phtest(fixed ,Between)


###############Tiobin's q Dataset with PLM package

?TobinQ

data(TobinQ)
class(TobinQ)
head(TobinQ)
pTobinQ =pdata.frame(TobinQ, index = c('cusip','year'))
head(pTobinQ)
dim(TobinQ)
pdim(pTobinQ)

Qeq =ikn~qn

ercomp(Qeq,data = pTobinQ)

Q.pooling =plm(Qeq,pTobinQ, model = 'pooling')
Q.fixed =plm(Qeq,pTobinQ, model = 'within')
Q.between =plm(Qeq,pTobinQ, model = 'between')

Q.models = list(Pooling=Q.pooling,Fixed=Q.fixed,Between=Q.between)


summary(Q.pooling)
sapply(Q.models,function(x)coef(summary(x))['qn',c('Estimate','Std. Error', 't-value')])
sapply(Q.models, function(x)summary(x)$r.squared[2])
##################Quantile Regression with r
install.packages("quantreg")
library(quantreg)

data("mtcars")

rqfit= rq(mpg~wt,data = mtcars)
summary(rqfit)
summary(rqfit, se='iid')
rqfit= rq(mpg~wt,data = mtcars, tau = .8)
summary(rqfit, se='iid')
summary(rqfit, se='ker')
summary(rqfit, se='boot')



seq(0,1,0.05)
rqfit= rq(mpg~wt,data = mtcars, tau = seq(0,1,0.05))

plot(rqfit)
