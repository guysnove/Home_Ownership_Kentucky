# Question 2 R Code

#Import the data
housing = read.table("C:/Users/guysn/OneDrive/Documents/Data Science Major/Spring 2022/Catagorical DA/Project HomeOwnership/sta327project_sample2.csv", header=TRUE, sep=",")

#Change HHINCOME to 1000's for easier coefficients
housing$hhincome2 = housing$hhincome/1000
housing$incomecat2 = housing$incomecat/1000


# Dependent Variable - indownership
# Indpeendent Variables - hhincome , agequant , famsize , indmarriagestatus , indpoverty , indsex , inddegree

# Bullet Point 1

# Logit
log.inc = glm(formula = indownership ~ hhincome2, data=housing, family = binomial(link=logit), na.action=na.exclude, control=list(epsilon=0.0001,maxit=50,trace=T))
log.age = glm(formula = indownership ~ agequant, data=housing, family = binomial(link=logit), na.action=na.exclude, control=list(epsilon=0.0001,maxit=50,trace=T))
log.fam =  glm(formula = indownership ~ famsize, data=housing, family = binomial(link=logit), na.action=na.exclude, control=list(epsilon=0.0001,maxit=50,trace=T))

log.inc
log.age
log.fam

# Probit
prob.inc = glm(formula = indownership ~ hhincome2, data=housing, family = binomial(link=probit), na.action=na.exclude, control=list(epsilon=0.0001,maxit=50,trace=T))
prob.age = glm(formula = indownership ~ agequant, data=housing, family = binomial(link=probit), na.action=na.exclude, control=list(epsilon=0.0001,maxit=50,trace=T))
prob.fam =  glm(formula = indownership ~ famsize, data=housing, family = binomial(link=probit), na.action=na.exclude, control=list(epsilon=0.0001,maxit=50,trace=T))

prob.inc
prob.age
prob.fam

# Cloglog
clog.inc = glm(formula = indownership ~ hhincome2, data=housing, family = binomial(link=cloglog), na.action=na.exclude, control=list(epsilon=0.0001,maxit=50,trace=T))
clog.age = glm(formula = indownership ~ agequant, data=housing, family = binomial(link=cloglog), na.action=na.exclude, control=list(epsilon=0.0001,maxit=50,trace=T))
clog.fam =  glm(formula = indownership ~ famsize, data=housing, family = binomial(link=cloglog), na.action=na.exclude, control=list(epsilon=0.0001,maxit=50,trace=T))

clog.inc
clog.age
clog.fam

# All 3 Models For Income

log.inc
prob.inc
clog.inc

library(nlme)
housing2<-data.frame(ownership=housing$indownership, incomerange=housing$incomecat2)
housing.sum<-gsummary(object=housing2, FUN=sum, groups=housing$incomecat2)
housing.length<-gsummary(object=housing2, FUN = length, groups=housing$incomecat2)
prop<-housing.sum$ownership/housing.length$ownership
housing.pattern<-data.frame(y=housing.sum$ownership, n=housing.length$ownership, prop=prop, incomerange=housing.sum$incomerange)
housing.pattern[1:10,]


logit.pred = log.inc$coefficients[1]+log.inc$coefficients[2]*seq(from=-10, to=1200, by=1)
logit.phat = exp(logit.pred)/(1+exp(logit.pred))
logit.phat

probit.pred = prob.inc$coefficients[1]+prob.inc$coefficients[2]*seq(from=-10, to=1200, by=1)
probit.phat = pnorm(probit.pred,0,1)
probit.phat

cloglog.pred = clog.inc$coefficients[1]+clog.inc$coefficients[2]*seq(from=-10, to=1200, by=1)
cloglog.phat = 1-exp(-exp(cloglog.pred))
cloglog.phat

hhincome=-10:1200
symbols(x=housing.pattern$incomerange, y=housing.pattern$prop, circles=(housing.pattern$n)^(.25), xlab = "Income Level (in thousands of dollars)", ylab= "Estimated Probability", xlim=c(-100,1200), ylim=c(0,1.2), main="Estimated Probability of Owning a Home")
lines(hhincome, logit.phat, lty=1, col="dodgerblue", lwd=3)
lines(hhincome, probit.phat, lty=2, col="darkseagreen", lwd=3)
lines(hhincome, cloglog.phat, lty=4, col="coral", lwd=3)
names=c("Logit","Probit","Cloglog")
legend(locator(1), legend=names, lty=c(1,2,4), lwd=c(3,3,3), col=c("dodgerblue","darkseagreen","coral"), bty="n", cex=1.5)

# Bullet Point 2

# Model Building

mod<-function(form, data)  {
  mod.fit<-glm(formula=form, data=data, family=binomial(link=logit), na.action=na.exclude, 
               control=list(epsilon=0.0001, maxit=50, trace=F))
  list(p=round(1-pchisq(mod.fit$null.deviance-mod.fit$deviance, mod.fit$df.null-
                          mod.fit$df.residual),4))
}

# hhincome , agequant , famsize , indmarriagestatus , indpoverty , indsex , inddegree


inc = mod(form=indownership~hhincome2, data=housing)
age = mod(form=indownership~agequant, data=housing)
fam = mod(form=indownership~famsize, data=housing)
marst = mod(form=indownership~indmarriagestatus, data=housing)
pov = mod(form=indownership~indpoverty, data=housing)
sex = mod(form=indownership~indsex, data=housing)
deg = mod(form=indownership~inddegree, data=housing)

data.frame(inc$p, age$p, fam$p, marst$p, pov$p, sex$p, deg$p)

# Sex is not significant at the 0.20 level, everything else is

# Backwards Selection

back1<-glm(formula= indownership~hhincome2+agequant+famsize+indmarriagestatus+indpoverty+inddegree, data=housing, family=binomial(link=logit), na.action=na.exclude, 
           control=list(epsilon=0.0001, maxit=50, trace=F))
summary(back1)
sum.back1<-summary(back1)
#OR CI

#Income by 10k

alpha<-0.05
lowerI<-10*back1$coefficients[2]-qnorm(1-alpha/2)*10*sqrt(sum.back1$cov.unscaled[2,2])
upperI<-10*back1$coefficients[2]+qnorm(1-alpha/2)*10*sqrt(sum.back1$cov.unscaled[2,2])
data.frame(theta.hat=10*back1$coefficients[2], lowerI, upperI)

lowerORI<-exp(lowerI)
SampORdist<-exp(back1$coefficients[2])
upperORI<-exp(upperI)
data.frame(SampORdist, lowerORI, upperORI)

#Age by 5 years

lowerA<-5*back1$coefficients[3]-qnorm(1-alpha/2)*5*sqrt(sum.back1$cov.unscaled[3,3])
upperA<-5*back1$coefficients[3]+qnorm(1-alpha/2)*5*sqrt(sum.back1$cov.unscaled[3,3])
data.frame(theta.hat=5*back1$coefficients[3], lowerA, upperA)

lowerORA<-exp(lowerA)
SampORdist<-exp(back1$coefficients[3])
upperORA<-exp(upperA)
data.frame(SampORdist, lowerORA, upperORA)

#Family size by 1

alpha<-0.05
lowerF<-back1$coefficients[4]-qnorm(1-alpha/2)*sqrt(sum.back1$cov.unscaled[4,4])
upperF<-back1$coefficients[4]+qnorm(1-alpha/2)*sqrt(sum.back1$cov.unscaled[4,4])
data.frame(beta.hat=back1$coefficients[4], lowerF, upperF)

lowerORF<-exp(lowerF)
SampORdist<-exp(back1$coefficients[4])
upperORF<-exp(upperF)
data.frame(SampORdist, lowerORF, upperORF)
#Marriage status

alpha<-0.05
lowerM<-back1$coefficients[5]-qnorm(1-alpha/2)*sqrt(sum.back1$cov.unscaled[5,5])
upperM<-back1$coefficients[5]+qnorm(1-alpha/2)*sqrt(sum.back1$cov.unscaled[5,5])
data.frame(beta.hat=back1$coefficients[5], lowerM, upperM)

lowerORM<-exp(lowerM)
SampORdist<-exp(back1$coefficients[5])
upperORM<-exp(upperM)
data.frame(SampORdist, lowerORM, upperORM)
#Poverty

alpha<-0.05
lowerP<-back1$coefficients[6]-qnorm(1-alpha/2)*sqrt(sum.back1$cov.unscaled[6,6])
upperP<-back1$coefficients[6]+qnorm(1-alpha/2)*sqrt(sum.back1$cov.unscaled[6,6])
data.frame(beta.hat=back1$coefficients[6], lowerP, upperP)

lowerORP<-exp(lowerP)
SampORdist<-exp(back1$coefficients[6])
upperORP<-exp(upperP)
data.frame(1/SampORdist, 1/upperORP, 1/lowerORP)
#(Inverted)

#Degree

alpha<-0.05
lowerD<-back1$coefficients[7]-qnorm(1-alpha/2)*sqrt(sum.back1$cov.unscaled[7,7])
upperD<-back1$coefficients[7]+qnorm(1-alpha/2)*sqrt(sum.back1$cov.unscaled[7,7])
data.frame(beta.hat=back1$coefficients[7], lowerD, upperD)

lowerORD<-exp(lowerD)
SampORdist<-exp(back1$coefficients[7])
upperORD<-exp(upperD)
data.frame(SampORdist, lowerORD, upperORD)

#Predictions

obsI<-c(15,50,90,36,78,78)
obsA<-c(20,23,30,40,40,40)
obsF<-c(1,1,1,1,2,4)
obsM<-c(0,0,0,0,1,1)
obsP<-c(1,0,0,0,0,0)
obsD<-c(0,1,1,1,1,1)

predict.data<-data.frame(hhincome2=obsI,agequant=obsA, famsize=obsF, indmarriagestatus=obsM, indpoverty=obsP, inddegree=obsD)
save.lp.hat<- predict(object=back1, newdata=predict.data, type="link", se=TRUE)
save.lp.hat

lower.lp<-save.lp.hat$fit-qnorm(1-alpha/2)*save.lp.hat$se
upper.lp<-save.lp.hat$fit+qnorm(1-alpha/2)*save.lp.hat$se
lower<-exp(lower.lp)/(1+exp(lower.lp))
upper<-exp(upper.lp)/(1+exp(upper.lp))
data.frame(predict.data, lp.hat=round(save.lp.hat$fit, 4), lower.lp=round(lower.lp,4), upper.lp=round(upper.lp,4), pi.hat=exp(save.lp.hat$fit)/(1+exp(save.lp.hat$fit)), lower=round(lower,4), upper=round(upper,4))
