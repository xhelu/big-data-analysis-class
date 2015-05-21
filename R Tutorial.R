library(xlsx)

sampledata <- read.xlsx("/Users/xousaenhelu/git repositories/IPython-Big-Data/R-tutorial/sampledata.xlsx", 1)
View(sampledata)

sampledata2 <- read.table("/Users/xousaenhelu/git repositories/IPython-Big-Data/R-tutorial/sampledata.txt", header=T, sep=",")
View(sampledata2)

data()

ToothGrowth

View(ToothGrowth)
plot(ToothGrowth$len)
len <- ToothGrowth$len
plot(len)

hist(len)
hist(len, breaks=20)
plot(dose)
plot(dose, len)
dose <- ToothGrowth$dose
plot(dose, len)
plot(dose, len, xlab="Vitamin C dose", ylab="tooth length", main="tooth length vs. Vit C dose", las=1)

var.test(len~supp, data=ToothGrowth)

cor(women$weight, women$height)
cor(women$weight, women$height, use="complete.obs")
cor.test(women$weight, women$height)
cor.test(women$weight, women$height, method="spearman")

View(sleep)
t.test(sleep$extra~sleep$group,paired=TRUE)

View(Titanic)
install.packages("prettyR")
library(prettyR)
xtabs(Freq~Class + Sex + Age + Survived, data=Titanic)
xtabs(Freq~Sex + Survived, data=Titanic)

survivaltable <- xtabs(Freq~Sex + Survived, data=Titanic)
survivaltable
titanictable <- survivaltable
install.packages("gmodels")
library(gmodels)
CrossTable(xtabs(Freq~Sex + Survived, data=Titanic))
CrossTable(survivaltable)
chisq.test(titanictable)

withage<- xtabs(Freq~Sex + Survived + Age, data=Titanic)
withage
mantelhaen.test(withage)

View(beaver1)
View(beaver2)
t.test(beaver1,beaver2, var.equal=TRUE)

t.test(len~ToothGrowth$supp)

View(USArrests)
t.test(USArrests$Murder, mu=10, data=USArrests)

supp <- ToothGrowth$supp

t.test(len~supp, alternative='less')
t.test(len~supp, alternative='greater')
t.test(len~supp, conf.level=.99)

toothgrowth.anova <- aov(len ~ factor(dose), data=ToothGrowth)
summary (toothgrowth.anova)
TukeyHSD(toothgrowth.anova)

toothlength <- lm(len ~ dose + supp, data=ToothGrowth)
summary(toothlength)
confint(toothlength, level=0.95)
fitted(toothlength)
residuals(toothlength)

layout(matrix(c(1,2,3,4),2,2))
plot(toothlength)

View(infert)

baby<- glm(case~age+induced+parity+spontaneous, data=infert, family=binomial())
summary(baby)
confint(baby)
exp(coef(baby))
exp(confint(baby))

hist(infert$spontaneous)
mean(infert$spontaneous)
var(infert$spontaneous)

preg <- glm(spontaneous ~ age + parity + induced, data=infert, family=poisson())
summary(preg)
exp(coef(preg))
install.packages(“survival”) 
library(survival)

hiv_file <- "/Users/xousaenhelu/Downloads/hmohiv.csv"
hiv <- read.table(hiv_file, sep=",", header=TRUE)
summary(hiv)
View(hiv)
km <- with(Surv(time,censor), data=hiv)
hiv.km <- survfit(km~1, data=hiv)
summary(hiv.km)
layout
plot(hiv.km, xlab='Time in Months', ylab='Survival Probability', main='Kaplan Meier Survival Probability Plot', las=1)

hiv.drug <- survfit(km~drug, data=hiv)
summary(hiv.drug)
plot(hiv.drug, col=c(2,4), lty=c(1,2), xlab="Time in Months", ylab="Survival Probability", main="Kaplan Meier Survival Plot by Drug", las=1)
legend(40, .9, lty=c(1,2), col=c(2,4), bty="n", legend=c("Drug = 0", "Drug = 1"))
survdiff(km~drug, data=hiv)

table(hiv$censor)
hist(hiv$age)
table(hiv$drug)

hiv.base <- coxph(formula = Surv(time, censor)~ drug, data=hiv)
summary(hiv.base)

hiv.full <- coxph(formula = Surv(time, censor) ~ drug + age, data=hiv)
summary(hiv.full)

hiv.age5 <- coxph(formula=Surv(time,censor)~drug+I(age/5), data=hiv)
summary(hiv.age5)

legend(40, .9, lty=c(1,2), col=c(2,4), bty="n", legend=c("Drug = 0", "Drug = 1"))
cox.zph(hiv.full)

interact <-coxph(formula = Surv(time, censor) ~ drug + drug:time + age, data = hiv)
summary(interact)
mean(hiv$time)
centered <-coxph(formula = Surv(time, censor) ~ drug + drug:I(time-11.36) + age, data = hiv)
summary(centered)

hiv$age2 <- (hiv$age*hiv$age)
hiv.line <- coxph(Surv(time,censor)~drug + age + age2, data=hiv)
summary(hiv.line)
sort(hiv$age)


install.packages("pwr")
library(pwr)

pwr.t.test(n = , d =.5 , sig.level =.05 , power =.9 , type = c("two.sample")) 
pwr.t.test(n = , d =.5 , sig.level =.05 , power =.9 , type = c("one.sample")) 
pwr.t.test(n = , d =.5 , sig.level =.05 , power =.9 , type = c("paired"))

pwr.t2n.test(n1 = 10 , n2=40 , d = .5 , sig.level = .05, power = )
pwr.chisq.test(w = .3, N = , df = 1 , sig.level = .05, power = .9 )

pwr.anova.test(k = 4, n = , f = .5, sig.level = .05, power = .9)
pwr.r.test(n = , r = .7, sig.level = .05, power =.9 )
pwr.2p.test(h = .3, n = , sig.level =.05, power =.9 )
pwr.p.test(h = .3, n = , sig.level =.05, power =.9 )
