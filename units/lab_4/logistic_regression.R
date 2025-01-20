
require(foreign)
require(MASS)
library(here)
library(tidyverse)

readin <- read.dta(here::here(
  "data", "gssCum7212Teach.dta"
  ), convert.factor=F
)
  

# create list of variable names used for variable selection below
usevar <- c("hlthc2", "age", "educ", "impinc", "female")
# subset the data (select variables)
mydta <- subset(readin[complete.cases(readin[usevar]),],
                select=c(hlthc2, age, educ, impinc, female))
lpm <- lm(hlthc2 ~
            age + educ + impinc + female, data = mydta)
summary(lpm)$coefficients

plot(mydta$educ, jitter(mydta$hlthc2, 0.3), xlab="Education", ylab="Health")
abline(lm(mydta$hlthc2 ~ mydta$educ))


mylogit <- glm(hlthc2 ~
                 age + female + educ + impinc,
               data=mydta, family=binomial(link="logit"))
summary(mylogit)$coefficients

zval <- (coef(mylogit)[2] - 0 )/ sqrt(vcov(mylogit)[2,2])
pval <- 2*pnorm(abs(zval), lower.tail=FALSE)
c(zval=zval, pval=pval)

library(car)
linearHypothesis(mylogit, c("age=0"))
Anova(mylogit, test='Wald')


mylogit.prob = predict(mylogit, type="response")
library(ROCR)
mylogit.pred <- prediction(mylogit.prob, mydta$hlthc2)
mylogit.perf <- performance(mylogit.pred,"tpr","fpr")
mylogit.auc <- performance(mylogit.pred, measure = "auc")
mylogit.auc@y.values[[1]]



x.vector = data.frame(age=35,
                      female=1,
                      educ = 16,
                      impinc = mean(mydta$impinc))
x.vector

prob = predict(mylogit, newdata=x.vector, type="response")
prob

link = predict(mylogit, newdata=x.vector, type="link", se.fit=TRUE)
zval = qnorm(0.975)
link.lb = link$fit - zval*link$se.fit
link.ub = link$fit + zval*link$se.fit


invLogit <- function(input){
  invLogit <- 1/(1 + exp(-1*input))
  return(invLogit)
}

prob.lb = invLogit(link.lb)
prob.ub = invLogit(link.ub)
print(c(prob.lb, prob.ub), digits=3)

print(cbind(x.vector, prob.lb, prob, prob.ub))







noAgeEduc.prob = predict(noAgeEduc, type="response")
noAgeEduc.pred <- prediction(noAgeEduc.prob, mydta$hlthc2)
noAgeEduc.perf <- performance(noAgeEduc.pred,"tpr","fpr")
noAgeEduc.auc <- performance(noAgeEduc.pred , measure = "auc")
noAgeEduc.auc@y.values[[1]]