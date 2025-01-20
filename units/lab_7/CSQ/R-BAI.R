library(gdata)
library(polycor)
library(psych)
library(psy)
library(lavaan)
library(GPArotation)


#-------------------------------------------------------------------
# read data
#-------------------------------------------------------------------

bai <- read.table("BAI.txt", header=TRUE)

#-------------------------------------------------------------------
# summary of the data
#-------------------------------------------------------------------

cbind(colMeans(bai))
hist(rowSums(bai))


#-------------------------------------------------------------------
# assign the name to the rows and columns of the matrix
#-------------------------------------------------------------------

all.items <- c("bai1","bai2", "bai3", "bai4",              
               "bai5","bai6", "bai7", "bai8",
               "bai9","bai10", "bai11", "bai12",
               "bai13","bai14", "bai15", "bai16",
               "bai17","bai18", "bai19", "bai20",
               "bai21")
               

#-------------------------------------------------------------------
# ... CFA
# four factors model    
#-------------------------------------------------------------------
            
four.factors.model <-  '
             F1 =~ NA*bai4 + bai5 + bai9 + bai10 + bai14 + bai17
             F2 =~ NA*bai1 + bai3 + bai6 + bai8 + bai12 + bai13 + bai19
             F3 =~ NA*bai2 + bai18 + bai20 + bai21
             F4 =~ NA*bai7 + bai11 + bai15 + bai16
                  
             # variances
             F1 ~~ 1*F1
             F2 ~~ 1*F2
             F3 ~~ 1*F3
             F4 ~~ 1*F4
             '

#-------------------------------------------------------------------
# one factor model        
#-------------------------------------------------------------------
        
one.factor.model <-  '
             F1 =~ NA*bai1 + bai2 + bai3 + bai4 + bai5 + bai6 +
                   bai7 + bai8 + bai9 + bai10 + bai11 + bai12 +
                   bai13 + bai14 + bai15 + bai16 + bai17 + bai18 +
                   bai19 + bai20 + bai21
                  
             # variances
             F1 ~~ 1*F1
             '

#-------------------------------------------------------------------
# fitting the model
#-------------------------------------------------------------------

fit1 <- cfa(one.factor.model, data=bai, ordered=all.items)
summary(fit1, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

# residuals
residuals(fit1)

# path diagram
lavaan.diagram(fit1) 

#-------------------------------------------------------------------
# fitting the model
#-------------------------------------------------------------------

fit2 <- cfa(four.factors.model, data=bai, ordered=all.items)
summary(fit2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

residuals(fit2)

# path diagram
lavaan.diagram(fit2) 


#-------------------------------------------------------------------
# let us consider only the firt factor F1
#-------------------------------------------------------------------

attach(bai)
baiF1.dat <- data.frame(bai4, bai5, bai9, bai10, bai14, bai17)
detach(bai)

#-------------------------------------------------------------------
# congeneric model
#-------------------------------------------------------------------

model.con <-  '
             F1 =~ NA*bai4 + bai5 + bai9 + bai10 + bai14 + bai17
             # variances
             F1 ~~ 1*F1
             '

fit.con <- cfa(model.con, data=baiF1.dat,
                 ordered=c("bai4", "bai5", "bai9", "bai10", "bai14", "bai17"))

summary(fit.con, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

# path diagram
lavaan.diagram(fit.con)

#-------------------------------------------------------------------
# tau-equivalent model
#-------------------------------------------------------------------

model.tau <-  '
             F1 =~ NA*bai4 + equal("F1=~bai4")*bai5 + equal("F1=~bai4")*bai9 +
                   equal("F1=~bai4")*bai10 + equal("F1=~bai4")*bai14 +
                   equal("F1=~bai4")*bai17
             # variances
             F1 ~~ 1*F1
             '

fit.tau <- cfa(model.tau, data=baiF1.dat,
                 ordered=c("bai4", "bai5", "bai9", "bai10", "bai14", "bai17"))

summary(fit.tau, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

# path diagram
lavaan.diagram(fit.baiF1)

#-------------------------------------------------------------------
# comparison between congeneric and the tau-equivalent models
#-------------------------------------------------------------------

chi2.c <- 5.461
chi2.t <- 38.680 

df.c <- 9
df.t <- 14

chi2.diff <- chi2.t - chi2.c
df.diff <- df.t - df.c

1 - pchisq(chi2.diff, df.diff)
## [1] 3.404756e-06
# evidence that the model is not tau-equivalent


#-------------------------------------------------------------------
# McDonald's omega
#-------------------------------------------------------------------

lambda <- coef(fit.con)[1:6]

psi <- c( 0.714, 0.385, 0.262, 0.397, 0.512, 0.310  )

omega <- (sum(lambda))^2 / ( (sum(lambda))^2 + sum(psi) )
omega
## [1] 0.886273


#-------------------------------------------------------------------
# Cornbach's alpha
#-------------------------------------------------------------------

# polichoric correlation matrix

lower <- '
1.0000000 
0.4233073 1.0000000 
0.3924215 0.6729254 1.0000000 
0.5213107 0.6072612 0.6258535 1.0000000 
0.3843480 0.5739210 0.6245394 0.4976908 1.0000000 
0.3517218 0.6379123 0.7508190 0.6600749 0.5671149 1.0000000 '

bai1.cov <- getCov(lower,
                   names=c("bai4", "bai5", "bai9", "bai10", "bai14", "bai17"))

bai1.cov

p <- 6

alpha <- (p / (p - 1)) * (1 - tr(bai1.cov) / sum(bai1.cov) )
## [1] 0.8811682

alpha(bai1.cov)
  ## raw_alpha std.alpha G6(smc) average_r
  ##     0.88      0.88    0.88      0.55


lambda <- 0.756
psi    <- 0.428


alpha <- (p^2 * lambda^2) / ((p^2 * lambda^2) + p*psi)
alpha
## [1] 0.8890391





