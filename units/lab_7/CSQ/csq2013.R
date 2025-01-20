library(psych)
library(car)
library(lavaan)
library(semPlot)
library(robust)
library(ltm)


# read data ---------------------------------------------------------------

all <- read.csv("dati340-4nov13.csv", header = TRUE, sep = ";", na.string = " ") 


# select csq scores -------------------------------------------------------

csq.dat <- all[, 2:73]   # 340 sogg
bdi <- all[,74:94]       # 340 sogg
bai <- all[,95:115]      # 340 sogg
das <- all[,116:155]     # prime 170 righe



# set size ----------------------------------------------------------------

n <- dim(csq.dat)[1]
n


# subscales ---------------------------------------------------------------

#internality
I <- csq.dat[, c(1,10,19,28,37,46,55,64,6,15,24,33,42,51,60,69)]

#globality
G <- csq.dat[, c(2,11,20,29,38,47,56,65,7,16,25,34,43,52,61,70)]

#stability 
S <- csq.dat[, c(3,12,21,30,39,48,57,66,8,17,26,35,44,53,62,71)]

#negcons
N <- csq.dat[, c(4,13,22,31,40,49,58,67)]

#selfworth
W <- csq.dat[, c(5,14,23,32,41,50,59,68,9,18,27,36,45,54,63,72)]



# recode items ------------------------------------------------------------

I1 <- I
I1[,1]  <- recode(I[, 1],"1=5;2=4;3=3;4=2;5=1;")
I1[,2]  <- recode(I[, 2],"1=5;2=4;3=3;4=2;5=1;")
I1[,4]  <- recode(I[, 4],"1=5;2=4;3=3;4=2;5=1;")
I1[,5]  <- recode(I[, 5],"1=5;2=4;3=3;4=2;5=1;")
I1[,6]  <- recode(I[, 6],"1=5;2=4;3=3;4=2;5=1;")
I1[,7]  <- recode(I[, 7],"1=5;2=4;3=3;4=2;5=1;")
I1[,8]  <- recode(I[, 8],"1=5;2=4;3=3;4=2;5=1;")
I1[,14] <- recode(I[, 14],"1=5;2=4;3=3;4=2;5=1;")

G1 <- G
G1[,3]  <- recode(G[,3],"1=5;2=4;3=3;4=2;5=1;")
G1[,4]  <- recode(G[,4],"1=5;2=4;3=3;4=2;5=1;")
G1[,5]  <- recode(G[,5],"1=5;2=4;3=3;4=2;5=1;")
G1[,7]  <- recode(G[,7],"1=5;2=4;3=3;4=2;5=1;")
G1[,8]  <- recode(G[,8],"1=5;2=4;3=3;4=2;5=1;")
G1[,9]  <- recode(G[,9],"1=5;2=4;3=3;4=2;5=1;")
G1[,10] <- recode(G[,10],"1=5;2=4;3=3;4=2;5=1;")
G1[,11] <- recode(G[,11],"1=5;2=4;3=3;4=2;5=1;")
G1[,14] <- recode(G[,14],"1=5;2=4;3=3;4=2;5=1;")

S1 <- S
S1[,1]  <- recode(S[,1],"1=5;2=4;3=3;4=2;5=1;")
S1[,4]  <- recode(S[,4],"1=5;2=4;3=3;4=2;5=1;")
S1[,5]  <- recode(S[,5],"1=5;2=4;3=3;4=2;5=1;")
S1[,10] <- recode(S[,10],"1=5;2=4;3=3;4=2;5=1;")
S1[,11] <- recode(S[,11],"1=5;2=4;3=3;4=2;5=1;")
S1[,14] <- recode(S[,14],"1=5;2=4;3=3;4=2;5=1;")
S1[,15] <- recode(S[,15],"1=5;2=4;3=3;4=2;5=1;")
S1[,16] <- recode(S[,16],"1=5;2=4;3=3;4=2;5=1;")

N1 <- N
N1[,2] <- recode(N[,2],"1=5;2=4;3=3;4=2;5=1;")
N1[,4] <- recode(N[,4],"1=5;2=4;3=3;4=2;5=1;")
N1[,6] <- recode(N[,6],"1=5;2=4;3=3;4=2;5=1;")
N1[,8] <- recode(N[,8],"1=5;2=4;3=3;4=2;5=1;")

W1 <- W
W1[,2]  <- recode(W[,2],"1=5;2=4;3=3;4=2;5=1;")
W1[,4]  <- recode(W[,4],"1=5;2=4;3=3;4=2;5=1;")
W1[,8]  <- recode(W[,8],"1=5;2=4;3=3;4=2;5=1;")
W1[,9]  <- recode(W[,9],"1=5;2=4;3=3;4=2;5=1;")
W1[,11] <- recode(W[,11],"1=5;2=4;3=3;4=2;5=1;")
W1[,13] <- recode(W[,13],"1=5;2=4;3=3;4=2;5=1;")
W1[,14] <- recode(W[,14],"1=5;2=4;3=3;4=2;5=1;")
W1[,15] <- recode(W[,15],"1=5;2=4;3=3;4=2;5=1;")



# compute total score for each subscale -----------------------------------

In <- rowSums(I1)
Gl <- rowSums(G1)
St <- rowSums(S1)
NC <- rowSums(N1)
SW <- rowSums(W1)



# CSQ data set ------------------------------------------------------------

csq <- cbind(In, Gl, St, NC, SW)

head(csq)



# normality check ---------------------------------------------------------

qqnorm (csq[, 1])
qqline(csq[, 1], col = 2)

qqnorm (csq[, 2])
qqline(csq[, 2], col = 2)

qqnorm (csq[, 3])
qqline(csq[, 3], col = 2)

qqnorm (csq[, 4])
qqline(csq[, 4], col = 2)

qqnorm (csq[, 5])
qqline(csq[, 5], col = 2)


shapiro.test(csq[, 1])
shapiro.test(csq[, 2])
shapiro.test(csq[, 3])
shapiro.test(csq[, 4])
shapiro.test(csq[, 5])


describe(csq, type = 2)



# data screening ----------------------------------------------------------

pairs.panels(csq)


nfact <- 1
(FS <- forward.search(csq, nfact))
plot(FS)



(ORresult <- obs.resid(csq, nfact))

hist(ORresult$std_res[, 1])
hist(ORresult$std_res[, 2])
hist(ORresult$std_res[, 3])
hist(ORresult$std_res[, 4])


output <- robustMD(csq)
output
summary(output)
plot(output)
plot(output$mah, type = 'qqplot')
boxplot(output$mah)

(gCDresult <- gCD(csq, nfact))
plot(gCDresult)
boxplot(gCDresult$gCD)

hist(gCDresult$gCD)


fm <- lm(csq[, 5] ~ csq[, 1] + csq[, 2] + csq[, 3] + csq[, 4])
plot(fm$fit, fm$res)


# correlation matrix ------------------------------------------------------

R <- cor(csq)
round(R, 2)

S <- cov(csq)
round(S, 2)

# robust correlation matrix
out <- covRob(csq, cor=TRUE)
R <- out$cov
round(R, 2)


# parallel analysis -------------------------------------------------------

ee <- eigen(R)
print(ee, 3)
fa.parallel(R, n.obs = n)


# exploratory FA ----------------------------------------------------------

fa <- factanal(covmat = R, factors = 1, n.obs = n)
fa


# confirmatory FA ---------------------------------------------------------

csq.model <- ' F1  =~ NA*In + Gl + St + NC + SW
                    # variances
                    F1 ~~ 1*F1
            '

fit <- lavaan:::cfa(csq.model, sample.cov = R, sample.nobs = n)
summary(fit, fit.measures = TRUE)



# path diagram ------------------------------------------------------------

semPaths(fit, title = FALSE, curvePivot = TRUE)

# Standardized parameters:
semPaths(fit, "std", edge.label.cex = 0.75, curvePivot = TRUE, title = TRUE)


#-------------------------------------------------------------------
# congeneric model
#-------------------------------------------------------------------

model.con <- ' F1  =~ NA*In + Gl + St + NC + SW
                    # variances
                    F1 ~~ 1*F1
            '

fit.con <- cfa(model.con, sample.cov = R, sample.nobs = n)

summary(fit.con, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

# Standardized parameters:
semPaths(fit.con, "std", edge.label.cex = 0.75, curvePivot = TRUE, title = TRUE)


lambda <- fit.con@Model@GLIST$lambda
lambda

psi <- c(fit.con@Model@GLIST$theta[1,1], 
         fit.con@Model@GLIST$theta[2,2], 
         fit.con@Model@GLIST$theta[3,3], 
         fit.con@Model@GLIST$theta[4,4], 
         fit.con@Model@GLIST$theta[5,5])

psi

# reliability

om <- sum(lambda)^2 / (sum(lambda)^2 + sum(psi))
om

omega.md <- (.258 + .85 + .848 + .762 + .839)^2 / 
  ((.258 + .85 + .848 + .762 + .839)^2 + 0.931 + .275 + .278 + .416 + .292)
omega.md

omega(R, nfactors = 1)


#-------------------------------------------------------------------
# tau-equivalent model
#-------------------------------------------------------------------

model.tau <-  ' F1 =~ NA*In + equal("F1=~In")*Gl + equal("F1=~In")*St +
                equal("F1=~In")*NC + equal("F1=~In")*SW
                # variances
                F1 ~~ 1*F1
              '

fit.tau <- cfa(model.tau, sample.cov = S, sample.nobs = n)

summary(fit.tau, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

# variance-covariance matrix
S

p <- 5
alpha <- (p/(p - 1)) * (1 - tr(S)/sum(S))
alpha

alpha(S)



#-------------------------------------------------------------------
# comparison between congeneric and the tau-equivalent models
#-------------------------------------------------------------------

chi2.c <- 12.959
chi2.t <- 108.322 

df.c <- 5
df.t <- 9

chi2.diff <- chi2.t - chi2.c
df.diff <- df.t - df.c

1 - pchisq(chi2.diff, df.diff)
# [1] 0
# the model is not tau-equivalent


#-------------------------------------------------------------------
# parallel model
#-------------------------------------------------------------------

rr <- NULL 
k <- 1 
for(i in 1:p) 
    for(j in 1:p){ 
      if(j != i)
        rr[k] <- R[i,j] 
      k <- k+1 
     } 

ro.1 <- mean(rr, na.rm=TRUE)
ro.1 

# reliability
p <- 5
(p*ro.1) / ((p-1)*ro.1 + 1) 




#-------------------------------------------------------------------
# item analysis
#-------------------------------------------------------------------

descript(csq)


describe(csq)







BDI <- rowSums(bdi)

BAI <- rowSums(bai)



valconv <- cbind(BDI,BAI)



cor(valconv)




# DAS
# da girare: 1 3 4 5 7 8 9 10 11 13 14 15 16 18 19 20 21 22 23 25 26 27 28 31 32 33 34 36 38 39 

D <- das[1:170, ]

D1<-D
D1[,1]<-recode(D[,1],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,3]<-recode(D[,3],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,4]<-recode(D[,4],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,5]<-recode(D[,5],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,7]<-recode(D[,7],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,8]<-recode(D[,8],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,9]<-recode(D[,9],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,10]<-recode(D[,10],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,11]<-recode(D[,11],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,13]<-recode(D[,13],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,14]<-recode(D[,14],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,15]<-recode(D[,15],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,16]<-recode(D[,16],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,18]<-recode(D[,18],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,19]<-recode(D[,19],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,20]<-recode(D[,20],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,21]<-recode(D[,21],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,22]<-recode(D[,22],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,23]<-recode(D[,23],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,25]<-recode(D[,25],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,26]<-recode(D[,26],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,27]<-recode(D[,27],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,28]<-recode(D[,28],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,31]<-recode(D[,31],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,32]<-recode(D[,32],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,33]<-recode(D[,33],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,34]<-recode(D[,34],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,36]<-recode(D[,36],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,38]<-recode(D[,38],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
D1[,39]<-recode(D[,39],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")



# BAI ---------------------------------------------------------------------


n <- dim(bai)[1] #si prende solo il primo attributo, ovvero numero righe
n

#somatic
S <- bai[, c(1,2,3,6,7,8,11,12,13,15,18,19,20,21)]

#cognitive
C <- bai[, c(4,5,9,10,14,16,17)]

#punteggio totale per ogni sottoscala
somatic <- rowSums(S)
cognitive <- rowSums(C)

#BAI dataset
baiD <- cbind(somatic, cognitive)
head(baiD)

BaiTot <- rowSums(baiD)
hist(BaiTot)

#Matrice di correlazione
R <- cor(bai)
round(R, 2)


#Analisi Fattoriale Confermativa modello Beck et al. (1988)

bai.model <- ' 
  F1  =~ NA*bai1 + bai2 + bai3 + bai6 + bai7 + bai8 + bai11 + bai12 + bai13 + bai15 + bai18 + bai19 + bai20 + bai21
  F2  =~ NA*bai4 + bai5 + bai9 + bai10 + bai14 + bai16 + bai17
  # variances
  F1 ~~ 1*F1
  F2 ~~ 1*F2
  F1 ~~ F2' #Definizione dei fattori da Kabacoff (1997)

fit <- lavaan:::cfa(bai.model, sample.cov = R, sample.nobs = n)
summary(fit, fit.measures = TRUE)

