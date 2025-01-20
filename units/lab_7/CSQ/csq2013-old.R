library(psych)
library(car)


all <- read.csv("datiOK.csv", header = TRUE, sep = ";", na.string = " ") 



csq <- all[,2:73]
bdi <- all[,74:94]
bai <- all[,95:115]
das <- all[,116:155]



n <- 185


#internality
I <-csq[,c(1,10,19,28,37,46,55,64,6,15,24,33,42,51,60,69)]

#globality
G <-csq[,c(2,11,20,29,38,47,56,65,7,16,25,34,43,52,61,70)]

#stability 
S <-csq[,c(3,12,21,30,39,48,57,66,8,17,26,35,44,53,62,71)]

#negcons
N <-csq[,c(4,13,22,31,40,49,58,67)]

#selfworth
W <-csq[,c(5,14,23,32,41,50,59,68,9,18,27,36,45,54,63,72)]


#------------------------
#Recode 

I1<-I
I1[,1]<-recode(I[,1],"1=5;2=4;3=3;4=2;5=1;")
I1[,2]<-recode(I[,2],"1=5;2=4;3=3;4=2;5=1;")
I1[,4]<-recode(I[,4],"1=5;2=4;3=3;4=2;5=1;")
I1[,5]<-recode(I[,5],"1=5;2=4;3=3;4=2;5=1;")
I1[,6]<-recode(I[,6],"1=5;2=4;3=3;4=2;5=1;")
I1[,7]<-recode(I[,7],"1=5;2=4;3=3;4=2;5=1;")
I1[,8]<-recode(I[,8],"1=5;2=4;3=3;4=2;5=1;")
I1[,14]<-recode(I[,14],"1=5;2=4;3=3;4=2;5=1;")

G1<-G
G1[,3]<-recode(G[,3],"1=5;2=4;3=3;4=2;5=1;")
G1[,4]<-recode(G[,4],"1=5;2=4;3=3;4=2;5=1;")
G1[,5]<-recode(G[,5],"1=5;2=4;3=3;4=2;5=1;")
G1[,7]<-recode(G[,7],"1=5;2=4;3=3;4=2;5=1;")
G1[,8]<-recode(G[,8],"1=5;2=4;3=3;4=2;5=1;")
G1[,9]<-recode(G[,9],"1=5;2=4;3=3;4=2;5=1;")
G1[,10]<-recode(G[,10],"1=5;2=4;3=3;4=2;5=1;")
G1[,11]<-recode(G[,11],"1=5;2=4;3=3;4=2;5=1;")
G1[,14]<-recode(G[,14],"1=5;2=4;3=3;4=2;5=1;")

S1<-S
S1[,1]<-recode(S[,1],"1=5;2=4;3=3;4=2;5=1;")
S1[,4]<-recode(S[,4],"1=5;2=4;3=3;4=2;5=1;")
S1[,5]<-recode(S[,5],"1=5;2=4;3=3;4=2;5=1;")
S1[,10]<-recode(S[,10],"1=5;2=4;3=3;4=2;5=1;")
S1[,11]<-recode(S[,11],"1=5;2=4;3=3;4=2;5=1;")
S1[,14]<-recode(S[,14],"1=5;2=4;3=3;4=2;5=1;")
S1[,15]<-recode(S[,15],"1=5;2=4;3=3;4=2;5=1;")
S1[,16]<-recode(S[,16],"1=5;2=4;3=3;4=2;5=1;")

N1<-N
N1[,2]<-recode(N[,2],"1=5;2=4;3=3;4=2;5=1;")
N1[,4]<-recode(N[,4],"1=5;2=4;3=3;4=2;5=1;")
N1[,6]<-recode(N[,6],"1=5;2=4;3=3;4=2;5=1;")
N1[,8]<-recode(N[,8],"1=5;2=4;3=3;4=2;5=1;")

W1<-W
W1[,2]<-recode(W[,2],"1=5;2=4;3=3;4=2;5=1;")
W1[,4]<-recode(W[,4],"1=5;2=4;3=3;4=2;5=1;")
W1[,8]<-recode(W[,8],"1=5;2=4;3=3;4=2;5=1;")
W1[,9]<-recode(W[,9],"1=5;2=4;3=3;4=2;5=1;")
W1[,11]<-recode(W[,11],"1=5;2=4;3=3;4=2;5=1;")
W1[,13]<-recode(W[,13],"1=5;2=4;3=3;4=2;5=1;")
W1[,14]<-recode(W[,14],"1=5;2=4;3=3;4=2;5=1;")
W1[,15]<-recode(W[,15],"1=5;2=4;3=3;4=2;5=1;")




#dimensioni (total scores)
dim.I <-rowSums(I1)
dim.G <-rowSums(G1)
dim.S <-rowSums(S1)
dim.N <-rowSums(N1)
dim.W <-rowSums(W1)



dimensioni <- cbind(dim.I,dim.G,dim.S,dim.N,dim.W)

head(dimensioni)


# Verifica normalità


qqnorm (dimensioni[,1])
qqline(dimensioni[,1], col = 2)

qqnorm (dimensioni[,2])
qqline(dimensioni[,2], col = 2)

qqnorm (dimensioni[,3])
qqline(dimensioni[,3], col = 2)

qqnorm (dimensioni[,4])
qqline(dimensioni[,4], col = 2)

qqnorm (dimensioni[,5])
qqline(dimensioni[,5], col = 2)





shapiro.test(dimensioni[,1])
shapiro.test(dimensioni[,2])
shapiro.test(dimensioni[,3])
shapiro.test(dimensioni[,4])
shapiro.test(dimensioni[,5])



describe(dimensioni, type=2)


#----------------- Matrice correlazione


Rdim <- cor(dimensioni)

round(Rdim, 2)


#------
#Estrazione fattori
ee <- eigen(Rdim)

print(ee, 3)

fa.parallel(Rdim, n.obs = n)


#--------------------

principal(Rdim, nfactors = 1, n.obs=n)


fa(Rdim, 1, n.obs = n, fm="ml")


fa <- factanal(covmat = Rdim, factors = 1, n.obs = n)
fa

# ANALISI CONFERMATIVA

library(lavaan)


csq.model <- ' F1 =~ NA*dim.I + dim.G + dim.S + dim.N + dim.W
                    # variances
                    F1 ~~ 1*F1 
             '

fit <- lavaan:::cfa(csq.model, sample.cov = Rdim, sample.nobs = n)
summary(fit, fit.measures = TRUE)





fit <- cfa(model = mod, orthogonal=FALSE, data=dimensioni, estimator="ML",std.lv=TRUE,fixed.x=TRUE,std.ov=TRUE)




