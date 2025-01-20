# Esercitazione
# Analisi fattoriale sui dati del test BAI
#
# "Thu May 10 11:41:36 2018"

# Augustine Osman, Kopper, Barrios, Osman, 
# and Wade (1997). The Beck Anxiety Inventory: Reexamination of Factor 
# Structure and Psychometric Properties. JOURNAL OF CLINICAL PSYCHOLOGY, 
# Vol. 53(1), 7–14.

# 0–9: normal to minimal anxiety
# 10–18: mild to moderate anxiety
# 19–29: moderate to severe anxiety
# 30–63: severe anxiety

library("tidyverse")
library("psych")
library("lavaan")
library("paran")
library("GPArotation")
library("corrplot")


#--------------------------------------------------------------------
# Read data

bai <- read.table("BAI.txt", header = TRUE)

dim(bai)
glimpse(bai)
summary(bai)


# total score
y <- rowSums(bai)
y

summary(y)
hist(y)

# descriptive stats
mean(y)
var(y)

dat <- data.frame(y)

dat$groups <- cut(
  dat$y, 
  c(0, 9.5, 18.5, 29.5, 63),
  include.lowest = TRUE,
  labels = c('normal to minimal', 'mild to moderate', 'moderate to severe', 'severe'))

summary(dat)

# histogram
ggplot(data = dat, aes(y)) + 
  geom_histogram(breaks= c(0, 9.5, 18.5, 29.5, 63))

# create covariance and correlation matrix
S <- cov(bai)
R <- cor(bai)

corrplot(R)


#--------------------------------------------------------------------
# How many factors?

e <- eigen(S)

# total variance
sum(e$val)
sum(diag(S))

e$val

e$val[1:5] / sum(diag(S))

paran(
  S,
  graph = TRUE,
  iterations = 5000,
  centile = 95
)


# factor analysis with ML
fa <- factanal(
  covmat = S, 
  factors = 1, 
  n.obs = 170
)
fa

fa <- factanal(
  covmat = S, 
  factors = 2, 
  n.obs = 170
)
fa

fa <- factanal(
  covmat = S, 
  factors = 3, 
  n.obs = 170
)
fa

fa <- factanal(
  covmat = S, 
  factors = 4, 
  n.obs = 170
)
fa

fa <- factanal(
  covmat = S, 
  factors = 5, 
  n.obs = 170
)
fa

fa <- factanal(
  covmat = S, 
  factors = 6, 
  n.obs = 170
)
fa

fa <- factanal(
  covmat = S, 
  factors = 7, 
  n.obs = 170
)
fa

# But the data are from Liker scales!
# Polychoric correlation is used to measure the degree of correlation between two ordinal 
# variables with the assumption that each ordinal variable is a discrete summary of an 
# underlying (latent) normally distributed continuous variable.  For example, if an ordinal 
# variable Height were measured as very short, short, average, tall, very tall, one could 
# assume that these categories represent actual height measurements that are continuous and 
# normally distributed.  A similar assumption might be made for Likert items, for example on 
# an agree–disagree spectrum.


poly <- cor.ci(R, n.iter = 10, poly = TRUE)
poly$rho

e <- eigen(R)

e$val

e$val[1:5] / sum(diag(R))

paran(
  poly$rho,
  graph = TRUE,
  iterations = 5000,
  centile = 95
)

# factor analysis with ML
fa <- factanal(
  covmat = poly$rho, 
  factors = 1, 
  n.obs = 170
)
fa


#--------------------------------------------------------------------
# Exploratory FA

fa(R, 3, rotate = "oblimin", fm = "pa")
fa(poly$rho, 3, rotate="oblimin", fm = "pa")
principal(poly$rho, 3, rotate = "oblimin")


#--------------------------------------------------------------------
# split half reliability
l <- as.numeric(fa$loadings)
l

u <- as.numeric(fa$uniquenesses)
u

sum(l)^2 / (sum(l)^2 + sum(u))

ii <- 1:21

nrep <- 1000
res <- rep(NA, nrep)
for (i in 1:nrep) {
  index1 <- sample(ii, 10, replace = FALSE)
  i1 <- sort(index1)
  i2 <- sort(setdiff(ii, index1))
  half1 <- bai[, (c(i1))]
  half2 <- bai[, (c(i2))]
  t1 <- rowSums(half1)
  t2 <- rowSums(half2)
  res[i] <- cor(t1, t2)
}

hist(res)
mean(res)
sd(res)


#--------------------------------------------------------------------
# CFA

# assign the name to the rows and columns of the matrix
all_items <- c("bai1","bai2", "bai3", "bai4",
               "bai5","bai6", "bai7", "bai8",
               "bai9","bai10", "bai11", "bai12",
               "bai13","bai14", "bai15", "bai16",
               "bai17","bai18", "bai19", "bai20",
               "bai21")


#--------------------------------------------------------------------
# four factors model (Osman et al., 1997)

four_factors_model <-  '
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

one_factor_model <-  '
             F1 =~ NA*bai1 + bai2 + bai3 + bai4 + bai5 + bai6 +
                   bai7 + bai8 + bai9 + bai10 + bai11 + bai12 +
                   bai13 + bai14 + bai15 + bai16 + bai17 + bai18 +
                   bai19 + bai20 + bai21

             # variances
             F1 ~~ 1*F1
             '

#-------------------------------------------------------------------
# fitting the one factor model

fit1 <- sem(
  one_factor_model, 
  data = bai, 
  ordered = all_items
)

summary(
  fit1,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare      = TRUE
)

333.646 183
393.09 189

393.09 - 333.646
189 - 183

1 - pchisq(59.444, 6) 

# residuals
residuals(fit1)

# path diagram
lavaan.diagram(fit1)

inspect(fit1, what="std")$lambda

lambda <- inspect(fit1, what="std")$lambda
lambda

psi <- 1 - lambda^2
psi

((sum(lambda))^2) / ((sum(lambda))^2 + sum(psi))

psych::alpha(bai)


#--------------------------------------------------------------------
# fitting the four-factor model

fit2 <- sem(
  four_factors_model, 
  data = bai, 
  ordered = all_items
)


summary(
  fit2,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare      = TRUE
)

residuals(fit2)

# path diagram
lavaan.diagram(fit2)


# comparison between the two models
# df = 189, chisq = 393.090
# df = 183, chisq = 333.646

1 - pchisq(393.090 - 333.646, 189 - 183)


#--------------------------------------------------------------------
# let us consider only the firt factor F1

bai_1 <- with(bai,
              data.frame(bai4, bai5, bai9, bai10, bai14, bai17))


#--------------------------------------------------------------------
# congeneric model

model_con <-  '
             F1 =~ NA*bai4 + bai5 + bai9 + bai10 + bai14 + bai17
             # variances
             F1 ~~ 1*F1
             '

fit_con <- sem(
  model_con, 
  data = bai_1,
  ordered = c("bai4", "bai5", "bai9", "bai10", "bai14", "bai17")
)


summary(
  fit_con,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)

# path diagram
lavaan.diagram(fit_con)


#--------------------------------------------------------------------
# tau-equivalent model

model_tau <-  '
             F1 =~ NA*bai4 +
                   equal("F1=~bai4") * bai5 +
                   equal("F1=~bai4") * bai9 +
                   equal("F1=~bai4") * bai10 +
                   equal("F1=~bai4") * bai14 +
                   equal("F1=~bai4") * bai17
             # variances
             F1 ~~ 1*F1
             '

fit_tau <- sem(
  model_tau, 
  data = bai_1,
  ordered = c("bai4", "bai5", "bai9", "bai10", "bai14", "bai17")
)

summary(
  fit_tau, 
  fit.measures = TRUE, 
  standardized = TRUE,
  rsquare = TRUE
)

# path diagram
lavaan.diagram(fit_tau)



53.588 14

16.244 9

53.588 - 16.244 


#-------------------------------------------------------------------
# comparison between congeneric and the tau-equivalent models

chi2_t <- 53.588
chi2_c <- 16.244

df_t <- 14
df_c <- 9

chi2_diff <- chi2_t - chi2_c
df_diff <- df_t - df_c

1 - pchisq(chi2_diff, df_diff)
# [1] 5.109802e-07
# the model is not tau-equivalent


#--------------------------------------------------------------------
# McDonald's omega

lambda <- inspect(fit_con, what="std")$lambda
lambda

psi <- 1 - lambda^2
psi

((sum(lambda))^2) / ((sum(lambda))^2 + sum(psi))

psych::alpha(bai_1)

omegaFromSem(
  fit_con, 
  m = NULL, 
  flip = TRUE, 
  plot = TRUE
)


#--------------------------------------------------------------------
# Cornbach's alpha

# polichoric correlation matrix

lower <- '
  1.0000000
  0.4233073 1.0000000
  0.3924215 0.6729254 1.0000000
  0.5213107 0.6072612 0.6258535 1.0000000
  0.3843480 0.5739210 0.6245394 0.4976908 1.0000000
  0.3517218 0.6379123 0.7508190 0.6600749 0.5671149 1.0000000 
'

bai1_cov <- getCov(
  lower,
  names = c("bai4", "bai5", "bai9", "bai10", "bai14", "bai17")
)

bai1_cov

p <- 6

alpha <- (p / (p - 1)) * (1 - tr(bai1_cov) / sum(bai1_cov))
alpha
## [1] 0.8811682

psych::alpha(bai1_cov)


#--------------------------------------------------------------------
# Reliability Spearman-Brown

lambda <- 0.756
psi    <- 0.428

alpha <- (p^2 * lambda^2) / ((p^2 * lambda^2) + p*psi)
alpha



