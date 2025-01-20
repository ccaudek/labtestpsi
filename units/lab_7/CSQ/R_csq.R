# -------------------------------------------------------------------
# CVSM Esercitazione sull'analisi fattoriale
#
# Corrado Caudek
# version: "Tue Apr  7 11:44:38 2020"
# -------------------------------------------------------------------

library("psych")
library("car")
library("lavaan")
library("semPlot")
library("tidyverse")
library("rio")


# read data ----

df <- import("csq540.csv")

df <- df %>% 
  dplyr::rename(
    "int" = "dim.I",
    "glo" = "dim.G",
    "sta" = "dim.S",
    "neg" = "dim.N",
    "sel" = "dim.W"
  )

# sample size
n <- nrow(df)
n


# data screening ----

qqPlot(df[, 1])
qqPlot(df[, 2])
qqPlot(df[, 3])
qqPlot(df[, 4])
qqPlot(df[, 5])

psych::describe(df, type = 2)

pairs.panels(df)


# correlation matrix ----

R <- cor(df)
round(R, 2)


# exploratory FA ----

fa <- factanal(
  covmat = R, 
  factors = 1, 
  n.obs = n
)
fa


# confirmatory FA ----

mod1 <- ' 
  F =~ NA*int + glo + sta + neg + sel
  # variances
  F ~~ 1*F
'

fit <- lavaan:::cfa(
  mod1,
  sample.cov = R,
  sample.nobs = n
)

summary(
  fit, 
  standardized = TRUE,
  fit.measures = TRUE
)

lavInspect(fit, "coef")

lambda <- lavInspect(fit, "coef")$lambda
lambda 

t(lambda) %*% lambda

t(lambda) %*% lambda / 5


psy <- diag(as.numeric(1 - lambda^2), 5, 5)

R_m <- lambda %*% t(lambda) + psy

R - R_m


# path diagram ----

semPaths(
  fit, 
  title = FALSE, 
  curvePivot = TRUE
)

# Standardized parameters:
semPaths(
  fit, 
  "std",
  edge.label.cex = 0.75, 
  curvePivot = TRUE, 
  title = TRUE
)







# reliability ------------------------------------------------------------

alpha(csq)



# ------------------------------------------------------------------
# ------------------------------------------------------------------
# ------------------------------------------------------------------
# ------------------------------------------------------------------



mydata <- data.frame(csq = totalScore, bdi = rowSums(bdi))



hist(totalScore, prob=TRUE,
     xlab="CSQ-SF",
     #ylim=c(0, 2),
     main="Punteggi CSQ-SF - studenti universitari")

curve(dnorm(x, mean=mean(totalScore), sd=sd(totalScore)),
      col="darkblue", lwd=3, add=TRUE, yaxt="n")

curve(dnorm(x, mean=199.09, sd=28.81),
      col="red", lwd=3, add=TRUE, yaxt="n")



# ------------------------------------------------------------------
# in una distribuzione gaussiana, il 68% dell'are è compresa
# nell'intervallo tra la media e +/- 1 sd. Questo è vero anche
# per il campione empirico considerato?
# ------------------------------------------------------------------


tmp <- subset(mydata,
              csq < mean(csq) + sd(csq) &
                csq > mean(csq) - sd(csq))
dim(tmp)

244 / 340


# ------------------------------------------------------------------
# per i dati del campione fiorentino (media 179.4, sd = 26.62),
# qual è il punteggio CSQ-SF che corrisponde al 90-esimo percentile
# della distribuzione?
# ------------------------------------------------------------------

qnorm(.90, 179.4, 26.62)
# [1] 213.5149

quantile(mydata$csq, .90)
# 90%
# 212


# ------------------------------------------------------------------
# usando i dati del campione fiorentino (media 179.4, sd = 26.62),
# si stimi l'errore standard della distribuzione campionaria delle
# media dei campioni di ampiezza 340
# ------------------------------------------------------------------

n <- length(mydata$csq)
n

sd(mydata$csq) / sqrt(n)
# [1] 1.44354
# come si interpreta questo valore?


# ------------------------------------------------------------------
# si calcoli l'intervallo di confidenza per il campione fiorentino
# al livello di fiducia del 95%
# ------------------------------------------------------------------


mean(mydata$csq) + c(-1, 1) * qt(.975, n-1) * sd(mydata$csq) / sqrt(n)
# [1] 176.5606 182.2394


# ------------------------------------------------------------------
# si usi l'intervallo di confidenza per la verifica dell'ipotesi
# nulla H0: mu = 199.09
# ------------------------------------------------------------------

# non contiene il valore 199.09, dunque posso rigettare l'ipotesi che
# il campione fiorentino provenga da una popolazione con media 199.09.


# ------------------------------------------------------------------
# si usi la procedura di verifica di ipotesi statistiche per valutare
# tramite il test t di Student, l'ipotesi nulla H0: mu = 199.09 contro
# l'alternativa bilaterale Ha: mu \neq 199.09
# ------------------------------------------------------------------


ym <- mean(mydata$csq)
ym

std <- sd(mydata$csq)
std


T <- (ym - 199.09) / (std/sqrt(n))
T
# [1] -13.64008


t.test(mydata$csq, mu = 199.09)

#   One Sample t-test

# data:  totalScore
# t = -13.6401, df = 339, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 199.09
# 95 percent confidence interval:
#  176.5606 182.2394
# sample estimates:
# mean of x
#     179.4


# ------------------------------------------------------------------
# soggetti con CSQ-SF nel terzo quartile
# ------------------------------------------------------------------

# 0–9: indicates minimal depression
# 10–18: indicates mild depression
# 19–29: indicates moderate depression
# 30–63: indicates severe depression.


q3.dt <- subset(mydata, csq > quantile(mydata$csq, .75))
dim(q3.dt)

q1.dt <- subset(mydata, csq < quantile(mydata$csq, .25))
dim(q1.dt)


# i soggetti nel terzo quartile calcolato rispetto a CSQ-SF soffronto di una
# lieve forma di depressione?
# test unilaterale destro, Ha: mu > 11


ym <- mean(q3.dt$bdi)
ym

std <- sd(q3.dt$bdi)
std

n <- nrow(q3.dt)
n


T <- (ym - 12) / (std/sqrt(n))
T

1 - pt(T, n-1)



t.test(q3.dt$bdi, mu = 12)




mean(q1.dt$bdi) + c(-1, 1) * qt(.975, nrow(q1.dt)-1) * sd(q1.dt$bdi)/sqrt(nrow(q1.dt))

mean(q3.dt$bdi) + c(-1, 1) * qt(.975, nrow(q3.dt)-1) * sd(q3.dt$bdi)/sqrt(nrow(q3.dt))


cor(mydata)






mydata$grp <- ifelse(mydata$csq > median(mydata$csq), 1, 0)
mydata$group <- factor(mydata$grp,
                       levels = c(0, 1),
                       labels = c("Low", "High"))

t.test(bdi ~ grp, mydata, var.equal = TRUE)


mydata %>%
  group_by(grp) %>%
  summarize(m = mean(bdi),
            sd = sd(bdi),
            n = n(),
            std = sd(bdi)/n())


boxplot(bdi ~ group, mydata)


t.test(sqrt(bdi) ~ grp, mydata, var.equal = TRUE)




sp <- sqrt((170*6.88^2 + 168*8.11^2) / (171+169-2))
