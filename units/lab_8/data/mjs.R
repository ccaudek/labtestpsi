#' ------------------------------------------------------------------------
#' Analysis of Assessing Jealousy: Factor Analyses, Measurement Invariance, 
#' Nomological Validity, and Longitudinal APIM Analyses of the Multidimensional
#' Jealousy Scale (Brauer & Proyer, 2024).
#' 
#' Version: Wed Nov 27 05:20:30 2024
#' ------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(lavaan)
  library(psych)
  library(MBESS)
})


# Descriptives ------------------------------------------------------------

data <- rio::import("MJS_Sample1.sav")

# Descriptive statistics
describe(
  data |> dplyr::select(MJS_Cognitive, MJS_Emotional, MJS_Behavioral)
)


# CFA ---------------------------------------------------------------------

data <- read.table("MJS_Study1.txt", header = TRUE)

# Rename the columns to match the variable names MJS01-MJS24
colnames(data) <- c("Gender", 
                    paste0("MJS", sprintf("%02d", 1:24)))

# Specifica il modello CFA
model <- '
  # Fattore generale che satura su tutte le 24 variabili
  General =~ MJS01 + MJS02 + MJS03 + MJS04 + MJS05 + MJS06 + 
             MJS07 + MJS08 + MJS09 + MJS10 + MJS11 + MJS12 + 
             MJS13 + MJS14 + MJS15 + MJS16 + MJS17 + MJS18 + 
             MJS19 + MJS20 + MJS21 + MJS22 + MJS23 + MJS24
'

# Definizione delle variabili categoriche (ordinarie)
ordered_vars <- paste0("MJS", sprintf("%02d", 1:24))

# Stima del modello con WLSMV
fit <- cfa(model, 
           data = data, 
           ordered = ordered_vars, 
           estimator = "WLSMV")

# Output dei risultati
summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE)


# Three correlated factors
model <- '
  # First-order factors
  Cognitive =~ MJS01 + MJS02 + MJS03 + MJS04 + MJS05 + MJS06 + MJS07 + MJS08
  Emotional =~ MJS09 + MJS10 + MJS11 + MJS12 + MJS13 + MJS14 + MJS15 + MJS16
  Behavior  =~ MJS17 + MJS18 + MJS19 + MJS20 + MJS21 + MJS22 + MJS23 + MJS24
'

# Define the ordinal variables
ordered_vars <- paste0("MJS", sprintf("%02d", 1:24))

# Fit the CFA model with WLSMV
fit <- cfa(model, 
           data = data, 
           ordered = ordered_vars, 
           estimator = "WLSMV")

# Output the results
summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE)


# Second-order model
model <- '
  # First-order factors
  Cognitive =~ MJS01 + MJS02 + MJS03 + MJS04 + MJS05 + MJS06 + MJS07 + MJS08
  Emotional =~ MJS09 + MJS10 + MJS11 + MJS12 + MJS13 + MJS14 + MJS15 + MJS16
  Behavior  =~ MJS17 + MJS18 + MJS19 + MJS20 + MJS21 + MJS22 + MJS23 + MJS24

  # Second-order factor
  Jealousy =~ Cognitive + Emotional + Behavior

  # Variance constraint on the second-order factor
  Jealousy ~~ 1 * Jealousy
'

# Define the ordinal variables
ordered_vars <- paste0("MJS", sprintf("%02d", 1:24))

# Fit the CFA model with WLSMV
fit <- cfa(model, 
           data = data, 
           ordered = ordered_vars, 
           estimator = "WLSMV")

# Output the results
summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE)



# Measurement Invariance --------------------------------------------------

# Load the data
data <- read.table("PPK_Jealousy.txt", header = TRUE)

# Rename columns to match MJS01-MJS24 and Gender
colnames(data) <- c("Gender", paste0("MJS", sprintf("%02d", 1:24)))

# Specify the CFA model
model <- '
  # First-order factors
  Cognitive =~ MJS01 + MJS02 + MJS03 + MJS04 + MJS05 + MJS06 + MJS07 + MJS08
  Emotional =~ MJS09 + MJS10 + MJS11 + MJS12 + MJS13 + MJS14 + MJS15 + MJS16
  Behavior  =~ MJS18 + MJS19 + MJS20 + MJS21 + MJS22 + MJS23 + MJS24
'

# Configural invariance (no equality constraints)
fit_configural <- cfa(model, data = data, group = "Gender", estimator = "MLR")

# Metric invariance (equal loadings)
fit_metric <- cfa(
  model, data = data, group = "Gender", 
  group.equal = "loadings", estimator = "MLR"
)

# Scalar invariance (equal loadings and intercepts)
fit_scalar <- cfa(
  model, data = data, group = "Gender", 
  group.equal = c("loadings", "intercepts"), estimator = "MLR"
)

# Compare the models using likelihood ratio tests
comparison <- lavTestLRT(fit_configural, fit_metric, fit_scalar)

# Output results
summary(fit_configural, fit.measures = TRUE, standardized = TRUE)
summary(fit_metric, fit.measures = TRUE, standardized = TRUE)
summary(fit_scalar, fit.measures = TRUE, standardized = TRUE)

comparison


# McDonald's omega coefficients -------------------------------------------

MJS_S1 <- read.table("MJS_Study1.txt", header = TRUE)

# Rename the columns to match the variable names MJS01-MJS24
colnames(MJS_S1) <- c("Gender", paste0("MJS", 1:24))

Cognitive_S1<- cbind( MJS_S1$MJS1,MJS_S1$MJS2, MJS_S1$MJS3, MJS_S1$MJS4, MJS_S1$MJS5, MJS_S1$MJS6, MJS_S1$MJS7, MJS_S1$MJS8)
Cognitive_S1

Emotional_S1<- cbind( MJS_S1$MJS9,MJS_S1$MJS10, MJS_S1$MJS11, MJS_S1$MJS12, MJS_S1$MJS13, MJS_S1$MJS14, MJS_S1$MJS15, MJS_S1$MJS16)
Emotional_S1

Behavior_S1<- cbind( MJS_S1$MJS17,MJS_S1$MJS18, MJS_S1$MJS19, MJS_S1$MJS20, MJS_S1$MJS21, MJS_S1$MJS22, MJS_S1$MJS23, MJS_S1$MJS24)
Behavior_S1

# Computing omega for Sample 1 data

set.seed(1)

# Cognitive
ci.reliability(
  data=Cognitive_S1, type="omega", conf.level=0.95,interval.type="mlr",B=1000
)

colnames(Cognitive_S1) <- c(paste0("MJS", 1:8))

# Specifica il modello CFA (ad esempio, per un singolo fattore latente "Cognitive")
model <- '
  Cognitive =~ MJS1 + MJS2 + MJS3 + MJS4 + MJS5 + MJS6 + MJS7 + MJS8
'

# Stima il modello CFA
fit <- cfa(model, data = Cognitive_S1, estimator = "MLR")

# Estrarre le saturazioni fattoriali e le varianze residue
lambda <- inspect(fit, "std")$lambda  # Saturazioni standardizzate
theta <- inspect(fit, "std")$theta    # Matrice delle varianze residue
psi <- inspect(fit, "std")$psi        # Varianza del fattore

# Calcolo di Omega (McDonald)
# Numeratore: varianza spiegata dal fattore
omega_num <- sum(lambda)^2
# Denominatore: varianza totale (spiegata + residua)
omega_den <- omega_num + sum(diag(theta))
omega <- omega_num / omega_den

# Stampa il risultato
cat("Omega (McDonald):", round(omega, 3), "\n")


# Emotional
ci.reliability(
  data=Emotional_S1, type="omega", conf.level=0.95,interval.type="mlr",B=1000
)

# Behavior
ci.reliability(
  data=Behavior_S1, type="omega", conf.level=0.95,interval.type="mlr",B=1000
)





