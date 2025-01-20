##################################################################
## This R script accompanies the paper "Don't waste your time   ##
## measuring intelligence: Further evidence for the validity of ##
## a three-minute speeded reasoning test" by Schubert et al.    ## 
## (2023) and contains the analysis code for Study 1.           ## 
##################################################################

# set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages ----
library(lavaan)
library(tidyverse)
library(psych)
library(patchwork)

# load data ----
data <- read.csv("data_study1.csv")

# descriptive  statistics ----
knitr::kable(describe(data))

miniq <- data %>%
  ggplot(aes(x = miniq_sum)) + 
  geom_histogram(mapping = aes(x = miniq_sum, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$miniq_sum), sd = sd(data$miniq_sum))) +
  labs(x = "mini-q performance") +
  theme_classic()

BIS_V <- data %>%
  ggplot(aes(x = BIS_V)) + 
  geom_histogram(mapping = aes(x = BIS_V, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$BIS_V), sd = sd(data$BIS_V))) +
  labs(x = "verbal abilities") +
  theme_classic()

BIS_F <- data %>%
  ggplot(aes(x = BIS_F)) + 
  geom_histogram(mapping = aes(x = BIS_F, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$BIS_F), sd = sd(data$BIS_F))) +
  labs(x = "figural abilities") +
  theme_classic()

BIS_N <- data %>%
  ggplot(aes(x = BIS_N)) + 
  geom_histogram(mapping = aes(x = BIS_N, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$BIS_N, na.rm = TRUE), sd = sd(data$BIS_N, na.rm = TRUE))) +
  labs(x = "numerical abilities") +
  theme_classic()

BIS_PC <- data %>%
  ggplot(aes(x = BIS_PC)) + 
  geom_histogram(mapping = aes(x = BIS_PC, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$BIS_PC), sd = sd(data$BIS_PC))) +
  labs(x = "processing capacity") +
  theme_classic()


BIS_PS <- data %>%
  ggplot(aes(x = BIS_PS)) + 
  geom_histogram(mapping = aes(x = BIS_PS, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$BIS_PS), sd = sd(data$BIS_PS))) +
  labs(x = "processing capacity") +
  theme_classic()

BIS_M <- data %>%
  ggplot(aes(x = BIS_M)) + 
  geom_histogram(mapping = aes(x = BIS_M, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$BIS_M, na.rm = TRUE), sd = sd(data$BIS_M, na.rm = TRUE))) +
  labs(x = "memory") +
  theme_classic()

BIS_C <- data %>%
  ggplot(aes(x = BIS_C)) + 
  geom_histogram(mapping = aes(x = BIS_C, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$BIS_C, na.rm = TRUE), sd = sd(data$BIS_C, na.rm = TRUE))) +
  labs(x = "creativity") +
  theme_classic()

WMC_MU <- data %>%
  ggplot(aes(x = WMC_MU)) + 
  geom_histogram(mapping = aes(x = WMC_MU, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$WMC_MU, na.rm = TRUE), sd = sd(data$WMC_MU, na.rm = TRUE))) +
  labs(x = "memory updating") +
  theme_classic()

WMC_OpSp <- data %>%
  ggplot(aes(x = WMC_OpSp)) + 
  geom_histogram(mapping = aes(x = WMC_OpSp, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$WMC_OpSp, na.rm = TRUE), sd = sd(data$WMC_OpSp, na.rm = TRUE))) +
  labs(x = "operation span") +
  theme_classic()

WMC_SenSp <- data %>%
  ggplot(aes(x = WMC_SenSp)) + 
  geom_histogram(mapping = aes(x = WMC_SenSp, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$WMC_SenSp, na.rm = TRUE), sd = sd(data$WMC_SenSp, na.rm = TRUE))) +
  labs(x = "sentence span") +
  theme_classic()

WMC_Binding <- data %>%
  ggplot(aes(x = WMC_Binding)) + 
  geom_histogram(mapping = aes(x = WMC_Binding, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$WMC_Binding, na.rm = TRUE), sd = sd(data$WMC_Binding, na.rm = TRUE))) +
  labs(x = "binding") +
  theme_classic()

SMS_RT <- data %>%
  ggplot(aes(x = SMS_RT)) + 
  geom_histogram(mapping = aes(x = SMS_RT, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$SMS_RT, na.rm = TRUE), sd = sd(data$SMS_RT, na.rm = TRUE))) +
  labs(x = "RT") +
  theme_classic()

CRT_RT <- data %>%
  ggplot(aes(x = CRT_RT)) + 
  geom_histogram(mapping = aes(x = CRT_RT, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$CRT_RT, na.rm = TRUE), sd = sd(data$CRT_RT, na.rm = TRUE))) +
  labs(x = "RT") +
  theme_classic()

PLM_RT <- data %>%
  ggplot(aes(x = PLM_RT)) + 
  geom_histogram(mapping = aes(x = PLM_RT, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$PLM_RT, na.rm = TRUE), sd = sd(data$PLM_RT, na.rm = TRUE))) +
  labs(x = "RT") +
  theme_classic()

SMS_v <- data %>%
  ggplot(aes(x = SMS_v)) + 
  geom_histogram(mapping = aes(x = SMS_v, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$SMS_v, na.rm = TRUE), sd = sd(data$SMS_v, na.rm = TRUE))) +
  labs(x = "v") +
  theme_classic()

CRT_v <- data %>%
  ggplot(aes(x = CRT_v)) + 
  geom_histogram(mapping = aes(x = CRT_v, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$CRT_v, na.rm = TRUE), sd = sd(data$CRT_v, na.rm = TRUE))) +
  labs(x = "v") +
  theme_classic()

PLM_v <- data %>%
  ggplot(aes(x = PLM_v)) + 
  geom_histogram(mapping = aes(x = PLM_v, y=after_stat(density)),bins = 12, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$PLM_v, na.rm = TRUE), sd = sd(data$PLM_v, na.rm = TRUE))) +
  labs(x = "v") +
  theme_classic()

miniq

(BIS_V | BIS_F | BIS_N) / (BIS_PC | BIS_PS | BIS_M | BIS_C)

(WMC_MU | WMC_OpSp) / (WMC_SenSp | WMC_Binding)

(SMS_RT | CRT_RT | PLM_RT) / (SMS_v | CRT_v | PLM_v)
  

odd_even_cor <- 2*cor(data$miniq_even, data$miniq_odd)/(1+cor(data$miniq_even, data$miniq_odd))

# structural equation models ----

# (1) How predictive is mini-q performance of general cognitive abilities?

# latent correlation between mini-q performance and general cognitive abilities
model_cor <- '
          # measurement models
          g =~ BIS_V_z + BIS_F_z + BIS_N_z
          mini_q =~ miniq_1_z + miniq_2_z + miniq_3_z
          
          # latent correlation
          mini_q ~~ g

          # fix intercepts to zero
          BIS_V_z ~ 0
          BIS_F_z ~ 0
          BIS_N_z ~ 0
          miniq_1_z ~ 0
          miniq_2_z ~ 0
          miniq_3_z ~ 0'

fit_cor <- sem(model = model_cor, data = data, estimator = "ML" , missing="fiml")
summary(fit_cor, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedSolution(fit_cor)

# corrected for age
model_cor_age <- '
          # measurement models
          g =~ BIS_V_z + BIS_F_z + BIS_N_z
          mini_q =~ miniq_1_z + miniq_2_z + miniq_3_z
          
          # latent correlation
          mini_q ~~ g
          mini_q ~ age_z
          g ~ age_z

          # fix intercepts to zero
          BIS_V_z ~ 0
          BIS_F_z ~ 0
          BIS_N_z ~ 0
          miniq_1_z ~ 0
          miniq_2_z ~ 0
          miniq_3_z ~ 0
          age_z ~ 0'

fit_cor_age <- sem(model = model_cor_age, data = data, estimator = "ML" , missing="fiml")
summary(fit_cor_age, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedSolution(fit_cor_age)

# (2) To what degree do processing speed and working memory capacity account for
# the relationship between mini-q performance and general cognitive abilities?

# regression model without mediation
model_reg <- '
          # measurement models
          g =~ BIS_V_z + BIS_F_z + BIS_N_z
          mini_q =~ miniq_1_z + miniq_2_z + miniq_3_z
          
          
          # regression 
            g ~ mini_q
            
          # fix intercepts to zero
          BIS_V_z ~ 0
          BIS_F_z ~ 0
          BIS_N_z ~ 0
          miniq_1_z ~ 0
          miniq_2_z ~ 0
          miniq_3_z ~ 0
          '

fit_reg <- sem(model = model_reg, data = data, estimator = "ML" , missing="fiml")
summary(fit_reg, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedSolution(fit_med)

# mediation model with WMC and drift as mediators
model_med_1 <- '
          # measurement models
          g =~ BIS_V_z + BIS_F_z + BIS_N_z
          mini_q =~ miniq_1_z + miniq_2_z + miniq_3_z
          WMC =~ WMC_MU_z + WMC_Binding_z + WMC_OpSp_z + WMC_SenSp_z
          v =~ CRT_v_z + SMS_v_z + PLM_v_z
          WMC_OpSp_z ~~ WMC_SenSp_z
          
          # mediation 
            g ~ c*mini_q
            WMC ~ a1*mini_q
            v ~ a2*mini_q
            g ~ b1*WMC
            g ~ b2*v
            WMC ~~ v
            
          # indirect effect (a*b)
             ab1 := a1*b1
             ab2 := a2*b2
             
          # total effect
             total := c + (a1*b1) + (a2*b2)

          # fix intercepts to zero
          BIS_V_z ~ 0
          BIS_F_z ~ 0
          BIS_N_z ~ 0
          miniq_1_z ~ 0
          miniq_2_z ~ 0
          miniq_3_z ~ 0
          WMC_MU_z ~ 0
          WMC_Binding_z ~ 0
          WMC_OpSp_z ~ 0
          WMC_SenSp_z ~ 0
          CRT_v_z ~ 0
          SMS_v_z ~ 0
          PLM_v_z ~ 0'

fit_med_1 <- sem(model = model_med_1, data = data, estimator = "ML" , missing="fiml")
summary(fit_med_1, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedSolution(fit_med_1)

# mediation model with WMC and mean RT as mediators
model_med_2 <- '
          # measurement models
          g =~ BIS_V_z + BIS_F_z + BIS_N_z
          mini_q =~ miniq_1_z + miniq_2_z + miniq_3_z
          WMC =~ WMC_MU_z + WMC_Binding_z + WMC_OpSp_z + WMC_SenSp_z
          RT =~ CRT_RT_z + SMS_RT_z + PLM_RT_z
          WMC_OpSp_z ~~ WMC_SenSp_z
          
          # mediation 
            g ~ c*mini_q
            WMC ~ a1*mini_q
            RT ~ a2*mini_q
            g ~ b1*WMC
            g ~ b2*RT
            WMC ~~ RT
            
          # indirect effect (a*b)
             ab1 := a1*b1
             ab2 := a2*b2
             
          # total effect
             total := c + (a1*b1) + (a2*b2)

          # fix intercepts to zero
          BIS_V_z ~ 0
          BIS_F_z ~ 0
          BIS_N_z ~ 0
          miniq_1_z ~ 0
          miniq_2_z ~ 0
          miniq_3_z ~ 0
          WMC_MU_z ~ 0
          WMC_Binding_z ~ 0
          WMC_OpSp_z ~ 0
          WMC_SenSp_z ~ 0
          CRT_RT_z ~ 0
          SMS_RT_z ~ 0
          PLM_RT_z ~ 0'

fit_med_2 <- sem(model = model_med_2, data = data, estimator = "ML" , missing="fiml")
summary(fit_med_2, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedSolution(fit_med_2)

# (3) How do different operation-related components contribute to mini-q performance?

# multivariate regression model
model_mreg_op <- '
          # measurement models
          mini_q =~ miniq_1_z + miniq_2_z + miniq_3_z

          # multiple regression
          mini_q ~ BIS_PC_z + BIS_M_z + BIS_C_z + BIS_PS_z
          BIS_PC_z ~~ BIS_M_z + BIS_C_z + BIS_PS_z
          BIS_M_z ~~ BIS_C_z + BIS_PS_z
          BIS_C_z ~~ BIS_PS_z

          # fix intercepts to zero
          BIS_PC_z ~ 0
          BIS_M_z ~ 0
          BIS_C_z ~ 0
          BIS_PS_z ~ 0
          miniq_1_z ~ 0
          miniq_2_z ~ 0
          miniq_3_z ~ 0'

fit_mreg_op <- sem(model = model_mreg_op, data = data, estimator = "ML" , missing="fiml")
summary(fit_mreg_op, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedSolution(fit_mreg_op)

# modified multivariate regression model (regression path from memory and creativity fixed to 0)
model_mreg_op_mod1 <- '
          # measurement models
          mini_q =~ miniq_1_z + miniq_2_z + miniq_3_z

          # multiple regression
          mini_q ~ BIS_PC_z + 0*BIS_M_z + 0*BIS_C_z + BIS_PS_z
          BIS_PC_z ~~ BIS_M_z + BIS_C_z + BIS_PS_z
          BIS_M_z ~~ BIS_C_z + BIS_PS_z
          BIS_C_z ~~ BIS_PS_z

          # fix intercepts to zero
          BIS_PC_z ~ 0
          BIS_M_z ~ 0
          BIS_C_z ~ 0
          BIS_PS_z ~ 0
          miniq_1_z ~ 0
          miniq_2_z ~ 0
          miniq_3_z ~ 0'

fit_mreg_op_mod1 <- sem(model = model_mreg_op_mod1, data = data, estimator = "ML" , missing="fiml")
summary(fit_mreg_op_mod1, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedSolution(fit_mreg_op_mod1)

# comparison between the two multiple regression models
anova(fit_mreg_op, fit_mreg_op_mod1)

# modified multivariate regression model (regression path from memory and creativity fixed to 0
# and regressions weights of processing capacity and speed constrained to be equal)
model_mreg_op_mod2 <- '
          # measurement models
          mini_q =~ miniq_1_z + miniq_2_z + miniq_3_z

          # multiple regression
          mini_q ~ b*BIS_PC_z + 0*BIS_M_z + 0*BIS_C_z + b*BIS_PS_z
          BIS_PC_z ~~ BIS_M_z + BIS_C_z + BIS_PS_z
          BIS_M_z ~~ BIS_C_z + BIS_PS_z
          BIS_C_z ~~ BIS_PS_z

          # fix intercepts to zero
          BIS_PC_z ~ 0
          BIS_M_z ~ 0
          BIS_C_z ~ 0
          BIS_PS_z ~ 0
          miniq_1_z ~ 0
          miniq_2_z ~ 0
          miniq_3_z ~ 0'

fit_mreg_op_mod2 <- sem(model = model_mreg_op_mod2, data = data, estimator = "ML" , missing="fiml")
summary(fit_mreg_op_mod2, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedSolution(fit_mreg_op_mod2)

# comparison between the two multiple regression models
anova(fit_mreg_op_mod1, fit_mreg_op_mod2)

# (4) How do verbal, figural, and numerical abilities contribute to mini-q performance?

# multivariate regression model
model_mreg <- '
          # measurement models
          mini_q =~ miniq_1_z + miniq_2_z + miniq_3_z

          # multiple regression
          mini_q ~ BIS_V_z + BIS_F_z + BIS_N_z
          BIS_V_z ~~ BIS_F_z + BIS_N_z
          BIS_F_z ~~ BIS_N_z

          # fix intercepts to zero
          BIS_V_z ~ 0
          BIS_F_z ~ 0
          BIS_N_z ~ 0
          miniq_1_z ~ 0
          miniq_2_z ~ 0
          miniq_3_z ~ 0'

fit_mreg <- sem(model = model_mreg, data = data, estimator = "ML" , missing="fiml")
summary(fit_mreg, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedSolution(fit_mreg)

# modified multivariate regression model (regression path from numerical abilities fixed to 0)
model_mreg_mod1 <- '
          # measurement models
          mini_q =~ miniq_1_z + miniq_2_z + miniq_3_z
          BIS_N_lat =~ 1*BIS_N_z

          # multiple regression
          mini_q ~ BIS_V_z + BIS_F_z + 0*BIS_N_lat
          BIS_V_z ~~ BIS_F_z + BIS_N_lat
          BIS_F_z ~~ BIS_N_lat

          # fix intercepts to zero
          BIS_V_z ~ 0
          BIS_F_z ~ 0
          BIS_N_z ~ 0
          miniq_1_z ~ 0
          miniq_2_z ~ 0
          miniq_3_z ~ 0'

fit_mreg_mod1 <- sem(model = model_mreg_mod1, data = data, estimator = "ML" , missing="fiml")
summary(fit_mreg_mod1, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedSolution(fit_mreg1)

# comparison between the two multiple regression models
anova(fit_mreg, fit_mreg_mod1)

# modified multivariate regression model (regression path from numerical abilities fixed
# to 0 and regression weights of verbal and figural abilities constrained to be equal)
model_mreg_mod2 <- '
          # measurement models
          mini_q =~ miniq_1_z + miniq_2_z + miniq_3_z
          BIS_N_lat =~ 1*BIS_N_z

          # multiple regression
          mini_q ~ b*BIS_V_z + b*BIS_F_z + 0*BIS_N_lat
          BIS_V_z ~~ BIS_F_z + BIS_N_lat
          BIS_F_z ~~ BIS_N_lat

          # fix intercepts to zero
          BIS_V_z ~ 0
          BIS_F_z ~ 0
          BIS_N_z ~ 0
          miniq_1_z ~ 0
          miniq_2_z ~ 0
          miniq_3_z ~ 0'

fit_mreg_mod2 <- sem(model = model_mreg_mod2, data = data, estimator = "ML" , missing="fiml")
summary(fit_mreg_mod2, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedSolution(fit_mreg)

# model comparison
anova(fit_mreg_mod1, fit_mreg_mod2)

