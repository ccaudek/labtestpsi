##################################################################
## This R script accompanies the paper "Don't waste your time   ## 
## measuring intelligence: Further evidence for the validity    ## 
## of a three-minute speeded reasoning test" by Schubert  et    ##
## al. (2023) and contains the analysis code for Study 2.       ## 
##################################################################


# set working directory --------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# load packages ----------------------------------------------------------------
library(lavaan)
library(tidyverse)
library(psych)
library(effectsize)


# load data --------------------------------------------------------------------

# cleaned and tidy data of study 2, response-coded
data_study2 <- read.csv("data_study2.csv") 

# merged data of study 1 and 2, correct-coded
data_SEM_measurement_invariance <- read.csv("data_SEM_measurement_invariance.csv")

# data only of the HMT-S subsample, correct-coded
data_SEM_HMTS_correlation <- read.csv("data_SEM_HMTS_correlation.csv")


# descriptive statistics -------------------------------------------------------

# Overview over mini-q and HMT-S scores, times, age  
data_study2 %>% select(correct, miniqtime, HMTS_correct, HMTStime, age) %>%
  rename(miniq = correct, miniq_time = miniqtime, HMTS = HMTS_correct,
         HMTS_time = HMTStime, age = age) %>%
  psych::describe()


# mini-q scores distribution overall
ggplot(data = data_study2)+
  geom_histogram(mapping = aes(x = correct, y = ..density..), bins = 20, fill = "steelblue", color = "black")+
  stat_function(fun = dnorm, args = list(mean = mean(data_study2$correct), sd = sd(data_study2$correct)))+
  theme_classic()+
  xlim(c(0,64))+
  xlab("mini-q performance")+
  ylab("density")


# mini-q scores distribution young sample
data_study2_young <- data_study2 %>% filter(age_group == "young")
ggplot(data = data_study2_young)+
  geom_histogram(mapping = aes(x = correct, y = ..density..), bins = 20, fill = "steelblue", color = "black")+
  stat_function(fun = dnorm, args = list(mean = mean(data_study2_young$correct), sd = sd(data_study2_young$correct)))+
  theme_classic()+
  xlim(c(0,64))+
  xlab("mini-q performance")+
  ylab("density") +
  ggtitle("Young sample (18 to 30)") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16))


# mini-q scores distribution middle-aged sample
data_study2_old <- data_study2 %>% filter(age_group == "old")
ggplot(data = data_study2_old)+
  geom_histogram(mapping = aes(x = correct, y = ..density..), bins = 20, fill = "steelblue", color = "black")+
  stat_function(fun = dnorm, args = list(mean = mean(data_study2_old$correct), sd = sd(data_study2_old$correct)))+
  theme_classic()+
  xlim(c(0,64))+
  xlab("mini-q performance")+
  ylab("density") +
  ggtitle("Middle-aged sample (31 to 60)") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16))


# HMT-S scores distribution
ggplot(data= data_SEM_HMTS_correlation, mapping = aes(x = HMTS_correct)) +
  geom_bar(fill = "steelblue", color = "black") + 
  theme_classic() + 
  ylab("frequency") +
  xlab("HMT-S performance") +
  scale_x_continuous(breaks = c(0:6))


# item-wise analysis of study 2

## solution-vector for mini-q items
loesung_miniq <- c(0,0,0,1,0,0,1,1,0,0,1,0,1,0,0,1,0,0,1,1,1,1,0,1,1,1,1,0,1,0,0,1,
                   0,0,1,0,0,1,1,0,0,1,1,0,1,0,1,1,1,0,1,0,1,0,1,0,1,0,0,0,1,1,0,1)

## prepare data
df_itemstats <- as.data.frame(t(data_study2[,4:67])) # transpose dataframe 
for (i in 1:nrow(df_itemstats)){ # add variable "clicked"
  df_itemstats$clicked[i] <- sum(!is.na(df_itemstats[i,]))
}
for (i in 1:nrow(df_itemstats)){ # ad variable "correct" --> no. of correct responses per item
  df_itemstats$correct[i] <- sum(df_itemstats[i,] == loesung_miniq[i], na.rm = T)
}
df_itemstats <- df_itemstats %>% ## add variables "incorrect" and "item_nr"
                mutate(incorrect = nrow(data_study2) - correct, item_nr = sprintf("%02d", 1:64))

## plot
df_itemstats %>% select(correct, incorrect, item_nr) %>%
  pivot_longer(., cols = c(correct, incorrect),
               names_to = "result",
               values_to = "sum") %>%
  ggplot(data = ., aes(x = fct_rev(item_nr), y = sum, fill = result))+
  geom_col(position = position_stack(reverse = T))+
  scale_fill_manual(values = c("#02b5a6","#fa9691"))+
  coord_flip()+
  xlab("Item number")+
  ylab("Frequency (number of participants)")+
  labs(fill = "Result") +
  theme_minimal()



# reliability ------------------------------------------------------------------

# mini-q

## Cronbach's alpha
data_SEM_measurement_invariance %>% 
  filter(study == 2) %>%
  select(starts_with("item")) %>%
  psych::alpha(., warnings = F) %>%
  .$total %>%
  round(., 2)

## odd-even split
miniqitems_odd <- c(3+seq(from =1, to=63, by=2)) # columns of odd items
miniqitems_even <- c(3+seq(from = 2, to = 64, by =2)) # columns of even items

miniq_sum_odd <- rowSums(data_SEM_measurement_invariance[data_SEM_measurement_invariance$study == 2, 
                                                     miniqitems_odd]) 
miniq_sum_even <- rowSums(data_SEM_measurement_invariance[data_SEM_measurement_invariance$study == 2,
                                                      miniqitems_even])

odd_even_r <- cor(miniq_sum_odd, miniq_sum_even) # uncorrected odd-even correlation
(2*odd_even_r) / (1+odd_even_r) # Spearman-Brown corrected odd-even correlation


# HMT-S (Cronbach's alpha)
data_SEM_HMTS_correlation %>%
  select(starts_with("HMTS_0")) %>%
  psych::alpha(., warnings = F) %>%
  .$total %>%
  round(., 2)
  

# demographics -----------------------------------------------------------------

# gender
table(data_study2$gender)


# is german the native language?
table(factor(data_study2$native_language_german, labels = c("fluent in German", "native speaker")))


# languages spoken besides German
data_study2$p_Fluent_languages %>%
  strsplit(., ",") %>% 
  unlist() %>% 
  str_squish(.) %>% 
  table(., exclude = NULL) %>%
  as.data.frame() %>%
  filter(. != "German") %>%
  arrange(desc(Freq))


# highest level of education 

## data preperation
data_study2$education <- factor(data_study2$education, 
                                levels = c("still a student",
                                           "secondary general school", "secondary school", 
                                           "apprenticeship", 
                                           "university entrance qualification \nfor applied sciences",
                                           "university entrance qualification", 
                                           "bachelor's degree", 
                                           "master's degree", "PhD", "other"), ordered = T)

## histogram
ggplot(data = data_study2, aes(x = education))+
  geom_bar(fill = "steelblue", color = "black")+
  theme_classic()+
  ggtitle("Highest level of education")+
  ylab("frequency") +
  scale_y_continuous(breaks=seq(0,300,by=50)) +
  theme(axis.text.x = element_text(size = 11, angle = 55, vjust = 1, hjust=1),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_blank())
## table
table(data_study2$education) 


# current employment status 

## histogram
ggplot(data = data_study2, aes(x = forcats::fct_infreq(job)))+
  geom_bar(fill = "steelblue", color = "black")+
  theme_classic()+
  ggtitle("Current employment status")+
  ylab("frequency") +
  scale_y_continuous(breaks=seq(0,300,by=50)) +
  theme(axis.text.x = element_text(size = 11, angle = 55, vjust = 1.0, hjust=1),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_blank())
## table
table(data_study2$job)


# ethnicity
table(data_study2$p_Ethnicity_simplified, exclude = NULL)


# norm tables ------------------------------------------------------------------

# young sample

## create dataframe
norm_young <- data.frame(matrix(NA, nrow = length(unique(data_study2_young$correct)), ncol = 2))
names(norm_young) <- c("raw_values", "IQ_values")

## get unique values
norm_young$raw_values <- sort(unique(data_study2_young$correct))

## calculate IQ-scores
norm_young %>% mutate(IQ_values = ((raw_values- mean(data_study2_young$correct))/
                                    sd(data_study2_young$correct)) * 15 + 100) %>%
               mutate(IQ_values = round(IQ_values))


# middle-aged sample

## create dataframe
norm_old <- data.frame(matrix(NA, nrow = length(unique(data_study2_old$correct)), ncol = 2))
names(norm_old) <- c("raw_values", "IQ_values")

## get unique values
norm_old$raw_values <- sort(unique(data_study2_old$correct))

## calculate IQ-scores
norm_old %>% mutate(IQ_values = ((raw_values- mean(data_study2_old$correct))/
                                  sd(data_study2_old$correct)) * 15 + 100) %>%
             mutate(IQ_values = round(IQ_values))


# overall sample

## create dataframe
norm_all <- data.frame(matrix(NA, nrow = length(unique(data_study2$correct)), ncol = 2))
names(norm_all) <- c("raw_values", "IQ_values")

## get unique values
norm_all$raw_values <- sort(unique(data_study2$correct))

## calculate IQ-scores
norm_all %>% mutate(IQ_values = ((raw_values- mean(data_study2$correct))/
                                  sd(data_study2$correct)) * 15 + 100) %>%
             mutate(IQ_values = round(IQ_values))


# zero-order correlations ------------------------------------------------------

# correlation of mini-q with age 
cor.test(data_study2$age, data_study2$correct)

# t-test of performance between age-groups 
t.test(data_study2[data_study2$age_group=="young",]$correct, 
       data_study2[data_study2$age_group=="old",]$correct, 
       var.equal = TRUE)

cohens_d(correct ~ age_group, data = data_study2)

# zero-order correlations between mini-q performance, HMT-S performance, the three 
# mini-q parcels containing every third mini-q item, respectively, and the single
# HMT-S items ==> all variables used in the second structural equation model assessing
# the latent correlation between mini-q and HMT-S performance

data_SEM_HMTS_correlation %>%
  mutate(miniq = correct,
         HMTS = HMTS_correct) %>%
  select(miniq, HMTS, miniq_c1, miniq_c2, miniq_c3, starts_with("HMTS_0")) %>%
  cor() %>% 
  round(.,2)


# structural equation models ---------------------------------------------------

# (1) measurement invariance models for mini-q parcels in study 1 (lab study) 
#     and study 2 (online study)

model_inv <- '
          # measurement models
          mini_q =~ NA * miniq_m1 + miniq_m2 + miniq_m3 + miniq_m4
            '

## configural measurement invariance
fit_configural <- sem(model = model_inv, data = data_SEM_measurement_invariance, 
                      estimator = "ML" , missing="fiml", group = "study", std.lv = T)

summary(fit_configural, fit.measures=TRUE,standardized = TRUE,rsquare=T)

## metric measurement invariance
fit_metric <- sem(model = model_inv, data = data_SEM_measurement_invariance, 
                  estimator = "ML" , missing="fiml", group = "study",
                  group.equal = c("loadings"), std.lv = T)

summary(fit_metric, fit.measures=TRUE,standardized = TRUE,rsquare=T)

## scalar measurement invariance
fit_scalar <- sem(model = model_inv, data = data_SEM_measurement_invariance, 
                  estimator = "ML" , missing="fiml", group = "study",
                  group.equal = c("loadings", "intercepts"), std.lv = T)

summary(fit_scalar, fit.measures=TRUE,standardized = TRUE,rsquare=T)

## ANOVA for model comparison
anova(fit_configural, fit_metric, fit_scalar)



# (2) latent correlation between mini-q performance and HMT-S performance

model_cor <- '
            #measurement models

            mini_q =~ miniq_c1 + miniq_c2 + miniq_c3
            HMT =~ HMTS_01 + HMTS_02 + HMTS_03 + HMTS_04 + HMTS_05 + HMTS_06

            #correlation
            mini_q ~~ HMT'

fit_cor <- sem(model = model_cor , data = data_SEM_HMTS_correlation,
               ordered = c("HMTS_01","HMTS_02", "HMTS_03","HMTS_04", "HMTS_05", "HMTS_06"))
summary(fit_cor, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedSolution(fit_cor)



