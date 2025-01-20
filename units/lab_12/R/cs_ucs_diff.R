#' Idiographic analysis of Neff's hypothesis



# Setup -------------------------------------------------------------------

# Load packages
if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  lavaan, here, tictoc, rio, tidyverse, cmdstanr, posterior,
  bayesplot, purrr, vars, forecast, cluster
)

# Set seed
set.seed(42)

# Load helper functions
source(here::here("units", "lab_12", "R", "importing_cleaning_data.R"))


# Import and clean data ---------------------------------------------------

df <- rio::import(
  here::here(
    "units", "lab_12", "R", "public_study_1_data.csv"
  )
)

library(lavaan)

model <- "
  # Measurement model
  CS =~ scs_pos_1 + scs_pos_3 + scs_pos_6 + scs_pos_7
  UCS =~ scs_neg_2 + scs_neg_4 + scs_neg_5 + scs_neg_8

  # Regressions of latent variables
  CS ~ neg_aff
  UCS ~ neg_aff

  # Correlations between latent factors
  CS ~~ UCS
"


# Fit the model
fit <- sem(model, data = df, estimator = "MLR", missing = "fiml")

# Summary of the results
summary(fit, fit.measures = TRUE, standardized = TRUE)




# Visualize model fit
library(semPlot)
semPaths(fit, "standardized", edge.label.cex = 0.8, layout = "tree")


df_wide <- df %>%
  group_by(user_id) %>% # Group by participant ID
  arrange(user_id, time_window) %>% # Ensure measurements are ordered correctly
  mutate(occ = row_number()) %>% # Create a unique number for each measurement occasion
  pivot_wider(
    names_from = occ, # Create wide columns based on occasion numbers
    values_from = c(
      neg_aff, scs_pos_1, scs_pos_3, scs_pos_6, scs_pos_7,
      scs_neg_2, scs_neg_4, scs_neg_5, scs_neg_8
    ) # Specify columns to widen
  )


df <- df %>%
  mutate(
    CS = rowMeans(dplyr::select(., scs_pos_1, scs_pos_3, scs_pos_6, scs_pos_7), na.rm = TRUE),
    UCS = rowMeans(dplyr::select(., scs_neg_2, scs_neg_4, scs_neg_5, scs_neg_8), na.rm = TRUE)
  )

df_wide <- df %>%
  group_by(user_id) %>%
  arrange(user_id, time_window) %>%
  mutate(occ = row_number()) %>%
  pivot_wider(
    names_from = occ,
    values_from = c(neg_aff, CS, UCS)
  )


# Inspect the covariance matrix of latent variables
lavInspect(fit_lgm, "cov.lv")






df <- get_data(reverse_coding_ucs = 0)

length(unique(df$user_id))
# [1] 495


# Step 1: Randomly select three measurements per subject
set.seed(123) # For reproducibility
df_selected <- df %>%
  group_by(user_id) %>%
  sample_n(3) %>% # Randomly select 3 rows per user
  arrange(user_id, day, time_window) %>% # Sort for consistent order
  ungroup()

# Step 2: Compute CS and UCS scores
df_selected <- df_selected %>%
  mutate(
    CS = rowMeans(dplyr::select(., scs_pos_1, scs_pos_2, scs_pos_3, scs_pos_4), na.rm = TRUE),
    UCS = rowMeans(dplyr::select(., scs_neg_1, scs_neg_2, scs_neg_3, scs_neg_4), na.rm = TRUE)
  )

# Step 3: Select the relevant Negative Affect measure for each occasion
# In this example, we'll use neg_aff_Moment as the primary predictor
df_selected <- df_selected %>%
  dplyr::select(user_id, day, time_window, CS, UCS, neg_aff_Moment)

# Check the resulting data structure
str(df_selected)


# Step 4: Convert to wide format
df_wide <- df_selected %>%
  # Create a row number for each measurement occasion per user
  group_by(user_id) %>%
  mutate(occasion = row_number()) %>%
  ungroup() %>%
  # Pivot to wide format
  pivot_wider(
    id_cols = user_id,
    names_from = occasion,
    values_from = c(CS, UCS, neg_aff_Moment),
    names_sep = "_T"
  )

df_wide <- rio::import(
  here::here(
    "units", "lab_12", "R", "ema_state_self_compassion_data.csv"
  )
)

# Check the resulting structure
str(df_wide)

# Calculate the difference between CS and UCS for each time point
df_wide <- df_wide %>%
  mutate(
    Diff_T1 = CS_T1 - UCS_T1,
    Diff_T2 = CS_T2 - UCS_T2,
    Diff_T3 = CS_T3 - UCS_T3
  )

model <- "
  # Predictive paths from Negative Affect
  Diff_T1 ~ c1*neg_aff_Moment_T1
  Diff_T2 ~ c2*neg_aff_Moment_T2 + a*Diff_T1
  Diff_T3 ~ c3*neg_aff_Moment_T3 + a*Diff_T2

  # Covariances among difference scores at each time point
  Diff_T1 ~~ Diff_T2
  Diff_T1 ~~ Diff_T3
  Diff_T2 ~~ Diff_T3
"

fit <- sem(model, data = df_wide)
summary(fit, standardized = TRUE)

fitMeasures(fit)
