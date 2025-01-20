library(lavaan)
library(semPlot)

# Correlation matrix from the description
cor_matrix <- matrix(c(
  1.000, -0.34,  -0.26, -0.35,  # 16-PF Warmth
  -0.34,  1.000,   0.56,  0.27,  # TAS Total
  -0.26,  0.56,   1.000,  0.26,  # SCL-90-R Hostility
  -0.35,  0.27,   0.26,  1.000   # CBA2.0 Suicidal Ideation
), nrow = 4, ncol = 4, byrow = TRUE)

# Variable names
colnames(cor_matrix) <- rownames(cor_matrix) <- c("Warmth", "Alexithymia", "Hostility", "SuicidalIdeation")

# Specify the path model
model <- '
  # Direct effects
  Alexithymia ~ b1 * Warmth
  Hostility ~ b2 * Warmth + b3 * Alexithymia
  SuicidalIdeation ~ b4 * Hostility

  # Indirect effects
  Warmth_to_Hostility_via_Alexithymia := b1 * b3
  Warmth_to_SuicidalIdeation_via_Hostility := b1 * b3 * b4
  Alexithymia_to_SuicidalIdeation_via_Hostility := b3 * b4
'

# Fit the model using the correlation matrix
fit <- sem(model, sample.cov = cor_matrix, sample.nobs = 190) # Replace 1000 with your sample size

# Summarize results
summary(fit, standardized = TRUE, fit.measures = TRUE)

# Plot the path diagram
library(semPlot)
semPaths(
  fit,
  what = "std",          # Standardized estimates
  whatLabels = "std",    # Show standardized values as edge labels
  style = "ram",         # Style of the diagram
  layout = "tree",       # Organize layout in a tree structure
  edge.label.cex = 1.2,  # Adjust label size
  sizeMan = 7,           # Size of manifest variables
  nCharNodes = 8         # Allow full variable names
)
