library("lavaan")

lower <- '
   1
   .38 1
   .41 .64 1
   .34 .44 .30 1
   .29 .12 .27 .06 1
   .29 .22 .20 .17 .54 1
   .30 .15 .23 .09 .73 .69 1
'

dat_cov <- lavaan::getCov(
  lower, 
  names=c("dmis","con", "dep", "consu", "acc", "goal", "imp"))
dat_cov

mod <- '
  drpe =~ NA*acc + goal + imp
  amis =~ NA*con + dep + consu
  dmis ~~ amis
  amis ~ drpe
  dmis ~ drpe
  drpe ~~ 1*drpe
  amis ~~ 1*amis
'

fit <- lavaan::sem(mod, sample.cov = dat_cov, sample.nobs = 284)

summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


mod_alt <- '
  drpe =~ NA*acc + goal + imp
  amis =~ NA*con + dep + consu
  dmis ~~ amis
  drpe ~ amis + dmis
  drpe ~~ 1*drpe
  amis ~~ 1*amis
'

fit_alt <- sem(mod_alt, sample.cov = dat_cov, sample.nobs = 311)
summary(fit_alt, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
