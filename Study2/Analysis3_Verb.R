library(tidyverse)
library(cmdstanr)
library(rstan)
library(loo)
library(here)
library(ggmcmc)
options(mc.cores = parallel::detectCores())

# read raw data
d <- read_csv(here("Study2", "data_study2.csv"), col_types = "cddcccdddddcdddd")

# check the data exclusion criteria
d %>% 
  group_by(id, time) %>% 
  summarise(Trial_Total = sum(!is.na(PropLook_Correct)),
            Trial_Right = sum(!is.na(PropLook_Correct) & PropLook_Right >= 0.5),
            RightRatio = Trial_Right/Trial_Total,
            .groups = "drop") %>% 
  mutate(RightOnly = if_else(RightRatio == 1 | RightRatio == 0, 1, 0)) -> d_side

# check participants who looked only to the left or right
filter(d_side, RightOnly == 1)
         
# check participants without pointing
filter(d_side, Trial_Total == 0)

# data exclusion
d_side %>% 
  filter(RightOnly == 1 | Trial_Total == 0) %>% 
  pull(id) -> ID_exclude

# Data shaping
d %>% 
  filter(!id %in% ID_exclude) %>% 
  group_by(id, time) %>% 
  summarise(noun = first(noun),
            verb = first(verb),
            .groups = "drop") %>% 
  pivot_longer(c("noun", "verb"), names_to = "category") %>% 
  pivot_wider(names_from = c("category", "time"), names_sep = "") %>% 
  mutate(diff_noun = noun2 - noun1,
         diff_verb = verb2 - verb1) %>% 
  select(id, starts_with("diff")) -> d_word

d %>% 
  filter(!id %in% ID_exclude) %>%
  filter(time == 1) %>% 
  group_by(id, Condition) %>% 
  summarise(AgeM = first(ageM),
            Trial_Total = sum(!is.na(PropLook_Correct)),
            Trial_Correct = sum(Correct_Looking, na.rm = TRUE),
            Prop = if_else(Trial_Total != 0, Trial_Correct/Trial_Total, NA_real_),
            .groups = "drop") %>%
  select(!Trial_Correct) %>% 
  pivot_wider(names_from = "Condition", values_from = "Prop") %>% 
  left_join(d_word, by = "id") -> df

# MCMC sampling setting
iter_set <- 20000
warmup_set <- 2000

# 5 fixed effects
## Model 1: diff_words ~ Match + Mis + ObjDiff + ObjAbs
### make a data list for stan
formula1 <- "diff_words ~ Match + Mis + ObjDiff + ObjAbs"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             X2 = df$Match,
             X3 = df$Mis,
             X4 = df$ObjDiff,
             X5 = df$ObjAbs)

## MCMC sampling with cmdstanr
here("Study2", "Models", "VocabularyGrowth", "model_5FixedEffects.stan") %>%
  cmdstan_model() -> model_5fixed

fit1 <- model_5fixed$sample(data = data,
                            chains = 4,
                            iter_warmup = warmup_set,
                            iter_sampling = iter_set,
                            adapt_delta = 0.8, 
                            max_treedepth = 15,
                            seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model1")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit1$save_output_files(dir = output_dir, basename = "model1")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit1
print(fit1, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit1, merge_chains = FALSE)
waic1 <- waic(log_lik)
print(waic1, digits=2)


# 4 fixed effects
## Model 2: diff_words ~ Match + Mis + ObjDiff
### make a data list for stan
formula2 <- "diff_words ~ Match + Mis + ObjDiff"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             X2 = df$Match,
             X3 = df$Mis,
             X4 = df$ObjDiff)
             # X5 = df$ObjAbs)

## MCMC sampling with cmdstanr
here("Study2", "Models", "VocabularyGrowth", "model_4FixedEffects.stan") %>%
  cmdstan_model() -> model_4fixed

fit2 <- model_4fixed$sample(data = data,
                            chains = 4,
                            iter_warmup = warmup_set,
                            iter_sampling = iter_set,
                            adapt_delta = 0.8, 
                            max_treedepth = 15,
                            seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model2")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit2$save_output_files(dir = output_dir, basename = "model2")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit2
print(fit2, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit2, merge_chains = FALSE)
waic2 <- waic(log_lik)
print(waic2, digits=2)


## Model 3: diff_words ~ Match + Mis + ObjAbs
### make a data list for stan
formula3 <- "diff_words ~ Match + Mis + ObjAbs"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             X2 = df$Match,
             X3 = df$Mis,
             # X4 = df$ObjDiff
             X4 = df$ObjAbs)

## MCMC sampling with cmdstanr
fit3 <- model_4fixed$sample(data = data,
                            chains = 4,
                            iter_warmup = warmup_set,
                            iter_sampling = iter_set,
                            adapt_delta = 0.8, 
                            max_treedepth = 15,
                            seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model3")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit3$save_output_files(dir = output_dir, basename = "model3")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit3
print(fit3, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit3, merge_chains = FALSE)
waic3 <- waic(log_lik)
print(waic3, digits=2)


## Model 4: diff_words ~ Match + ObjDiff + ObjAbs
### make a data list for stan
formula4 <- "diff_words ~ Match + ObjDiff + ObjAbs"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             X2 = df$Match,
             # X3 = df$Mis,
             X3 = df$ObjDiff,
             X4 = df$ObjAbs)

## MCMC sampling with cmdstanr
fit4 <- model_4fixed$sample(data = data,
                            chains = 4,
                            iter_warmup = warmup_set,
                            iter_sampling = iter_set,
                            adapt_delta = 0.8, 
                            max_treedepth = 15,
                            seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model4")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit4$save_output_files(dir = output_dir, basename = "model4")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit4
print(fit4, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit4, merge_chains = FALSE)
waic4 <- waic(log_lik)
print(waic4, digits=2)


## Model 5: diff_words ~ Mis + ObjDiff + ObjAbs
### make a data list for stan
formula5<- "diff_words ~ Mis + ObjDiff + ObjAbs"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             # X2 = df$Match,
             X2 = df$Mis,
             X3 = df$ObjDiff,
             X4 = df$ObjAbs)

## MCMC sampling with cmdstanr
fit5 <- model_4fixed$sample(data = data,
                            chains = 4,
                            iter_warmup = warmup_set,
                            iter_sampling = iter_set,
                            adapt_delta = 0.8, 
                            max_treedepth = 15,
                            seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model5")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit5$save_output_files(dir = output_dir, basename = "model5")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit5
print(fit5, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit5, merge_chains = FALSE)
waic5 <- waic(log_lik)
print(waic5, digits=2)


# 3 fixed effects
## Model 6: diff_words ~ Match + Mis
### make a data list for stan
formula6 <- "diff_words ~ Match + Mis"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             X2 = df$Match,
             X3 = df$Mis)
             # X3 = df$ObjDiff,
             # X4 = df$ObjAbs)

## MCMC sampling with cmdstanr
here("Study2", "Models", "VocabularyGrowth", "model_3FixedEffects.stan") %>%
  cmdstan_model() -> model_3fixed
fit6 <- model_3fixed$sample(data = data,
                            chains = 4,
                            iter_warmup = warmup_set,
                            iter_sampling = iter_set,
                            adapt_delta = 0.8, 
                            max_treedepth = 15,
                            seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model6")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit6$save_output_files(dir = output_dir, basename = "model6")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit6
print(fit6, pars = c("beta0", "beta1", "beta2", "beta3", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit6, merge_chains = FALSE)
waic6 <- waic(log_lik)
print(waic6, digits=2)

## Model 7: diff_words ~ Match + ObjDiff
### make a data list for stan
formula7 <- "diff_words ~ Match + ObjDiff"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             X2 = df$Match,
             # X3 = df$Mis,
             X3 = df$ObjDiff)
             # X4 = df$ObjAbs)

## MCMC sampling with cmdstanr
fit7 <- model_3fixed$sample(data = data,
                            chains = 4,
                            iter_warmup = warmup_set,
                            iter_sampling = iter_set,
                            adapt_delta = 0.8, 
                            max_treedepth = 15,
                            seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model7")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit7$save_output_files(dir = output_dir, basename = "model7")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit7
print(fit7, pars = c("beta0", "beta1", "beta2", "beta3", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit7, merge_chains = FALSE)
waic7 <- waic(log_lik)
print(waic7, digits=2)

## Model 8: diff_words ~ Match + ObjAbs
### make a data list for stan
formula8 <- "diff_words ~ Match + ObjAbs"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             X2 = df$Match,
             # X3 = df$Mis,
             # X3 = df$ObjDiff)
             X3 = df$ObjAbs)

## MCMC sampling with cmdstanr
fit8 <- model_3fixed$sample(data = data,
                            chains = 4,
                            iter_warmup = warmup_set,
                            iter_sampling = iter_set,
                            adapt_delta = 0.8, 
                            max_treedepth = 15,
                            seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model8")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit8$save_output_files(dir = output_dir, basename = "model8")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit8
print(fit8, pars = c("beta0", "beta1", "beta2", "beta3", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit8, merge_chains = FALSE)
waic8 <- waic(log_lik)
print(waic8, digits=2)


## Model 9: diff_words ~ Mis + ObjDiff
### make a data list for stan
formula9 <- "diff_words ~ Mis + ObjDiff"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             # X2 = df$Match,
             X2 = df$Mis,
             X3 = df$ObjDiff)
             # X3 = df$ObjAbs)

## MCMC sampling with cmdstanr
fit9 <- model_3fixed$sample(data = data,
                            chains = 4,
                            iter_warmup = warmup_set,
                            iter_sampling = iter_set,
                            adapt_delta = 0.8, 
                            max_treedepth = 15,
                            seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model9")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit9$save_output_files(dir = output_dir, basename = "model9")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit9
print(fit9, pars = c("beta0", "beta1", "beta2", "beta3", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit9, merge_chains = FALSE)
waic9 <- waic(log_lik)
print(waic9, digits=2)


## Model 10: diff_words ~ Mis + ObjAbs
### make a data list for stan
formula10 <- "diff_words ~ Mis + ObjAbs"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             # X2 = df$Match,
             X2 = df$Mis,
             # X3 = df$ObjDiff)
             X3 = df$ObjAbs)

## MCMC sampling with cmdstanr
fit10 <- model_3fixed$sample(data = data,
                             chains = 4,
                             iter_warmup = warmup_set,
                             iter_sampling = iter_set,
                             adapt_delta = 0.8, 
                             max_treedepth = 15,
                             seed = 1234)
 
output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model10")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit10$save_output_files(dir = output_dir, basename = "model10")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit10
print(fit10, pars = c("beta0", "beta1", "beta2", "beta3", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit10, merge_chains = FALSE)
waic10 <- waic(log_lik)
print(waic10, digits=2)


## Model 11: diff_words ~ ObjDiff + ObjAbs
### make a data list for stan
formula11 <- "diff_words ~ ObjDiff + ObjAbs"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             # X2 = df$Match,
             # X2 = df$Mis,
             X2 = df$ObjDiff,
             X3 = df$ObjAbs)

## MCMC sampling with cmdstanr
fit11 <- model_3fixed$sample(data = data,
                             chains = 4,
                             iter_warmup = warmup_set,
                             iter_sampling = iter_set,
                             adapt_delta = 0.8, 
                             max_treedepth = 15,
                             seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model11")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit11$save_output_files(dir = output_dir, basename = "model11")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit11
print(fit11, pars = c("beta0", "beta1", "beta2", "beta3", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit11, merge_chains = FALSE)
waic11 <- waic(log_lik)
print(waic11, digits=2)


# 2 fixed effects
## Model 12: diff_words ~ Match
### make a data list for stan
formula12 <- "diff_words ~ Match"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             X2 = df$Match)

## MCMC sampling with cmdstanr
here("Study2", "Models", "VocabularyGrowth", "model_2FixedEffects.stan") %>%
  cmdstan_model() -> model_2fixed
fit12 <- model_2fixed$sample(data = data,
                             chains = 4,
                             iter_warmup = warmup_set,
                             iter_sampling = iter_set,
                             adapt_delta = 0.8, 
                             max_treedepth = 15,
                             seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model12")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit12$save_output_files(dir = output_dir, basename = "model12")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit12
print(fit12, pars = c("beta0", "beta1", "beta2", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit12, merge_chains = FALSE)
waic12 <- waic(log_lik)
print(waic12, digits=2)


## Model 13: diff_words ~ Mis
### make a data list for stan
formula13 <- "diff_words ~ Mis"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             X2 = df$Mis)

## MCMC sampling with cmdstanr
fit13 <- model_2fixed$sample(data = data,
                             chains = 4,
                             iter_warmup = warmup_set,
                             iter_sampling = iter_set,
                             adapt_delta = 0.8, 
                             max_treedepth = 15,
                             seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model13")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit13$save_output_files(dir = output_dir, basename = "model13")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit13
print(fit13, pars = c("beta0", "beta1", "beta2", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit13, merge_chains = FALSE)
waic13 <- waic(log_lik)
print(waic13, digits=2)


## Model 14: diff_words ~ ObjDiff
### make a data list for stan
formula14 <- "diff_words ~ ObjDiff"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             X2 = df$ObjDiff)

## MCMC sampling with cmdstanr
fit14 <- model_2fixed$sample(data = data,
                             chains = 4,
                             iter_warmup = warmup_set,
                             iter_sampling = iter_set,
                             adapt_delta = 0.8, 
                             max_treedepth = 15,
                             seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model14")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit14$save_output_files(dir = output_dir, basename = "model14")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit14
print(fit14, pars = c("beta0", "beta1", "beta2", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit14, merge_chains = FALSE)
waic14 <- waic(log_lik)
print(waic14, digits=2)


## Model 15: diff_words ~ ObjAbs
### make a data list for stan
formula15 <- "diff_words ~ ObjAbs"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             X2 = df$ObjAbs)

## MCMC sampling with cmdstanr
fit15 <- model_2fixed$sample(data = data,
                             chains = 4,
                             iter_warmup = warmup_set,
                             iter_sampling = iter_set,
                             adapt_delta = 0.8, 
                             max_treedepth = 15,
                             seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model15")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit15$save_output_files(dir = output_dir, basename = "model15")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit15
print(fit15, pars = c("beta0", "beta1", "beta2", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit15, merge_chains = FALSE)
waic15 <- waic(log_lik)
print(waic15, digits=2)


# 1 fixed effect
## Model 16: diff_words ~ 1
### make a data list for stan
formula16 <- "diff_words ~ 1"
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0))

## MCMC sampling with cmdstanr
here("Study2", "Models", "VocabularyGrowth", "model_1FixedEffect.stan") %>%
  cmdstan_model() -> model_1fixed
fit16 <- model_1fixed$sample(data = data,
                             chains = 4,
                             iter_warmup = warmup_set,
                             iter_sampling = iter_set,
                             adapt_delta = 0.8, 
                             max_treedepth = 15,
                             seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model16")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit16$save_output_files(dir = output_dir, basename = "model16")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit16
print(fit16, pars = c("beta0", "beta1", "sigma"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit16, merge_chains = FALSE)
waic16 <- waic(log_lik)
print(waic16, digits=2)


# model selection
rbind(waic1$estimates,
      waic2$estimates,
      waic3$estimates,
      waic4$estimates,
      waic5$estimates,
      waic6$estimates,
      waic7$estimates,
      waic8$estimates,
      waic9$estimates,
      waic10$estimates,
      waic11$estimates,
      waic12$estimates,
      waic13$estimates,
      waic14$estimates,
      waic15$estimates,
      waic16$estimates) %>% 
  cbind(Model = rep(str_c("model", 1:16), each = 3)) %>%
  cbind(Formula = rep(c(formula1, formula2, formula3, formula4, formula5,
                        formula6, formula7, formula8, formula9, formula10,
                        formula11, formula12, formula13, formula14, formula15, formula16), each = 3)) %>%
  cbind(Measure = c("elpd_waic", "p_waic", "waic")) %>% 
  as_tibble() -> df_waic

df_waic %>% 
  filter(Measure == "waic") %>%
  relocate(Model, Formula, Measure) %>% 
  arrange(Estimate) 

# selected model
## make a data list for stan
expand(df, Match, Mis, ObjAbs, ObjDiff) %>% 
  mutate(id = row_number())-> df_new
data <- list(Nrow = nrow(df),
             diff_words = df$diff_verb,
             X1 = if_else(df$AgeM == 19, 1, 0),
             X2 = df$Match,
             X3 = df$Mis,
             X4 = df$ObjDiff,
             X5 = df$ObjAbs,
             N_pred = nrow(df_new),
             X2_pred = df_new$Match,
             X3_pred = df_new$Mis,
             X4_pred = df_new$ObjDiff,
             X5_pred = df_new$ObjAbs)

## MCMC sampling with cmdstanr
here("Study2", "Models", "VocabularyGrowth", "model_selected.stan") %>%
  cmdstan_model() -> model
fit <- model$sample(data = data,
                    chains = 4,
                    iter_warmup = warmup_set,
                    iter_sampling = iter_set,
                    adapt_delta = 0.8, 
                    max_treedepth = 15,
                    seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model_selected")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit$save_output_files(dir = output_dir, basename = "model_selected")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit
print(fit, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "sigma"), probs = c(0.5, 0.025, 0.975)) # Table 3

## visualization
ggs(fit) %>% 
  filter(str_detect(Parameter, pattern = "lambda_pred")) %>% 
  group_by(Parameter) %>% 
  summarise(MED = median(value),
            lwr = quantile(value, probs = 0.025),
            upr = quantile(value, probs = 0.975)) %>% 
  mutate(id = as.numeric(str_extract(Parameter, pattern = "[[:digit:]]+")),
         Category = "Verbs") %>% 
  left_join(df_new, by = "id") -> df_predict

df %>% pivot_longer(c("diff_noun", "diff_verb"), names_to="Category", values_to="Value") %>% 
  group_by(Category) %>% 
  summarise(N = n(),
            Mean = mean(Value),
            SD = sd(Value),
            Min = min(Value),
            Max = max(Value))

df_predict %>% 
  filter(ObjDiff == 0.5, ObjAbs == 0.5) %>% 
  filter(Match >= 0.5) %>% 
  mutate(Match = as.character(Match)) %>% 
  ggplot(aes(x = Mis)) +
  geom_jitter(data = df, aes(y = diff_verb), color="#4C0A5B", shape=16, size=4, width=0.02, height=0.2, alpha=0.3) +
  geom_line(aes(y = MED, color = Match, group = Match), lwd = 2.3) +
  scale_y_continuous(limits=c(-0.2,23), breaks=seq(0,22,3))+
  scale_x_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25))+
  scale_color_manual(values = c("0.5" = "#DB7FF1", "0.75" = "#AF17D3", "1" = "#4C0A5B"),
                     labels = c("0.5" = "0.50", "0.75" = "0.75", "1" = "1.00"))+
  guides(color = guide_legend(reverse = TRUE)) +
  facet_wrap(~Category) +
  theme_bw() +
  theme(axis.ticks = element_line(color = "black"),
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.position = c(0.08, 0.96),
        legend.justification = c(0.1, 1),
        legend.background = element_rect(colour = "gray30")) +
  labs(x = "Proportion of correct responses (looking) \n in mismatch condition at 1st time",
       y = "Vocabulary growth",
       color = "Match condition \n at 1st time") -> gp
print(gp)
output_fig_dir <- here("Study2", "Figures")
if (!dir.exists(output_fig_dir)){
  dir.create(output_fig_dir, recursive = TRUE)
}
ggsave(file = here("Study2", "Figures", "Analysis3_Verb.jpg"), plot = gp, dpi = 350, width = 4.1, height = 4.8) # Figure 4


# standardization of the best model
## make a data list for stan
df_st <- scale(df[2:7]) %>% as.data.frame() %>% mutate(diff_noun=df$diff_noun, diff_verb=df$diff_verb)
data <- list(Nrow = nrow(df_st),
             diff_words = df_st$diff_verb,
             X1 = df_st$AgeM,
             X2 = df_st$Match,
             X3 = df_st$Mis,
             X4 = df_st$ObjDiff,
             X5 = df_st$ObjAbs)

## MCMC sampling with cmdstanr
here("Study2", "Models", "VocabularyGrowth", "model_selected_standardized.stan") %>%
  cmdstan_model() -> model
fit <- model$sample(data = data,
                    chains = 4,
                    iter_warmup = warmup_set,
                    iter_sampling = iter_set,
                    adapt_delta = 0.8, 
                    max_treedepth = 15,
                    seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis3_Verb", "model_selected_standardized")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit$save_output_files(dir = output_dir, basename = "model_selected_standardized")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit
print(fit, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "sigma"), probs = c(0.5, 0.025, 0.975)) # Table 3

## calculate WAIC
log_lik <- extract_log_lik(fit, merge_chains = FALSE)
waic_st <- waic(log_lik)
print(waic_st, digits=2)
