library(tidyverse)
library(cmdstanr)
library(rstan)
library(loo)
library(here)
library(ggmcmc)
options(mc.cores = parallel::detectCores())

# read raw data
d <- read_csv(here("Study1", "data_study1.csv"), col_types = "cdcccdddddcdddd")

# check the data exclusion criteria
d %>% 
  group_by(id) %>% 
  summarise(Trial_Total = sum(!is.na(PropLook_Correct)),
            Trial_Right = sum(!is.na(PropLook_Correct) & PropLook_Right >= 0.5),
            RightRatio = Trial_Right/Trial_Total) %>% 
  mutate(RightOnly = if_else(RightRatio == 1 | RightRatio == 0, 1, 0)) -> d_side

# check participants who looked only to the left or right
filter(d_side, RightOnly == 1)

# check participants without looking
filter(d_side, Trial_Total == 0)

# Data shaping
d %>% 
  group_by(id) %>% 
  summarise(ageM = first(ageM),
            vcbAll = first(vcbAll),
            noun = first(noun),
            verb = first(verb),
            .groups = "drop") -> df

# MCMC sampling setting
iter_set <- 20000
warmup_set <- 2000

# verb
## make a data list for stan
data <- list(Nrow = nrow(df),
             words = df$verb,
             age19 = if_else(df$ageM == 19, 1, 0),
             age20 = if_else(df$ageM == 20, 1, 0),
             age21 = if_else(df$ageM == 21, 1, 0),
             age22 = if_else(df$ageM == 22, 1, 0),
             age23 = if_else(df$ageM == 23, 1, 0))

## MCMC sampling with cmdstanr
here("Study1", "Models", "Model_Analysis3.stan") %>%
  cmdstan_model() -> model

fit_verb <- model$sample(data = data,
                         chains = 4,
                         iter_warmup = warmup_set,
                         iter_sampling = iter_set,
                         adapt_delta = 0.8, 
                         max_treedepth = 10,
                         seed = 1234)

output_dir <- here("Study1", "MCMCSamples", "Analysis3_Verb")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit_verb$save_output_files(dir = output_dir, basename = "verb")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit_verb
print(fit_verb, pars = c("beta0", "b_age19", "b_age20", "b_age21", "b_age22", "b_age23", "sigma"), probs = c(0.5, 0.025, 0.975))

print(fit_verb, pars = c("b_age_diff_18_19", "b_age_diff_19_20", "b_age_diff_20_21", "b_age_diff_21_22", "b_age_diff_22_23"), probs = c(0.5, 0.025, 0.975), digits=1) # see Table S8

ggs(fit_verb) %>% 
  filter(str_detect(Parameter, pattern = "_pred")) %>% 
  group_by(Parameter) %>% 
  summarise(MED = median(value),
            lwr = quantile(value, prob = 0.025),
            upr = quantile(value, prob = 0.975)) %>% 
  mutate(ageM = as.character(str_extract(Parameter, pattern = "[[:digit:]]+")),
         Category = "Verbs") -> df_predict_verb

# noun
## make a data list for stan
data <- list(Nrow = nrow(df),
             words = df$noun,
             age19 = if_else(df$ageM == 19, 1, 0),
             age20 = if_else(df$ageM == 20, 1, 0),
             age21 = if_else(df$ageM == 21, 1, 0),
             age22 = if_else(df$ageM == 22, 1, 0),
             age23 = if_else(df$ageM == 23, 1, 0))

## MCMC sampling with cmdstanr
here("Study1", "Models", "Model_Analysis3.stan") %>%
  cmdstan_model() -> model

fit_noun <- model$sample(data = data,
                         chains = 4,
                         iter_warmup = warmup_set,
                         iter_sampling = iter_set,
                         adapt_delta = 0.8, 
                         max_treedepth = 10,
                         seed = 1234)

output_dir <- here("Study1", "MCMCSamples", "Analysis3_Noun")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit_noun$save_output_files(dir = output_dir, basename = "noun")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit_noun
print(fit_noun, pars = c("beta0", "b_age19", "b_age20", "b_age21", "b_age22", "b_age23", "sigma"), probs = c(0.5, 0.025, 0.975))

print(fit_noun, pars = c("b_age_diff_18_19", "b_age_diff_19_20", "b_age_diff_20_21", "b_age_diff_21_22", "b_age_diff_22_23"), probs = c(0.5, 0.025, 0.975), digits=1) # see Table S8

ggs(fit_noun) %>% 
  filter(str_detect(Parameter, pattern = "_pred")) %>% 
  group_by(Parameter) %>% 
  summarise(MED = median(value),
            lwr = quantile(value, prob = 0.025),
            upr = quantile(value, prob = 0.975)) %>% 
  mutate(ageM = as.character(str_extract(Parameter, pattern = "[[:digit:]]+")),
         Category = "Common nouns") -> df_predict_noun

# visualization
df_predict <- bind_rows(df_predict_verb, df_predict_noun)
df_predict

df %>% 
  pivot_longer(c("vcbAll", "noun", "verb"), names_to = "Category", values_to = "value") %>% 
  group_by(Category) %>% 
  summarize(Mean = mean(value),
            SD   = sd(value),
            Min  = min(value),
            Max  = max(value))

df %>% 
  pivot_longer(c("noun", "verb"), names_to = "Category", values_to = "value") %>% 
  mutate(ageM = as.character(ageM),
         Category = if_else(Category == "noun", "Common nouns", "Verbs")) %>%  
  ggplot(aes(x = ageM))+
  geom_boxplot(aes(y = value, fill = Category), color = "gray60", width = 0.7, alpha = 0.3) +
  geom_pointrange(data = df_predict, aes(y = MED, ymax = upr, ymin = lwr, color = Category), shape = 16, lwd = 1.1, alpha = 0.95) +
  facet_wrap(~Category, scales = "free_y") +
  labs(x = "Age in months", y = "Vocabulary size") +
  scale_y_continuous(limits = c(0, NA), breaks = scales::breaks_pretty(8)) +
  scale_fill_manual(values=c("#27ABA5", "#8D02AE")) +
  scale_color_manual(values=c("#14544C", "#440154")) +
  theme_bw()+
  theme(axis.ticks = element_line(color = "black"),
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "none") -> gp
print(gp)
output_fig_dir <- here("Study1", "Figures")
if (!dir.exists(output_fig_dir)){
  dir.create(output_fig_dir, recursive = TRUE)
}
ggsave(file = here("Study1", "Figures", "Analysis3.png"), plot = gp, dpi = 350, width = 6.0, height = 3.8) # Figure S4
