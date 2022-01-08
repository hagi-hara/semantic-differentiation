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
  group_by(id, Condition, time) %>% 
  summarise(ageM = first(ageM),
            vcbAll = first(vcbAll),
            noun = first(noun),
            verb = first(verb),
            Trial_Total = sum(!is.na(PropLook_Correct)),
            Trial_Correct = sum(Correct_Looking, na.rm = TRUE),
            .groups = "drop") %>%
  filter(Condition == "ObjDiff" | Condition == "ObjAbs") %>% 
  mutate(ID_subject = as.numeric(as.factor(id)),
         ID_sub_time = as.numeric(as.factor(str_c(id, time, sep = "_"))),
         ID_sub_cond = as.numeric(as.factor(str_c(id, Condition, sep = "_"))))  -> df

# MCMC sampling setting
iter_set <- 20000
warmup_set <- 2000

# Model 1: cbind(Trial_Correct, Trial_Total - Trial_Correct) ~ time * Condition + (1|id) + (1|id:time) + (1|id:cond)
## make a data list for stan
data <- list(Nrow   = nrow(df),
             N_total = df$Trial_Total,
             N_correct = df$Trial_Correct,
             X1 = df$time,
             X2 = if_else(df$Condition == "ObjAbs", 1, 0),
             X3 = df$time * if_else(df$Condition == "ObjAbs", 1, 0),
             N_subject = length(unique(df$ID_subject)),
             N_sub_time = length(unique(df$ID_sub_time)),
             N_sub_cond = length(unique(df$ID_sub_cond)),
             ID_subject = df$ID_subject,
             ID_sub_time = df$ID_sub_time,
             ID_sub_cond = df$ID_sub_cond)

## MCMC sampling with cmdstanr
here("Study2", "Models", "Binary", "model1.stan") %>%
  cmdstan_model() -> model1

fit1 <- model1$sample(data = data,
                      chains = 4,
                      iter_warmup = warmup_set,
                      iter_sampling = iter_set,
                      adapt_delta = 0.98, 
                      max_treedepth = 15,
                      seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis2_PreferentialLooking_Binary", "model1")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit1$save_output_files(dir = output_dir, basename = "model1")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit1
print(fit1, pars = c("beta0", "beta1", "beta2", "beta3", "sigma1", "sigma2", "sigma3"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit1, merge_chains = FALSE)
waic1 <- waic(log_lik)
print(waic1, digits=2)


# Model 2: cbind(Trial_Correct, Trial_Total - Trial_Correct) ~ time + Condition + (1|id) + (1|id:time) + (1|id:cond)
## make a data list for stan
data <- list(Nrow   = nrow(df),
             N_total = df$Trial_Total,
             N_correct = df$Trial_Correct,
             X1 = df$time,
             X2 = if_else(df$Condition == "ObjAbs", 1, 0),
             # X3 = df$time * if_else(df$Condition == "ObjAbs", 1, 0),
             N_subject = length(unique(df$ID_subject)),
             N_sub_time = length(unique(df$ID_sub_time)),
             N_sub_cond = length(unique(df$ID_sub_cond)),
             ID_subject = df$ID_subject,
             ID_sub_time = df$ID_sub_time,
             ID_sub_cond = df$ID_sub_cond)

## MCMC sampling with cmdstanr
here("Study2", "Models", "Binary", "model2.stan") %>% 
  cmdstan_model() -> model2

fit2 <- model2$sample(data = data,
                      chains = 4,
                      iter_warmup = warmup_set,
                      iter_sampling = iter_set,
                      adapt_delta = 0.98, 
                      max_treedepth = 15,
                      seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis2_PreferentialLooking_Binary", "model2")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit2$save_output_files(dir = output_dir, basename = "model2")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit2
print(fit2, pars = c("beta0", "beta1", "beta2", "sigma1", "sigma2", "sigma3"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit2, merge_chains = FALSE)
waic2 <- waic(log_lik)
print(waic2, digits=2)


# Model 3: cbind(Trial_Correct, Trial_Total - Trial_Correct) ~ time + (1|id)
## make a data list for stan
data <- list(Nrow   = nrow(df),
             N_total = df$Trial_Total,
             N_correct = df$Trial_Correct,
             X1 = df$time,
             # X2 = if_else(df$Condition == "ObjAbs", 1, 0),
             # X3 = df$time * if_else(df$Condition == "ObjAbs", 1, 0),
             N_subject = length(unique(df$ID_subject)),
             ID_subject = df$ID_subject)

## MCMC sampling with cmdstanr
here("Study2", "Models", "Binary", "model3_4.stan") %>% 
  cmdstan_model() -> model3_4

fit3 <- model3_4$sample(data = data,
                        chains = 4,
                        iter_warmup = warmup_set,
                        iter_sampling = iter_set,
                        adapt_delta = 0.98, 
                        max_treedepth = 15,
                        seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis2_PreferentialLooking_Binary", "model3")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit3$save_output_files(dir = output_dir, basename = "model3")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit3
print(fit3, pars = c("beta0", "beta1", "sigma1"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit3, merge_chains = FALSE)
waic3 <- waic(log_lik)
print(waic3, digits=2)


# Model 4: cbind(Trial_Correct, Trial_Total - Trial_Correct) ~ condition + (1|id)
## make a data list for stan
data <- list(Nrow   = nrow(df),
             N_total = df$Trial_Total,
             N_correct = df$Trial_Correct,
             # X1 = df$time,
             X1 = if_else(df$Condition == "ObjAbs", 1, 0),
             # X3 = df$time * if_else(df$Condition == "ObjAbs", 1, 0),
             N_subject = length(unique(df$ID_subject)),
             ID_subject = df$ID_subject)

## MCMC sampling with cmdstanr
fit4 <- model3_4$sample(data = data,
                        chains = 4,
                        iter_warmup = warmup_set,
                        iter_sampling = iter_set,
                        adapt_delta = 0.98, 
                        max_treedepth = 15,
                        seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis2_PreferentialLooking_Binary", "model4")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit4$save_output_files(dir = output_dir, basename = "model4")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit4
print(fit4, pars = c("beta0", "beta1", "sigma1"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit4, merge_chains = FALSE)
waic4 <- waic(log_lik)
print(waic4, digits=2)


# Model 5: cbind(Trial_Correct, Trial_Total - Trial_Correct) ~ 1 + (1|id)
## make a data list for stan
data <- list(Nrow   = nrow(df),
             N_total = df$Trial_Total,
             N_correct = df$Trial_Correct,
             # X1 = df$time,
             # X2 = if_else(df$Condition == "ObjAbs", 1, 0),
             # X3 = df$time * if_else(df$Condition == "ObjAbs", 1, 0),
             N_subject = length(unique(df$ID_subject)),
             ID_subject = df$ID_subject)

## MCMC sampling with cmdstanr
here("Study2", "Models", "Binary", "model5.stan") %>% 
  cmdstan_model() -> model5

fit5 <- model5$sample(data = data,
                      chains = 4,
                      iter_warmup = warmup_set,
                      iter_sampling = iter_set,
                      adapt_delta = 0.98, 
                      max_treedepth = 15,
                      seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis2_PreferentialLooking_Binary", "model5")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit5$save_output_files(dir = output_dir, basename = "model5")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit5
print(fit5, pars = c("beta0", "sigma1"), probs = c(0.5, 0.025, 0.975))

## calculate WAIC
log_lik <- extract_log_lik(fit5, merge_chains = FALSE)
waic5 <- waic(log_lik)
print(waic5, digits=2)


# model selection
rbind(waic1$estimates,
      waic2$estimates,
      waic3$estimates,
      waic4$estimates,
      waic5$estimates) %>% 
  cbind(Model = rep(str_c("model", 1:5), each = 3)) %>%
  cbind(Formula = rep(c("~ time * Condition", "~ time + Condition", "~ time", "~ Condition", "~ 1"), each = 3)) %>%
  cbind(Measure = c("elpd_waic", "p_waic", "waic")) %>% 
  as_tibble() -> df_waic

df_waic %>% 
  filter(Measure == "waic") %>%
  relocate(Model, Formula, Measure) %>% 
  arrange(Estimate) # see Table S9


# selected model
expand(df, time, Condition) %>% 
  mutate(id = row_number()) -> df_new
## make a data list for stan
data <- list(Nrow   = nrow(df),
             N_total = df$Trial_Total,
             N_correct = df$Trial_Correct,
             # X1 = df$time,
             # X2 = if_else(df$Condition == "Mis", 1, 0),
             # X3 = df$time * if_else(df$Condition == "Mis", 1, 0),
             N_subject = length(unique(df$ID_subject)),
             # N_sub_time = length(unique(df$ID_sub_time)),
             # N_sub_cond = length(unique(df$ID_sub_cond)),
             ID_subject = df$ID_subject,
             # ID_sub_time = df$ID_sub_time,
             # ID_sub_cond = df$ID_sub_cond,
             N_pred = nrow(df_new))

## MCMC sampling with cmdstanr
here("Study2", "Models", "Binary", "model5_selected.stan") %>% 
  cmdstan_model() -> model

fit <- model$sample(data = data,
                    chains = 4,
                    iter_warmup = warmup_set,
                    iter_sampling = iter_set,
                    adapt_delta = 0.98, 
                    max_treedepth = 15,
                    seed = 1234)

output_dir <- here("Study2", "MCMCSamples", "Analysis2_PreferentialLooking_Binary", "model_selected")
if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
fit$save_output_files(dir = output_dir, basename = "model_selected")

## get MCMC samples with stanfit object format
str_c(output_dir,
      list.files(output_dir, pattern = ".csv"), sep = "/") %>% 
  read_stan_csv() -> fit

## proportion of correct response
ggs(fit) %>% 
  filter(str_detect(Parameter, pattern = "q_pred")) %>% 
  group_by(Parameter) %>% 
  summarise(MED = median(value),
            lwr = quantile(value, probs = 0.025),
            upr = quantile(value, probs = 0.975)) %>%
  mutate(id = as.numeric(str_extract(Parameter, pattern = "[[:digit:]]+"))) %>% 
  left_join(df_new, by = "id") %>% 
  mutate(time = if_else(time == 1, "1st", "2nd"),
         Condition = if_else(Condition == "ObjDiff", "Different object", "Absent object"),
         Condition = fct_relevel(Condition, "Different object", "Absent object")) -> df_predict
df_predict

## visualization
df %>%
  mutate(Prop = Trial_Correct/Trial_Total,
         time = if_else(time == 1, "1st", "2nd"),
         Condition = if_else(Condition == "ObjDiff", "Different object", "Absent object"),
         Condition = fct_relevel(Condition, "Different object", "Absent object")) -> df_obs

df_obs %>% 
  group_by(Condition, time, Prop) %>% 
  count() %>% 
  ungroup() %>% 
  filter(!is.na(Prop)) %>%
  add_row(time = "1st", Condition = "Different object", Prop = 2.0, n = 13) %>% 
  mutate(Condition = fct_relevel(Condition, "Different object", "Absent object")) -> df_count

ggplot(df_count, aes(x = time)) +
  geom_hline(yintercept = 0.5, lty = 2, color = "gray30", lwd = 1) +
  geom_line(data = df_obs, aes(y = Trial_Correct/Trial_Total, group = id), color="gray85", lwd=1.4, alpha=0.5) +
  geom_point(aes(y = Prop, size = n, color = Condition), shape = 16, alpha = 0.5) +
  geom_ribbon(data = df_predict, aes(ymax = upr, ymin = lwr, fill = Condition, group = Condition), alpha = 0.25) +
  geom_line(data = df_predict, aes(y = MED, color = Condition, group = Condition), lwd = 2.5) +
  facet_wrap(~ Condition) +
  labs(x = "Time", y = "Proportion of correct responses (looking)") +
  scale_color_manual(values = c("#A021FF", "#719E00")) +
  scale_fill_manual(values = c("#A021FF", "#719E00")) +
  scale_size_continuous(breaks=c(1,4,8,12)) +
  scale_y_continuous(limits = c(0, 1), breaks = 0:10/10) +
  theme_bw() +
  theme(axis.ticks = element_line(color = "black"),
        axis.text = element_text(size = 14, color = "black"),
        axis.title=element_text(size=14, color="black"),
        strip.text = element_text(size = 14),
        legend.title=element_text(size=14, color="black", face="italic", hjust=0.2),
        legend.text=element_text(size=12, color="black"),
        legend.justification=c(1,0)) +
  guides(color=FALSE, fill=FALSE) -> gp
print(gp)
output_fig_dir <- here("Study2", "Figures")
if (!dir.exists(output_fig_dir)){
  dir.create(output_fig_dir, recursive = TRUE)
}
ggsave(file = here("Study2", "Figures", "Analysis2_PreferentialLokking_Binary.png"), plot = gp, dpi = 350, width = 5.5, height = 4.8) # Figure 3b
