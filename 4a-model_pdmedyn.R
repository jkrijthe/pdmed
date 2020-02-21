set.seed(1210)

n_workers <- 60 #30
n_repeats <- 1000 #1000
out_path <- "data/gmethods_ppmi_PDMEDYN.RData"


# ---- Setup ----
library(tidyverse)
library(broom)
library(future)
library(furrr)
devtools::load_all("gmethods")

# ---- Models ----

source("R/setupmodels.R")

# ---- Analyses ----

df_varying <- readRDS("data/df_varying.rds")
df_constant <- readRDS("data/df_constant.rds")
df_last <- readRDS("data/df_last.rds")

plan(transparent)
plan(multiprocess,workers=n_workers)

pats_stopped_PDMEDYN <-
  df_varying %>%
  mutate(treatment=PDMEDYN) %>%
  group_by(id) %>%
  arrange(time) %>%
  mutate(treatment_prev=lag(treatment)) %>%
  ungroup %>%
  filter(treatment_prev==TRUE, treatment==FALSE) %>% .$id %>% unique

pats_stopped_PDMEDYN <- c(pats_stopped_PDMEDYN,
  df_varying %>% mutate(treatment=PDMEDYN) %>% filter(time==0,treatment==TRUE) %>% .$id)


### All
df_v_PDMEDYN <- df_varying %>%
  filter(!(id %in% pats_stopped_PDMEDYN)) %>%
  mutate(treatment=as.integer(PDMEDYN)) %>%
  mutate(MDS_UPDRS_III_off=if_else(time==0,MDS_UPDRS_III_other,as.numeric(MDS_UPDRS_III_off)))
df_c_PDMEDYN <- df_constant %>%
  filter(!(id %in% pats_stopped_PDMEDYN)) %>%
  left_join(df_last %>% select(id,starts_with("LEDD")),by="id")
df_l_PDMEDYN <- df_last %>%
  filter(!(id %in% pats_stopped_PDMEDYN)) %>%
  select(-starts_with("LEDD"))

print(pats_stopped_PDMEDYN)
length(pats_stopped_PDMEDYN)
df_v_PDMEDYN %>% nrow %>% {./5}
df_c_PDMEDYN %>% nrow()
df_l_PDMEDYN %>% nrow()

#print(system.time(full_results_PDMEDYN <- run_once(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN, msms_all, msms_ipw, compact=FALSE)))
#full_results_PDMEDYN %>%  mutate(n_obs=map_int(full_results_PDMEDYN$fitted, ~.x %>% .$model %>% nrow)) %>% select(msm,outcome,n_obs,method) %>% spread(method,n_obs) %>% View()

print(system.time(full_results_PDMEDYN <- run_once(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN, msms_all, msms_ipw, compact=TRUE)))


print("Full run complete, now bootstrapping")
bs_results_PDMEDYN <- run_bootstrap(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN, msms_all, msms_ipw, repeats=n_repeats)
save.image(out_path)

### Started
pats_not_started_PDMEDYN <-
  df_varying %>%
  mutate(treatment=PDMEDYN) %>%
  filter(time==4) %>%
  filter(is.na(treatment) | treatment==FALSE) %>%
  .$id

df_v_PDMEDYN <- df_varying %>%
  filter(!(id %in% c(pats_stopped_PDMEDYN,pats_not_started_PDMEDYN))) %>%
  mutate(treatment=as.integer(PDMEDYN)) %>%
  mutate(MDS_UPDRS_III_off=if_else(time==0,MDS_UPDRS_III_other,as.numeric(MDS_UPDRS_III_off)))
df_c_PDMEDYN <- df_constant %>%
  filter(!(id %in% c(pats_stopped_PDMEDYN,pats_not_started_PDMEDYN))) %>%
  left_join(df_last %>% select(id,starts_with("LEDD")),by="id")
df_l_PDMEDYN <- df_last %>%
  filter(!(id %in% c(pats_stopped_PDMEDYN,pats_not_started_PDMEDYN))) %>%
  select(-starts_with("LEDD"))


print(pats_stopped_PDMEDYN)
length(pats_stopped_PDMEDYN)
length(pats_not_started_PDMEDYN)
df_v_PDMEDYN %>% nrow %>% {./5}
df_c_PDMEDYN %>% nrow()
df_l_PDMEDYN %>% nrow()

#print(system.time(full_results_PDMEDYN <- run_once(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN, msms_all_starters, msms_ipw_starters, compact=FALSE)))
#full_results_PDMEDYN %>%  mutate(n_obs=map_int(full_results_PDMEDYN$fitted, ~.x %>% .$model %>% nrow)) %>% select(outcome,n_obs,method) %>% View()

system.time(full_results_PDMEDYN_started <- run_once_starters(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN, msms_all_starters, msms_ipw_starters))
print("Full run complete, now bootstrapping")
bs_results_PDMEDYN_started <- run_bootstrap_starters(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN, msms_all_starters, msms_ipw_starters, repeats=n_repeats)
save.image(out_path)

