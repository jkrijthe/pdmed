set.seed(1210)

n_workers <- 60 #30
n_repeats <- 1000 #1000
out_path <- "data/gmethods_ppmi_pdmedyn.RData"


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


### On medication
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

insert_nas <- function(x,y) {
  ins <- ifelse(is.logical(x),NA, ifelse(is.double(x),NA_real_,NA_integer_))
  if_else(y,x,ins)
}

df_l_PDMEDYN_with_na <- 
  df_l_PDMEDYN %>% 
  mutate_at(vars(ends_with("_year2")),~insert_nas(.,df_l_PDMEDYN$PDMEDYN_year2)) %>% 
  mutate_at(vars(ends_with("_year3")),~insert_nas(.,df_l_PDMEDYN$PDMEDYN_year3)) %>% 
  mutate_at(vars(ends_with("_year4")),~insert_nas(.,df_l_PDMEDYN$PDMEDYN_year4))

# Check NAs are really removed:
# df_l_PDMEDYN_with_na %>% 
#   select(ends_with("_year2")) %>% 
#   filter(is.na(PDMEDYN_year2)) %>% 
#   is.na %>% all

print(pats_stopped_PDMEDYN)
length(pats_stopped_PDMEDYN)
df_v_PDMEDYN %>% nrow %>% {./5}
df_c_PDMEDYN %>% nrow()
df_l_PDMEDYN %>% nrow()

set.seed(1210)
print("Calculating models using data with those on medication at time of measurement")
print(system.time(full_results_PDMEDYN_medication <- run_once(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN_with_na, msms_all, msms_ipw, compact=TRUE)))
print("Full run complete, now bootstrapping")
bs_results_PDMEDYN_medication <- run_bootstrap(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN_with_na, msms_all, msms_ipw, repeats=n_repeats)
save.image(out_path)

set.seed(1210)
print("Calculating models using all measurements")
system.time(full_results_PDMEDYN_all <- run_once(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN, msms_all, msms_ipw,compact=TRUE))
print("Full run complete, now bootstrapping")
bs_results_PDMEDYN_all <- run_bootstrap(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN, msms_all, msms_ipw, repeats=n_repeats)
save.image(out_path)


# Checks
#full_results_PDMEDYN_medication %>% filter(term=="treatment") %>% filter(str_detect(outcome,"year2")) %>% View()
#full_results_PDMEDYN_medication %>% mutate(n_obs=map_int(fitted, ~.x %>% .$model %>% nrow)) %>% select(outcome,n_obs,method) %>% View()
#full_results_PDMEDYN_all %>% mutate(n_obs=map_int(full_results_PDMEDYN_all$fitted, ~.x %>% .$model %>% nrow)) %>% select(outcome,n_obs,method) %>% View()
