set.seed(1210)

n_workers <- 60 #30
n_repeats <- 1000 #1000
#out_path <- "data/gmethods_ppmi_onldopa_restricted.RData"

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

## LDOPA
pats_stopped_onldopa <-
  df_varying %>%
  mutate(treatment=ONLDOPA) %>%
  group_by(id) %>%
  arrange(time) %>%
  mutate(treatment_prev=lag(treatment)) %>%
  ungroup %>%
  filter(treatment_prev==TRUE, treatment==FALSE) %>% .$id %>% unique

pats_stopped_onldopa <- c(pats_stopped_onldopa,
                          df_varying %>% mutate(treatment=ONLDOPA) %>% filter(time==0,treatment==TRUE) %>% .$id)

# Levodopa only
pats_levoonly <- df_varying %>%
  mutate(LevoOnly=PD_MED_USE %in% c(0,NA,1)) %>%
  group_by(id) %>%
  summarize(LevoOnly=all(LevoOnly)) %>%
  ungroup %>%
  filter(LevoOnly) %>%
  .$id %>% unique

length(pats_levoonly)


pats_started <- df_last %>% filter(ONLDOPA_year2) %>% .$id




# Remove those who stopped:
df_v_onldopa <- df_varying %>%
  filter(id %in% pats_levoonly) %>%
  filter(id %in% pats_started) %>% 
  filter(!(id %in% c(pats_stopped_onldopa))) %>%
  mutate(treatment=as.integer(ONLDOPA)) %>%
  mutate(MDS_UPDRS_III_off=if_else(time==0,MDS_UPDRS_III_other,as.numeric(MDS_UPDRS_III_off)))
df_c_onldopa <- df_constant %>%
  filter(id %in% pats_levoonly) %>%
  filter(!(id %in% c(pats_stopped_onldopa))) %>%
  filter(id %in% pats_started) %>% 
  left_join(df_last %>% select(id,starts_with("LEDD")),by="id") %>% 
  mutate(Sex=(Sex=="Male"))
df_l_onldopa <- df_last %>%
  filter(id %in% pats_levoonly) %>%
  filter(!(id %in% c(pats_stopped_onldopa))) %>%
  filter(id %in% pats_started) %>% 
  select(-starts_with("LEDD"))

insert_nas <- function(x,y) {
  ins <- ifelse(is.logical(x),NA, ifelse(is.double(x),NA_real_,NA_integer_))
  if_else(y,x,ins)
}

df_l_onldopa_with_na <- 
  df_l_onldopa %>% 
  mutate_at(vars(ends_with("_year2")),~insert_nas(.,df_l_onldopa$ONLDOPA_year2)) %>% 
  mutate_at(vars(ends_with("_year3")),~insert_nas(.,df_l_onldopa$ONLDOPA_year3)) %>% 
  mutate_at(vars(ends_with("_year4")),~insert_nas(.,df_l_onldopa$ONLDOPA_year4))

df_l_onldopa$id %>% unique %>% length %>% print

set.seed(1210)

print(system.time(full_results_onldopa_levoonly_y2<- run_once(df_v_onldopa, df_c_onldopa, df_l_onldopa_with_na, msms_all, msms_ipw,compact=FALSE)))
bs_results_onldopa_levoonly_y2 <- run_bootstrap(df_v_onldopa, df_c_onldopa, df_l_onldopa_with_na, msms_all, msms_ipw, repeats=n_repeats)
save.image(out_path)




pats_started <- df_last %>% filter(ONLDOPA_year3) %>% .$id
# Remove those who stopped:
df_v_onldopa <- df_varying %>%
  filter(id %in% pats_levoonly) %>%
  filter(id %in% pats_started) %>% 
  filter(!(id %in% c(pats_stopped_onldopa))) %>%
  mutate(treatment=as.integer(ONLDOPA)) %>%
  mutate(MDS_UPDRS_III_off=if_else(time==0,MDS_UPDRS_III_other,as.numeric(MDS_UPDRS_III_off)))
df_c_onldopa <- df_constant %>%
  filter(id %in% pats_levoonly) %>%
  filter(!(id %in% c(pats_stopped_onldopa))) %>%
  filter(id %in% pats_started) %>% 
  left_join(df_last %>% select(id,starts_with("LEDD")),by="id") %>% 
  mutate(Sex=(Sex=="Male"))
df_l_onldopa <- df_last %>%
  filter(id %in% pats_levoonly) %>%
  filter(!(id %in% c(pats_stopped_onldopa))) %>%
  filter(id %in% pats_started) %>% 
  select(-starts_with("LEDD"))

insert_nas <- function(x,y) {
  ins <- ifelse(is.logical(x),NA, ifelse(is.double(x),NA_real_,NA_integer_))
  if_else(y,x,ins)
}

df_l_onldopa_with_na <- 
  df_l_onldopa %>% 
  mutate_at(vars(ends_with("_year2")),~insert_nas(.,df_l_onldopa$ONLDOPA_year2)) %>% 
  mutate_at(vars(ends_with("_year3")),~insert_nas(.,df_l_onldopa$ONLDOPA_year3)) %>% 
  mutate_at(vars(ends_with("_year4")),~insert_nas(.,df_l_onldopa$ONLDOPA_year4))

df_l_onldopa$id %>% unique %>% length %>% print

set.seed(1210)

print(system.time(full_results_onldopa_levoonly_y3<- run_once(df_v_onldopa, df_c_onldopa, df_l_onldopa_with_na, msms_all, msms_ipw)))
bs_results_onldopa_levoonly_y3 <- run_bootstrap(df_v_onldopa, df_c_onldopa, df_l_onldopa_with_na, msms_all, msms_ipw, repeats=n_repeats)
save.image(out_path)

df_varying %>% filter(id=="3020")

pats_started <- df_last %>% filter(ONLDOPA_year4) %>% .$id

# 
# df_varying%>% filter(id %in% setdiff(pats_started_onldopa,pats_started)) %>% filter(!(id %in% c(pats_stopped_onldopa)))
# 
# pats_started_onldopa <-
#   df_varying %>%
#   mutate(treatment=ONLDOPA) %>%
#   group_by(id) %>%
#   filter(treatment,!lag(treatment)) %>%
#   ungroup %>%
#   .$id %>% unique

# Remove those who stopped:
df_v_onldopa <- df_varying %>%
  filter(id %in% pats_levoonly) %>%
  filter(id %in% pats_started) %>% 
  filter(!(id %in% c(pats_stopped_onldopa))) %>%
  mutate(treatment=as.integer(ONLDOPA)) %>%
  mutate(MDS_UPDRS_III_off=if_else(time==0,MDS_UPDRS_III_other,as.numeric(MDS_UPDRS_III_off)))
df_c_onldopa <- df_constant %>%
  filter(id %in% pats_levoonly) %>%
  filter(!(id %in% c(pats_stopped_onldopa))) %>%
  filter(id %in% pats_started) %>% 
  left_join(df_last %>% select(id,starts_with("LEDD")),by="id") %>% 
  mutate(Sex=(Sex=="Male"))

df_l_onldopa <- df_last %>%
  filter(id %in% pats_levoonly) %>%
  filter(!(id %in% c(pats_stopped_onldopa))) %>%
  filter(id %in% pats_started) %>% 
  select(-starts_with("LEDD"))

insert_nas <- function(x,y) {
  ins <- ifelse(is.logical(x),NA, ifelse(is.double(x),NA_real_,NA_integer_))
  if_else(y,x,ins)
}

df_l_onldopa_with_na <- 
  df_l_onldopa %>% 
  mutate_at(vars(ends_with("_year2")),~insert_nas(.,df_l_onldopa$ONLDOPA_year2)) %>% 
  mutate_at(vars(ends_with("_year3")),~insert_nas(.,df_l_onldopa$ONLDOPA_year3)) %>% 
  mutate_at(vars(ends_with("_year4")),~insert_nas(.,df_l_onldopa$ONLDOPA_year4))

df_l_onldopa$id %>% unique %>% length %>% print

set.seed(1211)
print(system.time(full_results_onldopa_levoonly_y4<- run_once(df_v_onldopa, df_c_onldopa, df_l_onldopa_with_na, msms_all, msms_ipw)))
bs_results_onldopa_levoonly_y4 <- run_bootstrap(df_v_onldopa, df_c_onldopa, df_l_onldopa_with_na, msms_all, msms_ipw, repeats=n_repeats)
save.image(out_path)
