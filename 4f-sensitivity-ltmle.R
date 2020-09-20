set.seed(1210)

n_workers <- 60 #30
n_repeats <- 1000 #1000
out_path <- "data/sensitivity_tmle.RData"


# ---- Setup ----
library(tidyverse)
library(broom)
library(future)
library(furrr)
library(ltmle)
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


pats_started <- df_last %>% filter(PDMEDYN_year2) %>% .$id

### On medication
df_v_PDMEDYN <- df_varying %>%
  filter(!(id %in% pats_stopped_PDMEDYN)) %>%
  filter(id %in% pats_started) %>% 
  mutate(treatment=as.integer(PDMEDYN)) %>%
  mutate(MDS_UPDRS_III_off=if_else(time==0,MDS_UPDRS_III_other,as.numeric(MDS_UPDRS_III_off)))
df_c_PDMEDYN <- df_constant %>%
  filter(!(id %in% pats_stopped_PDMEDYN)) %>%
  filter(id %in% pats_started) %>% 
  left_join(df_last %>% select(id,starts_with("LEDD")),by="id")
df_l_PDMEDYN <- df_last %>%
  filter(!(id %in% pats_stopped_PDMEDYN)) %>%
  filter(id %in% pats_started) %>% 
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

reordering <- function(v,t) {
  rep(1:t,each=v)+rep(t*((1:v)-1),times=t)
} 

# getreg <- function(n) {
#   regime <- t(matrix(c(0,0,0,0,
#                        0,0,0,1,
#                        0,0,1,1,
#                        0,1,1,1,
#                        1,1,1,1),ncol=4,byrow = TRUE))
#   regimes <- array(NA,dim=c(n,nrow(regime),ncol(regime)))
#   
#   for (i in 1:n) {
#     regimes[i,,] <- regime
#   }
#   return(regimes)                
# }
#regimes <- getreg(nrow(data_lt))
#summeas <- array((0:5)/2,dim=c(5,1,1),dimnames = list(NULL,c("totaltime"),NULL))

getreg <- function(n) {
  regime <- t(matrix(c(0,0,0,
                       0,0,1,
                       0,1,1,
                       1,1,1),ncol=3,byrow = TRUE))
  regimes <- array(NA,dim=c(n,nrow(regime),ncol(regime)))
  
  for (i in 1:n) {
    regimes[i,,] <- regime
  }
  return(regimes)                
}
summeas <- array((1:5)/2,dim=c(4,1,1),dimnames = list(NULL,c("totaltime"),NULL))

run_bootstrap_tmle <- function(df_varying,df_constant,df_l, models=NULL, repeats=1000) {
  future_map(1:repeats, function(x) {
    sp <- bootstrap_sample2(unique(df_varying$id),df_varying,df_constant,df_l)
    run_once_tmle(sp$df_measurements,sp$df_outcomes,sp$df_outcomes2, models=models)
  },.progress = TRUE) %>% bind_rows(.id="rep")
}

run_once_tmle <- function(df_varying,df_c,df_l,models=NULL, return_model=FALSE) {
  data_lt <- df_varying %>% select(id, time, MDS_UPDRS_I, MDS_UPDRS_II, MDS_UPDRS_III_other, PDMEDYN, MSEADLG, MoCA) %>% #add censored
    mutate(PDMEDYN=as.integer(PDMEDYN)) %>% 
    pivot_wider(id_cols="id", names_from="time", values_from = c("PDMEDYN","MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG","MoCA")) %>% 
    left_join(df_c %>% select(id,Age,Sex,Duration,Education,CV_HISTORY), by="id") %>% 
    left_join(df_l %>% select(id,MDS_UPDRS_III_off_year2),by="id")
  
  data_lt <- data_lt %>% 
    select(-id) %>% 
    select(reordering(v=6,t=5)) %>% 
    select(-PDMEDYN_0,-MoCA_1,-MoCA_3) %>% 
    bind_cols(data_lt %>% select(Age,Sex,Duration,Education,CV_HISTORY),.) %>% 
    bind_cols(data_lt %>% select(MDS_UPDRS_III_off_year2)) %>% 
    mutate(censored=as.integer(!is.na(MDS_UPDRS_III_off_year2))) %>% 
    select(-MDS_UPDRS_III_off_year2,MDS_UPDRS_III_off_year2,-PDMEDYN_4)
  
  regimes <- getreg(nrow(data_lt))
  data_sel <- data_lt[,-c(27:31)]
  #browser()
  res1 <- ltmleMSM(data=data_sel, 
                   Anodes=grep("^PDMEDYN", names(data_sel)),
                   Cnodes = "censored",
                   Lnodes=grep("^MDS_UPDRS|^MSEA|^MoCA", names(data_sel))[-c(1:5,19)],
                   Ynodes=grep("^MDS_UPDRS|^MSEA|^MoCA", names(data_sel))[c(19)],
                   regimes = regimes,
                   summary.measures = summeas,
                   deterministic.g.function = MaintainTreatment,
                   SL.library = models,
                   working.msm = "Y ~ totaltime + MDS_UPDRS_III_other_0",
                   final.Ynodes="MDS_UPDRS_III_off_year2",
                   iptw.only = FALSE, gcomp=TRUE, msm.weights = NULL)
  res2 <- ltmleMSM(data=data_sel, 
                   Anodes=grep("^PDMEDYN", names(data_sel)),
                   Cnodes = "censored",
                   Lnodes=grep("^MDS_UPDRS|^MSEA|^MoCA", names(data_sel))[-c(1:5,19)],
                   Ynodes=grep("^MDS_UPDRS|^MSEA|^MoCA", names(data_sel))[c(19)],
                   regimes = regimes,
                   summary.measures = summeas,
                   deterministic.g.function = MaintainTreatment,
                   SL.library = models,
                   working.msm = "Y ~ totaltime + MDS_UPDRS_III_other_0",
                   final.Ynodes="MDS_UPDRS_III_off_year2",
                   iptw.only = FALSE, gcomp=FALSE, msm.weights = NULL)
  #res1$fit <- NULL
  #res2$fit <- NULL
  
  ret <- list("gform"=res1$beta[2], "tmle"=res2$beta[2], "iptw"= res1$beta.iptw[2])
  if (return_model) { attr(ret,"tmle") <- res2 }
  
  return(ret)
}



set.seed(1210)
print("tmle with glm models")
res_glm <- run_once_tmle(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN_with_na, models=NULL)
bs_glm <- run_bootstrap_tmle(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN_with_na, models=NULL, repeats=n_repeats)
save.image(out_path)

set.seed(1210)
print("tmle with glm, rf and bayesglm")
res_sl <- run_once_tmle(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN_with_na, models=list("SL.glm", "SL.randomForest", "SL.bayesglm"), return_model=TRUE)
bs_sl <- run_bootstrap_tmle(df_v_PDMEDYN, df_c_PDMEDYN, df_l_PDMEDYN_with_na, models=list("SL.glm", "SL.randomForest", "SL.bayesglm"), repeats=n_repeats)
save.image(out_path)

