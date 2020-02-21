library(tidyverse)
library(broom)
library(knitr)
library(extrafont)
extrafont::loadfonts(quiet = TRUE)

if (file.exists("data/results-summary.RData")) {
  load("data/results-summary.RData")
} else {
  load("data/gmethods_ppmi_PDMEDYN.RData")
  load("data/gmethods_ppmi_onldopa.RData")
  
  change_names <- function(df) { df %>% 
      separate(outcome,c("Outcome","Year"),sep = "_year") %>% unite("Model",method, modelname) 
  }
  summ_stats <- function(df) { df %>% group_by(Outcome,Model,Year,form,term) %>%
      summarize(est=mean(estimate,na.rm=TRUE),sd=sd(estimate,na.rm=TRUE),n=n()) %>% ungroup }
  
  bs_pdmedyn_all <- bs_results_PDMEDYN  %>% change_names %>% summ_stats
  bs_pdmedyn_started <-  bs_results_PDMEDYN_started  %>% change_names %>% summ_stats
  bs_onldopa_all <- bs_results_onldopa  %>% change_names %>% summ_stats
  bs_onldopa_started <-  bs_results_onldopa_started  %>% change_names %>% summ_stats
  bs_onldopa_levoonly <-  bs_results_onldopa_levoonly  %>% change_names %>% summ_stats
  
  
  
  full_results_PDMEDYN <- full_results_PDMEDYN %>% change_names
  full_results_PDMEDYN_started <- full_results_PDMEDYN_started %>% change_names
  full_results_onldopa_levoonly <- full_results_onldopa_levoonly %>% change_names
  full_results_onldopa_started <- full_results_onldopa_started %>% change_names
  full_results_onldopa <- full_results_onldopa %>% change_names
  
  save(full_results_PDMEDYN, full_results_PDMEDYN_started, 
       bs_pdmedyn_all, bs_pdmedyn_started,
       full_results_onldopa, full_results_onldopa_started, full_results_onldopa_levoonly,  
       bs_onldopa_all, bs_onldopa_levoonly, bs_onldopa_started,
       pats_not_started_PDMEDYN, pats_stopped_PDMEDYN, pats_levoonly, pats_started_onldopa, pats_stopped_onldopa,
       file="data/results-summary.RData")
}
df_varying <- readRDS("data/df_varying.rds")
df_constant <- readRDS("data/df_constant.rds")
df_last <- readRDS("data/df_last.rds")


process_results <- function(.x) {
  .x %>% 
    filter(term %in% c("I((treatment + treatment_lag1 + treatment_lag2 + treatment_lag3)/2)",
                       "I((treatment_lag1 + treatment_lag2 + treatment_lag3) * 2/3)")) %>% 
    filter(Model %in% c("Naive_msms","Naive_msms_adj","Censoring_msms_adj","IPW_msms_adj","gformula_msms_adj"))
}



plot_bootstrap <- function(df_res, df_boot) {
  df_boot_all <- df_boot %>% 
    mutate(Outcome=case_when(
      Outcome=="MDS_UPDRS_I"~"MDS-UPDRS Part I",
      Outcome=="MDS_UPDRS_II"~"MDS-UPDRS Part II",
      Outcome=="MDS_UPDRS_III_off"~"MDS-UPDRS Part III",
      Outcome=="MSEADLG"~"Modified Schwab & England"
    )) %>% 
    mutate(Model=case_when(
      Model=="Naive_msms"~"No Adjustment",
      Model=="Naive_msms_adj"~"Baseline Adjustment",
      Model=="Censoring_msms_adj"~"Baseline Adjustment + Censoring",
      Model=="IPW_msms_adj"~"Inverse Probability of Treatment Weighting",
      Model=="gformula_msms_adj"~"Parametric g-formula"
    ))
  
  df_res %>% 
    mutate(Outcome=case_when(
      Outcome=="MDS_UPDRS_I"~"MDS-UPDRS Part I",
      Outcome=="MDS_UPDRS_II"~"MDS-UPDRS Part II",
      Outcome=="MDS_UPDRS_III_off"~"MDS-UPDRS Part III",
      Outcome=="MSEADLG"~"Modified Schwab & England"
    )) %>% 
    mutate(Model=case_when(
      Model=="Naive_msms"~"No Adjustment",
      Model=="Naive_msms_adj"~"Baseline Adjustment",
      Model=="Censoring_msms_adj"~"Baseline Adjustment + Censoring",
      Model=="IPW_msms_adj"~"Inverse Probability of Treatment Weighting",
      Model=="gformula_msms_adj"~"Parametric g-formula"
    )) %>% 
    left_join(df_boot_all,by = c("Outcome", "Model", "Year")) %>%
    mutate(Outcome=fct_relevel(factor(Outcome),c("MDS-UPDRS Part III","Modified Schwab & England","MDS-UPDRS Part I"))) %>% 
    mutate(Model=fct_rev(fct_inorder(Model))) %>% 
    ggplot(aes(x=estimate,y=Model,color=Model)) +
    geom_vline(aes(xintercept=0))+
    geom_point() +
    geom_segment(aes(x=estimate-2*sd,xend=estimate+2*sd,yend=Model)) +
    theme_bw() +
    theme(text=element_text(family="sans"), 
          legend.position="none",
          panel.grid.major = element_line(linetype = 2,size=0.2),
          strip.background = element_rect(fill="white")) +
    ylab("") +xlab("") +
    facet_wrap(Outcome~.,labeller = function(.x) {label_value(.x,multi_line = FALSE)},ncol=3, scales = "free_x") +
    theme(strip.placement = "outside",text=element_text(family="Lato")) +
    NULL#coord_cartesian(xlim=c(-10,10))
}

plot_bootstrap(
  full_results_PDMEDYN_started %>% process_results %>% filter(Year==2, Outcome %in% c("MDS_UPDRS_I","MDS_UPDRS_III_off","MSEADLG")),
  bs_pdmedyn_started %>% process_results %>% filter(Year==2, Outcome %in% c("MDS_UPDRS_I","MDS_UPDRS_III_off","MSEADLG"))
) +  scale_color_brewer(palette = "Set1",type = "qual") +
  ggsave("different-models.pdf",dpi=1200, width = 8, height=2.6)

# plot_bootstrap(
#   full_results_PDMEDYN %>% process_results %>% filter(Year==2, Outcome %in% c("MDS_UPDRS_I","MDS_UPDRS_III_off","MSEADLG")),
#   bs_pdmedyn_all %>% process_results %>% filter(Year==2, Outcome %in% c("MDS_UPDRS_I","MDS_UPDRS_III_off","MSEADLG"))
# ) +
#   ggsave("different-models2.pdf",dpi=1200, width = 8, height=2.6)

plot_bootstrap_compact <- function(df_res, df_boot) {
  df_res <- df_res %>% mutate(dataset=fct_inorder(dataset))
  df_boot <- df_boot %>% mutate(dataset=fct_inorder(dataset))
  
  df_boot_all <- df_boot %>% 
    mutate(Outcome=case_when(
      Outcome=="MDS_UPDRS_I"~"Part I",
      Outcome=="MDS_UPDRS_II"~"Part II",
      Outcome=="MDS_UPDRS_III_other"~"Part 3",
      Outcome=="MDS_UPDRS_III_off"~"MDS-UPDRS Part III"
    )) %>% 
    mutate(Model=case_when(
      Model=="IPW_msms_adj"~"Inverse Probability of Treatment Weighting",
      Model=="gformula_msms_adj"~"Parametric g-formula"
    ))
  
  df_res %>% 
    mutate(Outcome=case_when(
      Outcome=="MDS_UPDRS_I"~"Part I",
      Outcome=="MDS_UPDRS_II"~"Part II",
      Outcome=="MDS_UPDRS_III_other"~"Part 3",
      Outcome=="MDS_UPDRS_III_off"~"MDS-UPDRS Part III"
    )) %>% 
    mutate(Model=case_when(
      Model=="IPW_msms_adj"~"Inverse Probability of Treatment Weighting",
      Model=="gformula_msms_adj"~"Parametric g-formula"
    )) %>% 
    filter(Outcome=="MDS-UPDRS Part III") %>% 
    left_join(df_boot_all,by = c("Outcome", "Model", "Year","dataset")) %>% 
    mutate(Model=fct_rev(fct_inorder(Model))) %>% 
    ggplot(aes(x=fct_rev(Year),y=estimate,color=Model)) +
    geom_hline(aes(yintercept=0))+
    #geom_point() +
    geom_pointrange(aes(ymin=estimate-2*sd,ymax=estimate+2*sd),position=position_dodge(.5)) +
    coord_flip(ylim=c(-10,10)) +
    theme_bw() +
    theme(text=element_text(family="Lato"), 
          legend.position="bottom",
          panel.grid.major = element_line(linetype = 2,size=0.2),
          panel.grid.minor = element_line(linetype = 2,size=0.2),
          strip.background = element_rect(fill="white")) +
    xlab("Year") +ylab("") +
    facet_grid(Outcome~dataset,labeller = function(.x) {label_value(.x,multi_line = FALSE)}) +
    theme(strip.placement = "outside") +
    guides(color=guide_legend(title=NULL,reverse = TRUE,override.aes=list(linetype=0))) +
    guides(shape=guide_legend(title=NULL,reverse = TRUE,override.aes=list(linetype=0))) +
    #scale_color_viridis_d() +
    NULL#coord_cartesian(xlim=c(-10,10))
}

plot_bootstrap_compact(
  bind_rows(full_results_PDMEDYN_started %>% process_results %>% mutate(dataset="PD Medication"), 
            full_results_onldopa_levoonly %>% process_results %>% mutate(dataset="Only Levodopa")) %>% 
    filter(Model %in% c("IPW_msms_adj","gformula_msms_adj"), str_detect(Outcome,"MDS_UPDRS")),
  bind_rows(bs_pdmedyn_started %>% process_results %>% mutate(dataset="PD Medication"),
            bs_onldopa_levoonly %>% process_results %>% mutate(dataset="Only Levodopa")) %>% 
    filter(Model %in% c("IPW_msms_adj","gformula_msms_adj"), str_detect(Outcome,"MDS_UPDRS"))
) + theme(strip.text.y = element_text(family="Lato",size=8) ) + scale_color_brewer(palette = "Set1",type = "qual") +
  ggsave("main-outcomes.pdf",dpi=1200, width = 8, height=2.6) 
