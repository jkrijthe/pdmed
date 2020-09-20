library(randomForest)

# Non-markov
models_ipw_prior_nonmarkov <- list(
  list(function(df) {glm_det(formula("treatment~censored+1"), data=df, family=binomial(link="logit")) },
       function(df) {glm_det(formula("treatment~censored+1"), data=df, family=binomial(link="logit")) },
       function(df) {glm_det(formula("treatment~censored+1"), data=df, family=binomial(link="logit")) },
       function(df) {glm_det(formula("treatment~censored+1"), data=df, family=binomial(link="logit")) }),
  list(function(df) {glm_det(formula("censored~1"), data=df, family=binomial(link="logit")) },
       function(df) {glm_det(formula("censored~treatment_lag1"), data=df, family=binomial(link="logit")) },
       function(df) {glm_det(formula("censored~treatment_lag1+treatment_lag2"), data=df, family=binomial(link="logit")) },
       function(df) {glm_det(formula("censored~treatment_lag1+treatment_lag2+treatment_lag3"), data=df, family=binomial(link="logit")) })
) %>% purrr::transpose()

models_ipw_nonmarkov <- list(
  list(function(df) {glm_det(formula(construct_model("treatment",c("censored+Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag1"), 
                                                     c("MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1)), data=df, family=binomial(link="logit")) },
       function(df) {glm_det(formula(construct_model("treatment",c("treatment_lag1+censored+Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag2"), 
                                                     c("MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1:2)), data=df, family=binomial(link="logit")) },
       function(df) {glm_det(formula(construct_model("treatment",c("treatment_lag1+censored+Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag1+MoCA_lag3"), 
                                                     c("MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1:3)), data=df, family=binomial(link="logit")) },
       function(df) {glm_det(formula(construct_model("treatment",c("treatment_lag1+censored+Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag2+MoCA_lag4"), 
                                                     c("MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1:4)), data=df, family=binomial(link="logit")) }),
  list(function(df) {glm_det(formula(construct_model("censored",c("Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag1"), c("treatment","MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1)), data=df, family=binomial(link="logit")) },
       function(df) {glm_det(formula(construct_model("censored",c("Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag2"), c("treatment","MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1:2)), data=df, family=binomial(link="logit")) },
       function(df) {glm_det(formula(construct_model("censored",c("Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag1+MoCA_lag3"), c("treatment","MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1:3)), data=df, family=binomial(link="logit")) },
       function(df) {glm_det(formula(construct_model("censored",c("Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag2+MoCA_lag4"), c("treatment","MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1:4)), data=df, family=binomial(link="logit")) })
) %>% purrr::transpose()


# Non-parametric Random Forest
mod_rf <- function(form, data, family=NULL, always.split.vars=c("treatment_lag1")) {
  treat_name <- treat_name <- form %>%  terms %>%  all.vars %>% .[[1]]
  #data <- data %>% filter(!is.na(treatment),!is.na(censored))
  if (treat_name=="treatment") {
    data <- data %>%  filter(treatment_lag1!=1)
  }
  
  if (length(unique(data[[treat_name]]))==1) {
    if (treat_name=="treatment") {
      return(glm_det(update.formula(form, treatment~.),data,family=binomial(link="logit")))
    }
    if (treat_name=="censored") {
      return(glm_det(update.formula(form, censored~.),data,family=binomial(link="logit")))
    }
  }
  
  obj <- randomForest(form, data,na.action=na.omit)
  #randomForest(form, data,nodesize=1,na.action=na.omit)
  #browser()
  obj$treat_name <- treat_name
  obj
  #glm(form, data=data, family=binomial(link="logit"))
}


predict_dens.randomForest <- function(object,new_data) {
  treat_name <- object$treat_name
  
  if (length(object$classes)==2) {
    predictions1 <- predict(object, new_data, type="prob")[,2]
  } else {
    error("Single outcome for all units detected.")
  }
  
  if (treat_name=="treatment") {
    predictions1<- if_else(new_data$treatment_lag1==1, 1.0, predictions1)
  }
  
  new_data[[treat_name]]*predictions1 +
    (1-new_data[[treat_name]])*(1-predictions1) %>%
    data.frame() -> pred
  names(pred) <- treat_name
  #print(treat_name)
  pred
}

models_ipw_np_rf <- list(
  list(function(df) {mod_rf(formula(construct_model("factor(treatment)",c("censored+Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag1"), 
                                                    c("MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1)), data=df, family=binomial(link="logit")) },
       function(df) {mod_rf(formula(construct_model("factor(treatment)",c("treatment_lag1+censored+Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag2"), 
                                                    c("MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1:2)), data=df, family=binomial(link="logit")) },
       function(df) {mod_rf(formula(construct_model("factor(treatment)",c("treatment_lag1+censored+Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag1+MoCA_lag3"), 
                                                    c("MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1:3)), data=df, family=binomial(link="logit")) },
       function(df) {mod_rf(formula(construct_model("factor(treatment)",c("treatment_lag1+censored+Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag2+MoCA_lag4"), 
                                                    c("MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1:4)), data=df, family=binomial(link="logit")) }),
  list(function(df) {mod_rf(formula(construct_model("factor(censored)",c("Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag1"), c("treatment","MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1)), data=df, family=binomial(link="logit")) },
       function(df) {mod_rf(formula(construct_model("factor(censored)",c("Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag2"), c("treatment","MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1:2)), data=df, family=binomial(link="logit")) },
       function(df) {mod_rf(formula(construct_model("factor(censored)",c("Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag1+MoCA_lag3"), c("treatment","MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1:3)), data=df, family=binomial(link="logit")) },
       function(df) {mod_rf(formula(construct_model("factor(censored)",c("Age+Education+Sex+Duration+CV_HISTORY+MoCA_lag2+MoCA_lag4"), c("treatment","MDS_UPDRS_I","MDS_UPDRS_II","MDS_UPDRS_III_other","MSEADLG"),lags=1:4)), data=df, family=binomial(link="logit")) })
) %>% purrr::transpose()