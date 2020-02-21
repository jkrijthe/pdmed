predict_dens.default <- function(object, new_data) { UseMethod("predict_dens") }
setGeneric("predict_dens",predict_dens.default)

#' Marginal Structural Models using Inverse probability Weighting
#'
#' @param data_varying
#' @param msm formula specification of the marginal structural model
#' @param models
#' @param models_prior
#'
msm_ipw <- function(data_varying, msm, models, models_prior, data_constant=NULL, data_last=NULL, pool=FALSE, save=TRUE, censor_outcome=FALSE) {

  T <- max(data_varying$time)
  if (T==0) stop("Only one time point provided")

  df <- data_varying %>% add_lags2(T)

  if (!is.null(data_last)) {
    df <- df %>% left_join(data_last %>% mutate(time=T),by=c("id","time"))
    if (censor_outcome) {
      var_censored <- map_chr(msm,~all.vars(terms(.x))[[1]]) %>% unique
      if (length(var_censored)!=1) stop("Multiple censoring variables")
      var_censored <- as.symbol(var_censored)
      df <- df %>% mutate(censored=if_else(time==T,censored | is.na(!!var_censored), censored))
    }
  }

  if (!is.null(data_constant)) {
    df <- df %>% left_join(data_constant,by="id")
  }


  fitted <- rep(list(list()),times=T)
  fitted_prior <- rep(list(list()),times=T)
  weights_t <- list()
  weights <- list()
  weights_t_prior <- list()
  weights_prior <- list()

  # train weight model, predict weights
  if (!pool) {
    df <- df
    for (t in seq_len(T)) {
      K <- length(models[[t]])
      for (i in seq_len(K)) {
        df_t <- df %>% filter(time==t)

        fitted[[t]][[i]] <- models[[t]][[i]](df_t)
        weights[[i]] <- predict_dens(fitted[[t]][[i]],df_t)

        fitted_prior[[t]][[i]] <- models_prior[[t]][[i]](df_t)
        weights_prior[[i]] <- predict_dens(fitted_prior[[t]][[i]],df_t)

      }
      weights_t[[t]] <- bind_cols(df_t %>% select(id), weights) %>% mutate(time=t)
      weights_t_prior[[t]] <- bind_cols(df_t %>% select(id), weights_prior) %>% mutate(time=t)
    }
  } else {

  }
  weights_t <- bind_rows(weights_t)
  weights_t_prior <- bind_rows(weights_t_prior)


  df_weights <- full_join(
    weights_t %>% gather("treatment","pw",-id,-time) %>%
      mutate(lpw=log(pw)) %>%
      group_by(id,treatment) %>%
      summarize(slpw=sum(lpw),n=n()),
    weights_t_prior %>% gather("treatment","pw",-id,-time) %>%
      mutate(lpw=log(pw)) %>%
      group_by(id,treatment) %>%
      summarize(prior_slpw=sum(lpw),n_prior=n()), by=c("id","treatment")
  ) %>%
    mutate(logweight=prior_slpw-slpw)

  #pat_id1 <- df_weights %>% group_by(id) %>% summarize(weight=exp(sum(logweight))) %>% filter(!is.na(weight)) %>% .$id
  #pat_id2 <- df %>% filter(time==T) %>% filter(!censored) %>% .$id
  #print(setdiff(pat_id1,pat_id2))

  df_f <- df %>%
    filter(time==T) %>%
    left_join(df_weights %>% group_by(id) %>% summarize(weight=exp(sum(logweight))),by="id") %>%
    filter(!is.na(weight))

  #print(nrow(df_f)-length(pat_id2))

  if (class(msm)=="formula") {
    out <- lm(msm, df_f, weights = weight, model=save, qr=save, na.action = na.omit)
  } else {
    out <- map(msm,~lm(.x, df_f, weights = weight, model=save, qr=save, na.action = na.omit))
  }

  if (save) {
    attr(out,"df_weights") <- df_weights
    attr(out,"models") <- fitted
    attr(out,"models_prior") <- fitted_prior
  }
  class(out) <- c(class(out),"ipw")
  out

}

msm_naive <- function(data_varying, msm, models, models_prior, data_constant=NULL, data_last=NULL, pool=FALSE) {

  T <- max(data_varying$time)
  if (T==0) stop("Only one time point provided")

  df <- data_varying %>% add_lags2(T)

  if (!is.null(data_last)) {
    df <- df %>% left_join(data_last %>% mutate(time=T),by=c("id","time"))
  }

  if (!is.null(data_constant)) {
    df <- df %>% left_join(data_constant,by="id")
  }

  df_f <- df %>%
    filter(time==T)

  if (class(msm)=="formula") {
    out <- lm(msm, df_f)
  } else {
    out <- map(msm,~lm(.x, df_f))
  }

  class(out) <- c(class(out),"ipw")
  out

}



