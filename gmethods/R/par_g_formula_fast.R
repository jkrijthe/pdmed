predict_mc.default <- function(object, new_data) { UseMethod("predict_mc") }
setGeneric("predict_mc",predict_mc.default)
#stop("Define a predict_mc method for your model.");


#' @param data_varying time-varying covariates
#' @param data_constant fixed covariates
#' @param pool Single model across time points
par_g_formula_fast <- function(data_varying, msm, models, data_constant=NULL, data_last=NULL, pool=FALSE, mc_samples=1, save=TRUE, treatment_assignment) {

  #TODO: if id and time not given, stop
  T <- max(data_varying$time)
  if (T==0) stop("Only one time point provided")

  df <- data_varying %>% add_lags2(T)

  if (!is.null(data_last)) {
    df <- df %>% left_join(data_last %>% mutate(time=T),by=c("id","time"))
  }

  if (!is.null(data_constant)) {
    df <- df %>% left_join(data_constant,by="id")
  }

  fitted <- rep(list(list()),times=T)
  # Train model for each time step
  if (!pool) {
    df <- df
    for (t in seq_len(T)) {
      K <- length(models[[t]])
      for (i in seq_len(K)) {
        df_t <- df %>% filter(time==t)
        fitted[[t]][[i]] <- models[[t]][[i]](df_t)

      }
    }
  } else {
    #TODO: pool data across time points
  }

  treats <- treatment_assignment

  sim <- function(treat,df_start) {

    df_treatment <- bind_rows(rep(list(treat),each=length(unique(df_start$ido)))) %>% mutate(ido=rep(unique(df_start$ido),each=nrow(treat)))

    blocks <- rep(list(),T+1)
    blocks[[1]] <- df_start %>% select(-id,-time,-ido)

    if (!is.null(data_constant)) {
      df_start_with_const <- df_start %>% select(id,ido) %>% left_join(data_constant,by=c("ido"="id"))
    } else {
     df_start_with_const <- df_start %>% select(id,ido)
    }

    for (t in seq_len(T)) {

      lagged_names <- function(data,l) {
        names(data) <- paste0(names(data),"_lag", l)
        data
      }

      df_t <- bind_cols(df_start_with_const,
                        map2(blocks,length(blocks):1,lagged_names)) %>% left_join(df_treatment %>% filter(time==t),by=c("ido"))

      K <- length(fitted[[t]])
      new_block <-  df_t %>% select(treatment)
      df_pred <- df_t
      for (i in seq_len(K)) {
        new_block <- bind_cols(new_block, predict_mc(fitted[[t]][[i]],df_pred))
        df_pred <- bind_cols(df_t,new_block)
      }
      blocks[[t+1]] <- new_block
    }
    df_t <- bind_cols(df_t, blocks[[T+1]])
    df_t

  }
  df_start <- map_dfr(1:mc_samples, ~data_varying %>% filter(time==0) %>% select(-treatment) %>% mutate(ido=id,id=paste0(id,"_rep",.x)))

  df_f <- bind_rows(map(treats,~sim(.x,df_start)))

  # Apply marginal structural model to df_mc
  if (class(msm)=="formula") {
    out <- lm(msm, df_f, model=save, qr=save)
  } else {
    out <- map(msm,~lm(.x, df_f, model=save, qr=save))
  }
  if (save) {
    attr(out,"df_mc") <- df_f
    attr(out,"fitted") <- fitted
  }
  out
}
