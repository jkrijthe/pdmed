predict_mc.default <- function(object, new_data) { UseMethod("predict_mc") }
setGeneric("predict_mc",predict_mc.default)
#stop("Define a predict_mc method for your model.");


#' @param data_varying time-varying covariates
#' @param data_constant fixed covariates
#' @param pool Single model across time points
par_g_formula <- function(data_varying, msm, models, data_constant=NULL, pool=FALSE, mc_samples=1, save=TRUE) {

  #TODO: if id and time not given, stop
  T <- max(data_varying$time)
  if (T==0) stop("Only one time point provided")

  df <- data_varying %>% add_lags2(T)

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

  # Simulate from model for assignment
  # Starting frame
  if (T==4) {
  treats <- list(
    data.frame(time=1:4,treatment=c(0,0,0,0)),
    data.frame(time=1:4,treatment=c(0,0,0,1)),
    data.frame(time=1:4,treatment=c(0,0,1,1)),
    data.frame(time=1:4,treatment=c(0,1,1,1)),
    data.frame(time=1:4,treatment=c(1,1,1,1))
  ) }
  else {
    treats <- list(
      data.frame(time=1:8,treatment=c(0,0,0,0,0,0,0,0)),
      data.frame(time=1:8,treatment=c(0,0,0,0,0,0,0,1)),
      data.frame(time=1:8,treatment=c(0,0,0,0,0,0,1,1)),
      data.frame(time=1:8,treatment=c(0,0,0,0,0,1,1,1)),
      data.frame(time=1:8,treatment=c(0,0,0,0,1,1,1,1)),
      data.frame(time=1:8,treatment=c(0,0,0,1,1,1,1,1)),
      data.frame(time=1:8,treatment=c(0,0,1,1,1,1,1,1)),
      data.frame(time=1:8,treatment=c(0,1,1,1,1,1,1,1)),
      data.frame(time=1:8,treatment=c(1,1,1,1,1,1,1,1))
    )
  }

  sim <- function(treat,mc_samples) {

    df_start <- map_dfr(1:mc_samples, ~data_varying %>% filter(time==0) %>% select(-treatment) %>% mutate(ido=id,id=paste0(id,"_rep",.x)))

    df_mc <- df_start
    df_treatment <- bind_rows(rep(list(treat),each=length(unique(df_mc$ido)))) %>% mutate(ido=rep(unique(df_mc$ido),each=nrow(treat)))

    for (t in seq_len(T)) {
      df_s <- bind_rows(df_mc, df_start %>% mutate(time=t)) # Add new time point
      df_s <- df_s %>% left_join(df_treatment,by=c("ido","time")) # Add treatment assignment

      # Constuct lagged frame
      df_t <- df_s %>% add_lags2(T) %>%
        select(id,ido, time,contains("treatment"),contains("_lag")) %>%
        filter(time==t)

      K <- length(fitted[[t]])
      for (i in seq_len(K)) {
        if (!is.null(data_constant)) {
          df_t <- bind_cols(df_t, predict_mc(fitted[[t]][[i]],df_t %>% left_join(data_constant,by=c("ido"="id"))))
        } else {
          df_t <- bind_cols(df_t, predict_mc(fitted[[t]][[i]],df_t))
        }
      }
      df_mc <- bind_rows(df_mc,df_t %>% select(-contains("treatment"),-contains("_lag")))
    }
    df_mc %>% left_join(df_treatment,by=c("ido","time"))
  }

  df_mc <- bind_rows(map(treats,~sim(.x,mc_samples)))
  df_f <- df_mc %>% add_lags2(T) %>% filter(time==T)
  # Simulate for each assignment
  # %>% arrange(id,time)
  if (!is.null(data_constant)) {
    df_f <- df_f %>% left_join(data_constant,by=c("ido"="id"))
  }

  # Apply marginal structural model to df_mc
  if (class(msm)=="formula") {
    out <- lm(msm, df_f, model=FALSE, qr=FALSE)
  } else {
    out <- map(msm,~lm(.x, df_f, model=FALSE, qr=FALSE))
  }
  if (save) {
    attr(out,"df_mc") <- df_mc
    attr(out,"fitted") <- fitted
  }
  out
}
