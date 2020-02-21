
add_lags <- function(df, max) {
  # See: https://stackoverflow.com/questions/44750761/adding-multiple-lag-variables-using-dplyr-and-for-loops
  df %>%
    group_by(id) %>%
    arrange(time) %>%
    nest %>%
    mutate(lags = map(data, function(dat) {
      imap_dfc(dat %>% select(-time), ~set_names(map(1:(max), lag, x = .x),
                                                 paste0(.y, '_lag', 1:(max))))
    })) %>%
    unnest()
}


add_lags2 <- function(df, max) {
  # See: https://stackoverflow.com/questions/44750761/adding-multiple-lag-variables-using-dplyr-and-for-loops
  setDT(df)
  d<-ncol(df)
  nn <- setdiff(colnames(df),c("id","time","ido"))
  as_tibble(df[, sapply(nn, function(x){paste0(x, '_lag', 1:max)}) := shift(.SD, 1:max),
               by = id, .SDcols = nn][])
}
