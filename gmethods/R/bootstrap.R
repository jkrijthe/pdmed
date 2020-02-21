
select_patno <- function(patno, unique_id, df_measurements, df_outcomes) {
  randid <- paste(patno,unique_id,sep = "-")
  list(
    df_measurements =
      df_measurements %>%
      filter(id==patno) %>%
      mutate(id=randid),
    df_outcomes=
      df_outcomes %>%
      filter(id==patno) %>%
      mutate(id=randid)
  )
}

bootstrap_sample <- function(patnos,df_measurements, df_outcomes) {
  patno_selected <- sample(patnos,length(patnos),replace = TRUE)
  map2(patno_selected, 1:length(patno_selected), select_patno, df_measurements, df_outcomes) %>%
    purrr::transpose() %>%
    lapply(bind_rows)
}

select_patno2 <- function(patno, unique_id, df_measurements, df_outcomes,df_outcomes2) {
  randid <- paste(patno,unique_id,sep = "-")
  list(
    df_measurements =
      df_measurements %>%
      filter(id==patno) %>%
      mutate(id=randid),
    df_outcomes=
      df_outcomes %>%
      filter(id==patno) %>%
      mutate(id=randid),
    df_outcomes2=
      df_outcomes2 %>%
      filter(id==patno) %>%
      mutate(id=randid)
  )
}

bootstrap_sample2 <- function(patnos,df_measurements, df_outcomes, df_outcomes2) {
  patno_selected <- sample(patnos,length(patnos),replace = TRUE)
  map2(patno_selected, 1:length(patno_selected), select_patno2, df_measurements, df_outcomes,df_outcomes2) %>%
    purrr::transpose() %>%
    lapply(bind_rows)
}
