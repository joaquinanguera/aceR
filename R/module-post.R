
#' Subset columns from processed ACE/SEA modules
#' 
#' User-friendly wrapper to choose demographics and selected
#' outcome variables from ACE/SEA data processed with \code{\link{proc_by_module}}.
#' 
#' @export
#' @importFrom dplyr matches mutate select
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom rlang enexprs syms !!!
#' 
#' @param df a df, output by \code{\link{proc_by_module}}, containing processed
#' ACE or SEA data.
#' @param demo_names a character vector containing the names of demographic columns
#' to include in the output. Column names will be matched exactly.
#' @param metric_names a character vector containing partial names of metric columns
#' to include in the output. All column names containing any of the inputs will be
#' included in output.
#' @return a df, similar in structure to \code{proc}, but containing only the
#' subsetted columns.

post_reduce_cols <- function (df,
                              demo_names = c("pid", "age", "grade", "gender"),
                              metric_names = c()) {
  
  call_metric_names <- paste0(metric_names, collapse = "|")
  
  if (all(c("module", "proc") %in% names(df))) {
    # if was processed with output = "long"
    df_reduced <- df %>%
      mutate(proc = map(proc, ~.x %>%
                          select(!!!syms(demo_names), matches(call_metric_names))))
  } else {
    # if was processed with output = "wide"
    df_reduced <- df %>% select(!!!syms(demo_names), matches(call_metric_names))
  }
  
  return(df_reduced)
  
}

#' Scrub processed data with below-chance accuracy
#' 
#' User-friendly wrapper to replace below-chance records with \code{NA}
#' in ACE/SEA data processed with \code{\link{proc_by_module}}.
#' 
#' @export
#' @importFrom dplyr case_when filter mutate select tibble
#' @importFrom magrittr %>%
#' @importFrom purrr map map2 pmap
#' @importFrom rlang sym !! :=
#' @importFrom tidyr unnest
#' 
#' @param df a df, output by \code{\link{proc_by_module}}, containing processed
#' ACE or SEA data.
#' @param overall Also scrub ".overall" data? Defaults to \code{TRUE}.
#' @param cutoff_dprime Minimum value of d' to allow, for relevant tasks
#' (Tap and Trace, SAAT, Filter). Defaults to 0.
#' @param cutoff_2choice Minimum value of accuracy to allow, for 2-response tasks
#' (Flanker, Boxed). Defaults to 0.5.
#' @param cutoff_4choice Minimum value of accuracy to allow, for 4-response tasks
#' (Stroop, Task Switch). Defaults to 0.25.
#' @return a df, similar in structure to \code{proc}, but with below-cutoff values in
#' certain columns converted to \code{NA}.

post_clean_chance <- function (df, overall = TRUE, cutoff_dprime = 0, cutoff_2choice = 0.5, cutoff_4choice = 0.25) {
  
  metric_cols <- tibble(module = c(TNT,
                                   STROOP,
                                   FLANKER,
                                   TASK_SWITCH,
                                   BOXED,
                                   SAAT,
                                   FILTER))
  if (overall) {
    metric_cols <- metric_cols %>%
      mutate(metric = list(c("dprime.overall", "dprime.tap_only"),
                           c("acc_mean.overall", "acc_mean.congruent"),
                           c("acc_mean.overall", "acc_mean.congruent"),
                           c("acc_mean.overall", "acc_mean.stay"),
                           c("acc_mean.overall", "acc_mean.feature_4"),
                           c("dprime.overall", "dprime.sustained", "dprime.impulsive"),
                           c("k.R2B0", "k.R4B0")))
  } else {
    metric_cols <- metric_cols %>%
      mutate(metric = list(c("dprime.tap_only"),
                           c("acc_mean.congruent"),
                           c("acc_mean.congruent"),
                           c("acc_mean.stay"),
                           c("acc_mean.feature_4"),
                           c("dprime.sustained", "dprime.impulsive"),
                           c("k.R2B0", "k.R4B0")))
  }
  
  metric_cols <- metric_cols %>%
    mutate(full = map2(module, metric, ~paste(.x, .y, sep = ".")),
           cutoff = case_when(module %in% c(TNT, SAAT, FILTER) ~ cutoff_dprime,
                              module %in% c(STROOP, TASK_SWITCH) ~ cutoff_4choice,
                              module %in% c(FLANKER, BOXED) ~ cutoff_2choice,
                              TRUE ~ NA_real_))
  
  if (all(c("module", "proc") %in% names(df))) {
    # if was processed with output = "long"
    df_scrubbed <- df %>%
      left_join(metric_cols, by = "module")
    
    df_nonscrubbed <- df_scrubbed %>%
      filter(is.na(cutoff))
    
    df_scrubbed <- df_scrubbed %>%
      filter(!is.na(cutoff)) %>%
      mutate(proc = pmap(list(proc, metric, cutoff), function (a, b, c) {
        # note: quosures don't seem to work inside pmap()
        for (this_b in b) {
          a[[this_b]] <- na_if_true(a[[this_b]], a[[this_b]] <= c)
        }
        return (a)
      }
      )) %>%
      bind_rows(df_nonscrubbed) %>%
      select(-metric, -full, -cutoff)

  } else {
    metric_cols <- metric_cols %>%
      select(full, cutoff) %>%
      unnest()
    # if was processed with output = "wide"
    df_scrubbed <- df
    for (metric_col in 1:nrow(metric_cols)) {
      df_scrubbed[[metric_cols$full[metric_col]]] <- na_if_true(df_scrubbed[[metric_cols$full[metric_col]]],
                                                                df_scrubbed[[metric_cols$full[metric_col]]] <= metric_cols$cutoff[metric_col])
    }
  }
return (df_scrubbed)
}
