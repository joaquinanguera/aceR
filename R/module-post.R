
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
#' to \emph{include} in the output. All column names containing any of the inputs
#' will be included in output.
#' @param metric_names_exclude a character vector containing partial names of metric
#' columns to \emph{exclude} from the output. All column names containing any of the
#' inputs will be excluded from output. This argument supersedes the prior, so any
#' columns matching both a partial name from \code{metric_names} and a partial name
#' from \code{metric_names_exclude} will be excluded.
#' @return a df, similar in structure to \code{proc}, but containing only the
#' subsetted columns.

post_reduce_cols <- function (df,
                              demo_names = c("pid", "age", "grade", "gender"),
                              metric_names = c(),
                              metric_names_exclude = c()) {
  
  call_metric_names <- paste0(metric_names, collapse = "|")
  call_metric_names_exclude <- paste0(metric_names_exclude, collapse = "|")
  
  if (all(c("module", "proc") %in% names(df))) {
    # if was processed with output = "long"
    df_reduced <- df %>%
      mutate(proc = map(proc, ~.x %>%
                          select(!!!syms(demo_names), matches(call_metric_names))))
    
    if (length(metric_names_exclude) > 0) {
      df_reduced %<>%
        mutate(proc = map(proc, ~.x %>%
                            select(-matches(call_metric_names_exclude))))
    }
    
  } else {
    # if was processed with output = "wide"
    df_reduced <- df %>%
      select(!!!syms(demo_names), matches(call_metric_names))
    
    if (length(metric_names_exclude) > 0) {
      df_reduced %<>% select(-matches(call_metric_names_exclude))
    }
  }
  
  return(df_reduced)
  
}

#' Scrub processed data with below-chance accuracy
#' 
#' User-friendly wrapper to replace below-chance records with \code{NA}
#' in ACE data processed with \code{\link{proc_by_module}}. Currently only
#' compatible with ACE (SEA not yet implemented),
#' 
#' @export
#' @importFrom dplyr case_when everything filter left_join mutate select tibble
#' @importFrom magrittr %>% %<>%
#' @importFrom purrr map map2 reduce
#' @importFrom rlang sym !!
#' 
#' @param df a df, output by \code{\link{proc_by_module}}, containing processed
#' ACE data.
#' @param app_type character. What app type produced this data? One of
#' \code{c("classroom", "explorer")}. Must be specified.
#' @param overall Also scrub ".overall" data? Defaults to \code{TRUE}.
#' @param cutoff_dprime Minimum value of d' to allow, for relevant tasks
#' (ACE Tap and Trace, SAAT, Filter). Defaults to 0.
#' @param cutoff_2choice Minimum value of accuracy to allow, for 2-response tasks
#' (ACE Flanker, Boxed). Defaults to 0.5.
#' @param cutoff_4choice Minimum value of accuracy to allow, for 4-response tasks
#' (ACE Stroop, Task Switch). Defaults to 0.25.
#' @param cutoff_5choice Minimum value of accuracy to allow, for 5-response tasks
#' (ACE Color Selection). Defaults to 0.25.
#' @param extra_demos Character vector specifying any custom-added demographics
#' columns (beyond app defaults) to pass through the function. Defaults to \{code{NULL}.
#' @return a df, similar in structure to \code{proc}, but with below-cutoff values in
#' certain columns converted to \code{NA}.

post_clean_chance <- function (df,
                               app_type = c("classroom", "explorer"),
                               overall = TRUE,
                               cutoff_dprime = 0,
                               cutoff_2choice = 0.5,
                               cutoff_4choice = 0.25,
                               cutoff_5choice = 0.2,
                               extra_demos = NULL) {
  
  stopifnot(length(app_type) == 1)
  stopifnot(app_type != "sea")
  
  metric_cols <- tibble(module = c(TNT,
                                   STROOP,
                                   FLANKER,
                                   TASK_SWITCH,
                                   BOXED,
                                   SAAT_SUS,
                                   SAAT_IMP,
                                   FILTER,
                                   ADP,
                                   COLOR_SELECT))
  # If SEA modules were getting included they would get concatenated right above here
  # and in the analogous metric_cols bits below
  
  stopifnot(length(app_type) == 1)
  
  is_ace = app_type != "sea"
  
  # Number of responses went from 4 in Classroom to 2 in Explorer
  if (app_type == "classroom") {
    cutoff_taskswitch <- cutoff_4choice
  } else if (app_type == "explorer") {
    cutoff_taskswitch <- cutoff_2choice
  } else {
    cutoff_taskswitch <- NA_real_
  }
  
  if (overall) {
    metric_cols %<>%
      mutate(metric = list(c("sdt_dprime.overall"),
                           c("acc_mean.overall"),
                           c("acc_mean.overall"),
                           c("acc_mean.overall"),
                           c("acc_mean.overall"),
                           c("sdt_dprime.overall"),
                           c("sdt_dprime.overall"),
                           c("k.r2b0", "k.r4b0"),
                           c("acc_mean.overall"),
                           c("acc_mean.overall.strict")))
  } else {
    metric_cols %<>%
      mutate(metric = list(c("sdt_dprime.tap_only"),
                           c("acc_mean.congruent"),
                           c("acc_mean.congruent"),
                           c("acc_mean.stay"),
                           c("acc_mean.feature_4"),
                           c("sdt_dprime.overall"),
                           c("sdt_dprime.overall"),
                           c("k.r2b0", "k.r4b0"),
                           c("acc_mean.overall"),
                           c("acc_mean.overall.strict")))
  }
  
  metric_cols %<>%
    mutate(full = map2(module, metric, ~paste(.x, .y, sep = ".")),
           cutoff = case_when(module %in% c(TNT, SAAT_SUS, SAAT_IMP, FILTER) ~ cutoff_dprime,
                              module == STROOP ~ cutoff_4choice,
                              module %in% c(FLANKER,
                                            BOXED,
                                            ADP) ~ cutoff_2choice,
                              module == TASK_SWITCH ~ cutoff_taskswitch,
                              module == COLOR_SELECT ~ cutoff_5choice,
                              TRUE ~ NA_real_)) %>%
    # this step should implicitly take of making metric_cols ace only or sea only
    # depending on the input data
    filter(module %in% get_valid_modules(df, is_ace = is_ace))
  
  if (!(all(c("module", "proc") %in% names(df)))) {
    # if "wide" form, coerce to "long-ish" first
    wide <- TRUE
    check_extra_demos(df, wide, extra_demos, is_ace = is_ace)
    valid_demos <- get_valid_demos(df, is_ace = is_ace)
    # because for ACE Classroom, the "safe" demos extend beyond the shared bindable demos
    valid_demos_rejoin <- valid_demos
    filter_col <- sym("full")
    df <- proc_wide_to_long(df, extra_demos, is_ace = is_ace)
  } else {
    wide <- FALSE
    filter_col <- sym("metric")
    check_extra_demos(df, wide, extra_demos, is_ace = is_ace)
    valid_demos <- get_valid_demos(df$proc[[1]], is_ace = is_ace)
  }
  
  valid_demos <- c(valid_demos, extra_demos)
  
  # so the unbindable non-shared ACE Classroom demos don't get coerced to logical in the "bad" data
  if (app_type == "classroom") valid_demos <- c(valid_demos, get_ace_classroom_wide_bids(df))
  
  df %<>%
    left_join(metric_cols %>%
                select(module,
                       filter_cols = !!filter_col,
                       cutoff),
              by = "module") %>%
    mutate(non_demo_cols = map(proc, ~names(.x)[!(names(.x) %in% valid_demos)]),
           call_filter_cols_bad = map2(filter_cols, cutoff,  ~map_call2_rel("<=", .x, .y)),
           call_filter_cols_good = map2(filter_cols, cutoff,  ~map_call2_rel(">", .x, .y)))
  
  df_scrubbed <- proc_na_all_by_some_cols(df)
  
  # If "wide" form:
  if (wide) {
    df_scrubbed$proc %>%
      reduce(dplyr::full_join, by = valid_demos_rejoin) %>%
      select(valid_demos, everything()) %>%
      return()
  } else {
    return (df_scrubbed)
  }
}

#' Scrub processed data with too few trials
#' 
#' User-friendly wrapper to replace records with too many no-responses with \code{NA}
#' in ACE data processed with \code{\link{proc_by_module}}.
#' 
#' @export
#' @importFrom dplyr bind_rows everything filter mutate mutate_at select
#' @importFrom magrittr %>% %<>%
#' @importFrom purrr map map2 map_if reduce
#' 
#' @param df a df, output by \code{\link{proc_by_module}}, containing processed
#' ACE or SEA data.
#' @param app_type character. What app type produced this data? One of
#' \code{c("classroom", "explorer")}. Must be specified.
#' @param min_trials Minimum number of trials to require in most restrictive condition.
#' Defaults to 5. This condition is checked against the \code{*_count} summary columns,
#' that count all trials with a valid response time (and all no-go trials, if a response
#' was not expected.)
#' @param extra_demos Character vector specifying any custom-added demographics
#' columns (beyond app defaults) to pass through the function. Defaults to \{code{NULL}.
#' @return a df, similar in structure to \code{proc}, but with records with too few trials
#' converted to \code{NA}.

post_clean_low_trials <- function (df,
                                   app_type = c("classroom", "explorer"),
                                   min_trials = 5,
                                   extra_demos = NULL) {
  stopifnot(length(app_type) == 1)
  stopifnot(app_type != "sea")
  
  is_ace = app_type != "sea"
  
  if (!(all(c("module", "proc") %in% names(df)))) {
    # if "wide" form, coerce to "long-ish" first
    wide <- TRUE
    check_extra_demos(df, wide, extra_demos, is_ace = is_ace)
    valid_demos <- get_valid_demos(df, is_ace = is_ace)
    valid_demos_rejoin <- valid_demos
    df <- proc_wide_to_long(df, extra_demos, is_ace = is_ace)
  } else {
    wide <- FALSE
    # as long as the demos are present somewhere this should be ok
    # since these aren't join cols later it doesn't have to be a perfect match
    check_extra_demos(df, wide, extra_demos, is_ace = is_ace)
    valid_demos <- get_valid_demos(df$proc[[1]], is_ace = is_ace)
  }
  
  valid_demos <- c(valid_demos, extra_demos)
  
  if (app_type == "classroom") valid_demos <- c(valid_demos, get_ace_classroom_wide_bids(df))
  
  # The easiest way to do it will be to work inside a df that only contains one module's data
  # aka "long" form
  
  # If "long" form:
  # filter just the bad ones, select only their demo cols, bind_rows back on to the good ones
  # will then populate all the non-demo cols with NA
  
  df %<>%
    mutate(filter_cols = map(proc, ~names(.x)[grepl("count", names(.x)) & 
                                                !grepl("half", names(.x)) & 
                                                !grepl("correct", names(.x)) & 
                                                !grepl("cost", names(.x)) & 
                                                !grepl("start", names(.x)) & 
                                                !grepl("early", names(.x)) & 
                                                !grepl("hit", names(.x)) & 
                                                !grepl("miss", names(.x)) & 
                                                !grepl("cr", names(.x)) & 
                                                !grepl("fa", names(.x)) & 
                                                !grepl("practice", names(.x))]),
           filter_cols = map_if(filter_cols, module == "SAAT", ~.x[!grepl("overall", .x)], .else = ~.x),
           non_demo_cols = map(proc, ~names(.x)[!(names(.x) %in% valid_demos)]),
           call_filter_cols_bad = map(filter_cols,  ~map_call2_rel("<", .x, min_trials)),
           call_filter_cols_good = map(filter_cols,  ~map_call2_rel(">=", .x, min_trials, include_nas = TRUE)))
  
  df_scrubbed <- proc_na_all_by_some_cols(df)
  
  # If "wide" form:
  if (wide) {
    df_scrubbed$proc %>%
      reduce(dplyr::full_join, by = valid_demos_rejoin) %>%
      select(valid_demos, everything()) %>%
      return()
  } else {
    return (df_scrubbed)
  }
}

#' Helper function that takes long or long-ish ACE proc data
#' and renders all non-demographic columns as NA for those rows
#' that satisfy a condition
#' expects filter_cols, call_filter_cols_bad, call_filter_cols_good, non_demo_cols
#' @importFrom dplyr bind_rows filter mutate mutate_at select
#' @importFrom magrittr %>% %<>%
#' @importFrom purrr map map2
#' @importFrom rlang !!!
#' @keywords internal

proc_na_all_by_some_cols <- function (df) {
  
  df_no_scrub <- df %>%
    filter(lengths(filter_cols) == 0) %>%
    select(module, proc)
  
  df_scrubbed <- df %>%
    filter(lengths(filter_cols) > 0)
  
  df_scrubbed$proc_bad <- map2(df_scrubbed$proc, df_scrubbed$call_filter_cols_bad, ~filter(.x, !!!.y))
  df_scrubbed$proc_good <- map2(df_scrubbed$proc, df_scrubbed$call_filter_cols_good, ~filter(.x, !!!.y))
  
  df_scrubbed %<>%
    mutate(proc_bad = map2(proc_bad, non_demo_cols, ~mutate_at(.x, .y, to_na)),
           proc_scrubbed = map2(proc_bad, proc_good, ~bind_rows(.y, .x))) %>%
    select(module, proc = proc_scrubbed) %>%
    bind_rows(df_no_scrub)
  
  return (df_scrubbed)
}

#' @keywords internal
check_extra_demos <- function (df, is_wide, extra_demos, is_ace) {
  if (is_ace) {
    all_possible_demos <- ALL_POSSIBLE_DEMOS
    all_modules <- ALL_MODULES
  } else {
    all_possible_demos <- ALL_POSSIBLE_SEA_DEMOS
    all_modules <- ALL_SEA_MODULES
  }
  
  if (is_wide) {
    # if "wide" form, coerce to "long-ish" first
    if (!is.null(extra_demos)) {
      if (!all(extra_demos %in% (names(df)))) {
        stop(crayon::red("Extra demo cols not found! Check spelling?"))
      }
    } else {
      if (!all(names(df)[!grepl(paste0("^", all_modules[all_modules != DEMOS], collapse = "|"), names(df))] %in% all_possible_demos)) {
        warning(crayon::yellow("Possible extra demo cols detected but argument not specified, ignoring."))
      }
    }
  } else {
    if (!is.null(extra_demos)) {
      if (!any(map_lgl(df$proc, ~all(extra_demos %in% names(.x))))) {
        stop(crayon::red("Extra demo cols not found! Check spelling?"))
      }
    }
  }
  return (NULL)
}

#' Most module-post functions work much better with long proc data.
#' This will be called under the hood to coerce wide data to long-ish
#' Because it seems that users prefer the wide data
#' @importFrom dplyr distinct mutate select
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @keywords internal

proc_wide_to_long <- function (df, extra_demos, is_ace) {
  valid_modules <- get_valid_modules(df, is_ace = is_ace)
  valid_demos <- get_valid_demos(df, is_ace = is_ace)
  valid_demos <- c(valid_demos, extra_demos)
  out <- tibble(module = valid_modules,
                proc = vector("list", length(valid_modules))) %>%
    mutate(proc = map(module, ~ df %>%
                        select(valid_demos, dplyr::starts_with(.x)) %>%
                        distinct()))
  # Note that the "long-ish" output will still have module names preprended to colnames (for now)
  return (out)
}

#' @importFrom purrr map_lgl
#' @keywords internal

get_valid_modules <- function (df, is_ace) {
  if (is_ace) {
    all_modules <- ALL_MODULES
  } else {
    all_modules <- ALL_SEA_MODULES
  }
  
  if (all(c("module", "proc") %in% names(df))) {
    # if was processed with output = "long"
    modules <- all_modules[map_lgl(all_modules, ~.x %in% df[[COL_MODULE]])]
  } else {
    # else if was processed with output = "wide"
    modules <- all_modules[map_lgl(all_modules, ~any(grepl(.x, names(df))))]
  }
  if (SAAT_SUS %in% modules | SAAT_IMP %in% modules) modules <- modules[modules != SAAT]
  
  return (modules)
}

#' @keywords internal

get_ace_classroom_wide_bids <- function (df) {
  valid_modules <- get_valid_modules(df, is_ace = TRUE)
  return (c(paste0(valid_modules, ".bid"), paste0(valid_modules, ".time")))
}
