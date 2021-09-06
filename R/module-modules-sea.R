
#' @keywords internal
#' @name sea_procs
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter

attempt_module <- function(df, module, app_type, verbose) {
  if (module %in% c(SAAT_SUS, SAAT_IMP)) {
    module_function <- SAAT
  } else {
    module_function <- module
  }
  if ("practice" %in% names(df)) {
    if (is.character(df[["practice"]])) {
      df <- filter(df, practice != "True")
    } else if (is.logical(df[["practice"]])) {
      df <- filter(df, !practice | is.na(practice)) 
      }
  }
  out <- tryCatch({
    
    if (module_function == SAAT) {
      df <- do.call(paste0("module_", tolower(module_function)), list(df = df, app_type = app_type)) 
    } else {
      df <- do.call(paste0("module_", tolower(module_function)), list(df = df)) 
    }
    
    df %<>%
      tibble::as_tibble() %>%
      clean_proc_cols()
    if (verbose) cat(crayon::green("Processed", module), sep = "\n")
    return(df)
  }, error = function (e) {
    warning(crayon::red("Unable to process", module), sep = "\n")
    return (data.frame())
  })
}

#' @keywords internal
#' @name sea_procs
#' @importFrom dplyr tibble mutate
#' @importFrom purrr map map2 reduce

module_math_fluency <- function(df) {
  out <- tibble(condition = c(COL_CONDITION, "operation_type", "answer_size"),
                cost_args = list(c("\\.switch", "\\.stay", "\\.switch_cost"),
                                 c("\\.subtraction", "\\.addition", "\\.operation_cost"),
                                 c("\\.2", "\\.1", "\\.answer_size_cost"))) %>%
    mutate(gen = map(condition, function(x) proc_generic_module(df, col_condition = rlang::sym(x), FUN = sea_descriptive_statistics)),
           cost = map2(gen, cost_args, ~multi_subtract(.x, .y[1], .y[2], .y[3])),
           both = map2(gen, cost, ~dplyr::bind_cols(.x, .y)),
           names = map(both, ~names(.x)))
  
  duplicate_cols <- unique(unlist(out$names)[duplicated(unlist(out$names))])
  
  out <- purrr::reduce(out$both, full_join, by = duplicate_cols)
  
  time <- proc_by_condition(df, "feedback_onset", include_overall = F, FUN = sea_task_duration)
  
  return (dplyr::full_join(out, time, by = COL_BID))
}

#' @keywords internal
#' @name sea_procs
#' @importFrom dplyr filter tibble mutate
#' @importFrom purrr map map2 map_dbl reduce

module_math_recall <- function(df) {
  out <- tibble(condition = c("carrying", "math_recall_orientation", "operation_type", "answer_size"),
                cost_args = list(c("\\.carry", "\\.no_carry",  "\\.carry_cost"),
                                 c("\\.vertical", "\\.horizontal",  "\\.orientation_cost"),
                                 c("\\.subtraction", "\\.addition", "\\.operation_cost"),
                                 c("\\.2", "\\.1", "\\.answer_size_cost"))) %>%
    mutate(gen = map(condition, function(x) proc_generic_module(df, col_condition = rlang::sym(x), FUN = sea_descriptive_statistics)),
           cost = map2(gen, cost_args, ~multi_subtract(.x, .y[1], .y[2], .y[3])),
           both = map2(gen, cost, ~dplyr::bind_cols(.x, .y)),
           names = map(both, ~names(.x))) %>%
    # Sometimes (old data?) math_recall_orientation doesn't appear in the raw data
    # so can't calculate anything along this
    filter(map_dbl(cost, ~nrow(.)) > 0)
  
  duplicate_cols <- unique(unlist(out$names)[duplicated(unlist(out$names))])
  return (purrr::reduce(out$both, full_join, by = duplicate_cols))
}

#' @keywords internal
#' @name sea_procs

module_reading_fluency <- function(df) {
  gen = proc_generic_module(df, col_condition = NULL, FUN = sea_reading_descriptive_statistics)
  gen = dplyr::select(gen, -dplyr::contains("correct_button_mean"))
  time = proc_by_condition(df, "feedback_onset", include_overall = F, FUN = sea_task_duration)
  return (dplyr::left_join(gen, time, by = COL_BID))
}

#' @keywords internal
#' @name sea_procs

module_reading_comprehension <- function(df) {
  gen = proc_generic_module(df, col_condition = NULL, FUN = sea_reading_descriptive_statistics)
  gen = dplyr::select(gen, -dplyr::contains("correct_button_mean"))
  time = proc_by_condition(df, "feedback_onset", include_overall = F, FUN = sea_task_duration)
  return (dplyr::left_join(gen, time, by = COL_BID))
}


#' @keywords internal
#' @name sea_procs
#' @importFrom dplyr tibble mutate
#' @importFrom purrr map map2 reduce

module_fractions_lvl_1 <- function(df) {
  
  # by left vs right and by size  but not crossed
  out <- tibble(condition = c(COL_CONDITION, "num_size"),
                cost_args = list(c("\\.left", "\\.right", "\\.cost"),
                                 c("\\.large", "\\.small", "\\.num_size_cost"))) %>%
    mutate(gen = map(condition, function(x) proc_generic_module(df, col_condition = rlang::sym(x), FUN = sea_descriptive_statistics)),
           cost = map2(gen, cost_args, ~multi_subtract(.x, .y[1], .y[2], .y[3])),
           both = map2(gen, cost, ~dplyr::bind_cols(.x, .y)),
           names = map(both, ~names(.x)))
  
  duplicate_cols <- unique(unlist(out$names)[duplicated(unlist(out$names))])
  return (reduce(out$both, full_join, by = duplicate_cols))
}

#' @keywords internal
#' @name sea_procs
#' @importFrom dplyr tibble mutate
#' @importFrom purrr map map2 reduce

module_fractions_lvl_2 <- function(df) {
  # by left vs right and by matched value but not crossed
  out <- tibble(condition = c(COL_CONDITION, "matched_value"),
                cost_args = list(c("\\.left", "\\.right", "\\.cost"),
                                 c("\\.num_matched", "\\.denom_matched", "\\.matched_value_cost"))) %>%
    mutate(gen = map(condition, function(x) proc_generic_module(df, col_condition = rlang::sym(x), FUN = sea_descriptive_statistics)),
           cost = map2(gen, cost_args, ~multi_subtract(.x, .y[1], .y[2], .y[3])),
           both = map2(gen, cost, ~dplyr::bind_cols(.x, .y)),
           names = map(both, ~names(.x)))
  
  duplicate_cols <- unique(unlist(out$names)[duplicated(unlist(out$names))])
  return (reduce(out$both, full_join, by = duplicate_cols))
}

#' @keywords internal
#' @name sea_procs
#' @importFrom dplyr tibble mutate
#' @importFrom purrr map map2 reduce

module_fractions_lvl_3 <- function(df) {
  # by left vs right and by numerator larger but not crossed
  out <- tibble(condition = c(COL_CONDITION, "congruency"),
                cost_args = list(c("\\.left", "\\.right", "\\.cost"),
                                 c("\\.incongruent", "\\.congruent", "\\.congruency_cost"))) %>%
    mutate(gen = map(condition, function(x) proc_generic_module(df, col_condition = rlang::sym(x), FUN = sea_descriptive_statistics)),
           cost = map2(gen, cost_args, ~multi_subtract(.x, .y[1], .y[2], .y[3])),
           both = map2(gen, cost, ~dplyr::bind_cols(.x, .y)),
           names = map(both, ~names(.x)))
  
  duplicate_cols <- unique(unlist(out$names)[duplicated(unlist(out$names))])
  return (reduce(out$both, full_join, by = duplicate_cols))
}

#' @keywords internal
#' @name sea_procs
#' @importFrom dplyr ends_with filter full_join select
#' @importFrom purrr map map2 reduce
#' @importFrom rlang sym

module_arithmetic_verification <- function(df) {
  
  gen = proc_generic_module(df, col_condition = sym("block_type"), FUN = sea_descriptive_statistics)
  gen_mixed = proc_generic_module(filter(df, block_type == "mixed"), FUN = sea_descriptive_statistics) %>%
    select(-ends_with(".overall"), -ends_with("_half"), -ends_with("correct"))
  gen_mixed_full = proc_generic_module(filter(df, block_type == "mixed"), col_condition = sym("switch_by_operation_type"), FUN = sea_descriptive_statistics) %>%
    select(-ends_with(".overall"), -ends_with("_half"), -ends_with("correct"))
  
  gens_mixed = full_join(gen_mixed, gen_mixed_full, by = "bid")
  
  cost = multi_subtract(gen, "\\.mixed", "\\.fixed", "\\._block_type_cost")
  cost_mixed = multi_subtract(gen_mixed,"\\.switch", "\\.stay", "\\.switch_cost")
  cost_mixed_multiplication = multi_subtract(gen_mixed_full, "\\.switch_multiplication", "\\.stay_multiplication", "\\.multiplication_switch_cost")
  cost_mixed_addition = multi_subtract(gen_mixed_full, "\\.switch_addition", "\\.stay_addition", "\\.addition_switch_cost")
  
  out = bind_cols(gen, cost)
  out_mixed = bind_cols(list(gens_mixed,
                             cost_mixed,
                             cost_mixed_multiplication,
                             cost_mixed_addition))
  return (full_join(out, out_mixed, by = "bid"))
}

#' @keywords internal
#' @name sea_procs

module_groupitizing <- function(df) {
  # calculate cost PAIRWISE (3 group number conditions)
  gen_num = proc_generic_module(df, col_condition = rlang::sym("number_groups"), FUN = sea_descriptive_statistics)
  cost_2_1 = multi_subtract(gen_num, "\\.2", "\\.1", "\\.2_1_cost")
  cost_3_1 = multi_subtract(gen_num, "\\.3", "\\.1", "\\.3_1_cost")
  cost_3_2 = multi_subtract(gen_num,  "\\.3", "\\.2", "\\.3_2_cost")
  
  out_num = dplyr::bind_cols(gen_num, cost_2_1, cost_3_1, cost_3_2)
  
  # calculate cost PAIRWISE again (3 arrangement conditions)
  gen_arr = proc_generic_module(df, col_condition = rlang::sym("arrangement"), FUN = sea_descriptive_statistics)
  cost_g_r = multi_subtract(gen_arr, "\\.random", "\\.group", "\\.group_random_cost")
  cost_s_r = multi_subtract(gen_arr, "\\.random", "\\.subitizing", "\\.subitizing_random_cost")
  cost_s_g = multi_subtract(gen_arr, "\\.group", "\\.subitizing", "\\.subitizing_group_cost")
  
  out_arr = dplyr::bind_cols(gen_arr, cost_g_r, cost_s_r, cost_s_g)
  
  duplicate_cols <- names(out_num)[names(out_num) %in% names(out_arr)]
  
  return (dplyr::full_join(out_num, out_arr, by = duplicate_cols))
}

#' @importFrom rlang sym
#' @keywords internal
#' @name sea_procs

module_running_memory_span <- function(df) {
  # this is two subtasks; letter and spatial
  # DO NOT REPORT .overall
  gen_strict = proc_by_condition(df, "correct_button_strict", sym("block_type"), FUN = sea_descriptive_statistics)
  gen_loose = proc_by_condition(df, "correct_button_loose", sym("block_type"), FUN = sea_descriptive_statistics)
  return (dplyr::full_join(gen_strict, gen_loose, by = "bid", suffix = c(".strict", ".loose")) %>% dplyr::select(-dplyr::contains(".overall"), -dplyr::starts_with("rt_")))
}

#' @keywords internal
#' @name sea_procs

module_relational_matching <- function (df) {
  return (proc_generic_module(df, FUN = sea_descriptive_statistics))
}
