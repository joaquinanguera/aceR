
#' @keywords internal
#' @name sea_procs

attempt_module <- function(df, module, verbose) {
  out <- tryCatch({
    df <- do.call(paste0("module_", tolower(module)), list(df = df))
    if (verbose) cat("Processed ", module, "\n")
    return(df)
  }, error = function (e) {
    warning("Unable to process ", module)
    return (data.frame())
  })
}

#' @keywords internal
#' @name sea_procs
#' @importFrom purrr map map2 reduce

module_math_fluency <- function(df) {
  out <- tibble(condition = c(COL_CONDITION, "operation_type", "answer_size"),
                cost_args = list(c("\\.stay", "\\.switch", "\\.switch_cost"),
                                 c("\\.addition", "\\.subtraction", "\\.operation_cost"),
                                 c("\\.1", "\\.2", "\\.answer_size_cost"))) %>%
    mutate(gen = map(condition, function(x) proc_generic_module(df, COL_CORRECT_BUTTON, x, FUN = sea_descriptive_statistics)),
           cost = map2(gen, cost_args, ~multi_subtract(.x, .y[1], .y[2], .y[3])),
           both = map2(gen, cost, ~dplyr::bind_cols(.x, .y)),
           names = map(both, ~names(.x)))
  
  duplicate_cols <- unique(unlist(out$names)[duplicated(unlist(out$names))])
  return (purrr::reduce(out$both, full_join, by = duplicate_cols))
}

#' @keywords internal
#' @name sea_procs
#' @importFrom purrr map map2 reduce

module_math_recall <- function(df) {
  out <- tibble(condition = c("carrying", "math_recall_orientation", "operation_type", "answer_size"),
                cost_args = list(c("\\.no_carry", "\\.carry", "\\.carry_cost"),
                                 c("\\.horizontal", "\\.vertical", "\\.orientation_cost"),
                                 c("\\.addition", "\\.subtraction", "\\.operation_cost"),
                                 c("\\.1", "\\.2", "\\.answer_size_cost"))) %>%
    mutate(gen = map(condition, function(x) proc_generic_module(df, COL_CORRECT_BUTTON, x, FUN = sea_descriptive_statistics)),
           cost = map2(gen, cost_args, ~multi_subtract(.x, .y[1], .y[2], .y[3])),
           both = map2(gen, cost, ~dplyr::bind_cols(.x, .y)),
           names = map(both, ~names(.x)))
  
  duplicate_cols <- unique(unlist(out$names)[duplicated(unlist(out$names))])
  return (purrr::reduce(out$both, full_join, by = duplicate_cols))
}

#' @keywords internal
#' @name sea_procs

module_reading_fluency <- function(df) {
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, FUN = sea_reading_descriptive_statistics)
  gen = dplyr::select(gen, -dplyr::contains("correct_button_mean"))
  time = proc_by_condition(df, "feedback_onset", include_overall = F, FUN = sea_task_duration)
  return (left_join(gen, time, by = COL_BID))
}

#' @keywords internal
#' @name sea_procs

module_reading_comprehension <- function(df) {
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, FUN = sea_reading_descriptive_statistics)
  gen = dplyr::select(gen, -dplyr::contains("correct_button_mean"))
  time = proc_by_condition(df, "feedback_onset", include_overall = F, FUN = sea_task_duration)
  return (dplyr::left_join(gen, time, by = COL_BID))
}

#' @keywords internal
#' @name sea_procs

module_fractions_lvl_1 <- function(df) {
  # just % accuracy and RT like normal
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, "num_size", FUN = sea_descriptive_statistics)
  cost = multi_subtract(gen, "\\.small", "\\.large", "\\.cost")
  return (dplyr::bind_cols(gen, cost))
}

#' @keywords internal
#' @name sea_procs

module_fractions_lvl_2 <- function(df) {
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, "matched_value", FUN = sea_descriptive_statistics)
  cost = multi_subtract(gen, "\\.denom_matched", "\\.num_matched", "\\.cost")
  return (dplyr::bind_cols(gen, cost))
}

#' @keywords internal
#' @name sea_procs
#' @importFrom purrr map map2 reduce

module_fractions_lvl_3 <- function(df) {
  # by left vs right and by numerator larger but not crossed
  out <- tibble(condition = c(COL_CONDITION, "num_larger"),
                cost_args = list(c("\\.left", "\\.right", "\\.cost"),
                                 c("\\.num_larger", "\\.denom_larger", "\\.num_larger_cost"))) %>%
    mutate(gen = map(condition, function(x) proc_generic_module(df, COL_CORRECT_BUTTON, x, FUN = sea_descriptive_statistics)),
           cost = map2(gen, cost_args, ~multi_subtract(.x, .y[1], .y[2], .y[3])),
           both = map2(gen, cost, ~dplyr::bind_cols(.x, .y)),
           names = map(both, ~names(.x)))
  
  duplicate_cols <- unique(unlist(out$names)[duplicated(unlist(out$names))])
  return (purrr::reduce(out$both, full_join, by = duplicate_cols))
}

#' @keywords internal
#' @name sea_procs
#' @importFrom purrr map map2 reduce

module_arithmetic_verification <- function(df) {
  
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, "block_type", FUN = sea_descriptive_statistics)
  gen_mixed = proc_generic_module(filter(df, block_type == "Mixed"), COL_CORRECT_BUTTON, COL_CONDITION, FUN = sea_descriptive_statistics) %>%
    select(-ends_with(".overall"), -ends_with("_half"), -ends_with("correct"))
  gen_mixed_full = proc_generic_module(filter(df, block_type == "Mixed"), COL_CORRECT_BUTTON, "switch_by_operation_type", FUN = sea_descriptive_statistics) %>%
    select(-ends_with(".overall"), -ends_with("_half"), -ends_with("correct"))
  
  gens = purrr::reduce(list(gen, gen_mixed, gen_mixed_full), full_join, by = "bid")
  
  cost = multi_subtract(gen, "\\.fixed", "\\.mixed", "\\.cost")
  cost_mixed = multi_subtract(gen_mixed, "\\.stay", "\\.switch", "\\.cost")
  cost_mixed_multiplication = multi_subtract(gen_mixed_full, "\\.stay_multiplication", "\\.switch_multiplication", "\\.cost_multiplication")
  cost_mixed_addition = multi_subtract(gen_mixed_full, "\\.stay_addition", "\\.switch_addition", "\\.cost_addition")
  
  return (dplyr::bind_cols(list(gens,
                                cost,
                                cost_mixed,
                                cost_mixed_multiplication,
                                cost_mixed_addition)))
}

#' @keywords internal
#' @name sea_procs

module_groupitizing <- function(df) {
  # calculate cost PAIRWISE (3 group number conditions)
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, "number_groups", FUN = sea_descriptive_statistics)
  cost_2_1 = multi_subtract(gen, "\\.1", "\\.2", "\\.2_1_cost")
  cost_3_1 = multi_subtract(gen, "\\.1", "\\.3", "\\.3_1_cost")
  cost_3_2 = multi_subtract(gen, "\\.2", "\\.3", "\\.3_2_cost")
  return (dplyr::bind_cols(gen, cost_2_1, cost_3_1, cost_3_2))
}

#' @keywords internal
#' @name sea_procs

module_running_memory_span <- function(df) {
  # this is two subtasks; letter and spatial
  # DO NOT REPORT .overall
  gen_strict = proc_generic_module(df, "correct_button_strict", "block_type", FUN = sea_descriptive_statistics)
  gen_loose = proc_generic_module(df, "correct_button_loose", "block_type", FUN = sea_descriptive_statistics)
  return (dplyr::full_join(gen_strict, gen_loose, by = "bid", suffix = c(".strict", ".loose")) %>% dplyr::select(-dplyr::contains(".overall"), -dplyr::starts_with("rt_")))
}

#' @keywords internal
#' @name sea_procs

module_relational_matching <- function (df) {
  return (proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION, FUN = sea_descriptive_statistics))
}