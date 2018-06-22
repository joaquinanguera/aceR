
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

module_math_fluency <- function(df) {
  gen_stay_switch = proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION, FUN = sea_descriptive_statistics)
  gen_operation_type = proc_generic_module(df, COL_CORRECT_BUTTON, "operation_type", FUN = sea_descriptive_statistics)
  gen_answer_size = proc_generic_module(df, COL_CORRECT_BUTTON, "answer_size", FUN = sea_descriptive_statistics)
  
  cost_stay_switch = multi_subtract(gen_stay_switch, "\\.stay", "\\.switch", "\\.switch_cost")
  cost_operation_type = multi_subtract(gen_operation_type, "\\.addition", "\\.subtraction", "\\.operation_cost")
  cost_answer_size = multi_subtract(gen_answer_size, "\\.1", "\\.2", "\\.answer_size_cost")
  
  return (dplyr::bind_cols(gen_stay_switch,
                           cost_stay_switch,
                           gen_operation_type,
                           cost_operation_type,
                           gen_answer_size,
                           cost_answer_size))
}

#' @keywords internal
#' @name sea_procs

module_math_recall <- function(df) {
  gen_carry = proc_generic_module(df, COL_CORRECT_BUTTON, "carrying", FUN = sea_descriptive_statistics)
  gen_orientation = proc_generic_module(df, COL_CORRECT_BUTTON, "math_recall_orientation", FUN = sea_descriptive_statistics)
  gen_operation_type = proc_generic_module(df, COL_CORRECT_BUTTON, "operation_type", FUN = sea_descriptive_statistics)
  gen_answer_size = proc_generic_module(df, COL_CORRECT_BUTTON, "answer_size", FUN = sea_descriptive_statistics)
  
  cost_carry = multi_subtract(gen_stay_switch, "\\.no_carry", "\\.carry", "\\.carry_cost")
  cost_orientation = multi_subtract(gen_orientation, "\\.horizontal", "\\.vertical", "\\.orientation_cost")
  cost_operation_type = multi_subtract(gen_operation_type, "\\.addition", "\\.subtraction", "\\.operation_cost")
  cost_answer_size = multi_subtract(gen_answer_size, "\\.1", "\\.2", "\\.answer_size_cost")
  
  return (dplyr::bind_cols(gen_carry,
                           cost_carry,
                           gen_orientation,
                           cost_orientation,
                           gen_operation_type,
                           cost_operation_type,
                           gen_answer_size,
                           cost_answer_size))
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

module_fractions_lvl_3 <- function(df) {
  # by left vs right and by numerator larger but not crossed
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION, FUN = sea_descriptive_statistics)
  gen_num_larger = proc_generic_module(df, COL_CORRECT_BUTTON, "num_larger", FUN = sea_descriptive_statistics)
  cost = multi_subtract(gen, "\\.left", "\\.right", "\\.cost")
  cost_num_larger = multi_subtract(gen, "\\.num_larger", "\\.denom_larger", "\\.num_larger_cost")
  return (dplyr::bind_cols(gen,
                           cost,
                           gen_num_larger,
                           cost_num_larger))
}

#' @keywords internal
#' @name sea_procs

module_arithmetic_verification <- function(df) {
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, "block_type", FUN = sea_descriptive_statistics)
  gen_mixed = proc_generic_module(filter(df, block_type == "Mixed"), COL_CORRECT_BUTTON, COL_CONDITION, FUN = sea_descriptive_statistics)
  gen_mixed_full = proc_generic_module(filter(df, block_type == "Mixed"), COL_CORRECT_BUTTON, "switch_by_operation_type", FUN = sea_descriptive_statistics)
  cost = multi_subtract(gen, "\\.fixed", "\\.mixed", "\\.cost")
  cost_mixed = multi_subtract(gen_mixed, "\\.stay", "\\.switch", "\\.cost")
  cost_mixed_multiplication = multi_subtract(gen_mixed_full, "\\.stay_multiplication", "\\.switch_multiplication", "\\.cost_multiplication")
  cost_mixed_addition = multi_subtract(gen_mixed_full, "\\.stay_addition", "\\.switch_addition", "\\.cost_addition")
  return (dplyr::bind_cols(gen,
                           cost,
                           gen_mixed,
                           cost_mixed,
                           gen_mixed_full,
                           cost_mixed_multiplication,
                           cost_mixed_addition))
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