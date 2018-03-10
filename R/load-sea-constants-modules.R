
#' SEA module constants
#'
#' @keywords internal
#' @name sea_module
NULL

#' @name sea_module
MATH_FLU <- "MATH_FLUENCY"

#' @name sea_module
READ_FLU <- "READING_FLUENCY"

#' @name sea_module
FRAC_1 <- "FRACTIONS_LVL_1"

#' @name sea_module
FRAC_2 <- "FRACTIONS_LVL_2"

#' @name sea_module
FRAC_3 <- "FRACTIONS_LVL_3"

#' @name sea_module
MATH_REC <- "MATH_RECALL"

#' @name sea_module
RUN_MEM_SPAN <- "RUNNING_MEMORY_SPAN"

#' @name sea_module
GROUPITIZE <- "GROUPITIZING"

#' @name sea_module
REL_MATCH_1 <- "RELATIONAL_MATCHING_ONE"

#' @name sea_module
REL_MATCH_2 <- "RELATIONAL_MATCHING_TWO"

#' @name sea_module
ARITHM_VER <- "ARITHMETIC_VERIFICATION"

#' @name sea_module
READ_COMP <- "READING_COMPREHENSION"

#' @name sea_module
ALL_SEA_MODULES = c(MATH_FLU,
                    READ_FLU,
                    FRAC_1,
                    FRAC_2,
                    FRAC_3,
                    MATH_REC,
                    RUN_MEM_SPAN,
                    GROUPITIZE,
                    REL_MATCH_1,
                    REL_MATCH_2,
                    ARITHM_VER,
                    READ_COMP)

#' @keywords internal
#' @importFrom dplyr mutate

standardize_sea_module_names <- function(df) {
  df = dplyr::mutate(df,
                     module = toupper(replace_spaces(.data$module, replacement = "_")))
  return (df)
}

#' @keywords internal
#' @importFrom dplyr recode

label_sea_module_conditions <- function (dat) {
  dat[[COL_CONDITION]] <- dplyr::recode(dat[[COL_MODULE]],
                                        "Fractions Lvl 1" = "lvl_1",
                                        "Fractions Lvl 2" = "lvl_2",
                                        "Fractions Lvl 3" = "lvl_3",
                                        .default = "")
  dat[[COL_MODULE]] <- ifelse(startsWith(dat[[COL_MODULE]], "Reading Fluency"), "Reading Fluency", dat[[COL_MODULE]])
  return (dat)
}