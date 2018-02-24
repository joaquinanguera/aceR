
#' SEA module constants
#'
#' @keywords internal
#' @name sea_module
NULL

#' @name sea_module


#' @name sea_module
ALL_SEA_MODULES = c()

#' @keywords internal
#' @importFrom dplyr recode

label_sea_module_conditions <- function (dat) {
  dat[[COL_CONDITION]] <- dplyr::recode(dat[[COL_MODULE]],
                                        "Fractions Lvl 1" = "lvl_1",
                                        "Fractions Lvl 2" = "lvl_2",
                                        "Fractions Lvl 3" = "lvl_3",
                                        .default = "")
  dat[[COL_MODULE]] <- ifelse(startsWith(dat[[COL_MODULE]], "Fractions"), "Fractions", dat[[COL_MODULE]])
  dat[[COL_MODULE]] <- ifelse(startsWith(dat[[COL_MODULE]], "Reading Fluency"), "Reading Fluency", dat[[COL_MODULE]])
  return (dat)
}