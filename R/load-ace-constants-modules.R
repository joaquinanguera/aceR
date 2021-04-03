
#' ACE module constants
#'
#' @keywords internal
#' @name ace_module
NULL

#' @name ace_module
BOXED <- "BOXED"

#' @name ace_module
BRT <- "BRT"

#' @name ace_module
DISCRIMINATION <- "DISCRIMINATION"

#' @name ace_module
FLANKER <- "FLANKER"

#' @name ace_module
SAAT <- "SAAT"

#' @name ace_module
SAAT_SUS <- "SAATSUSTAINED"

#' @name ace_module
SAAT_IMP <- "SAATIMPULSIVE"

#' @name ace_module
SPATIAL_SPAN <- "SPATIALSPAN"

#' @name ace_module
STROOP <- "STROOP"

#' @name ace_module
TASK_SWITCH <- "TASKSWITCH"

#' @name ace_module
TNT <- "TNT"

#' @name ace_module
FILTER <- "FILTER"

#' @name ace_module
BACK_SPATIAL_SPAN <- "BACKWARDSSPATIALSPAN"

#' @name ace_module
ISHIHARA <- "ISHIHARA"

#' @name ace_module
SPATIAL_CUE <- "SPATIALCUEING"

#' @name ace_module
DEMOS <- "DEMOGRAPHICS"

#' @name ace_module
ALL_MODULES = c(BOXED,
                BRT,
                DISCRIMINATION,
                FLANKER,
                SAAT,
                SPATIAL_SPAN,
                STROOP,
                TASK_SWITCH,
                TNT,
                FILTER,
                BACK_SPATIAL_SPAN,
                ISHIHARA,
                SPATIAL_CUE,
                DEMOS)

#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @keywords internal

module_split_saat <- function (df) {
  if (SAAT %in% df$module) {
    df$module <- paste0(df$module, toupper(df[[COL_CONDITION]]))
  }
  return (df)
}

#' Identify ACE module from filename
#'
#' Identifies ACE module from the filename
#'
#' @importFrom dplyr across as_tibble if_else mutate select summarize
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom tidyselect everything
#' 
#' @keywords internal
#' @param file a character string containing the module name.
#' @return Returns the name of the ACE module if found.

identify_module <- function(file) {
  file = gsub(" ", "", toupper(file), fixed = TRUE) # must be matched with NO spaces in the module name
  match = map(ALL_MODULES, ~grepl(., file)) %>%
    rlang::set_names(ALL_MODULES) %>%
    as_tibble() %>%
    # separates backwards spatial span bc spatial span also grepl = TRUE
    mutate(SPATIALSPAN = if_else(BACKWARDSSPATIALSPAN, FALSE, SPATIALSPAN),
           unknown = if_else(rowSums(.) == 0, TRUE, FALSE)) %>%
    # each COLUMN now one file, allows easier computation
    t() %>%
    # Need to give a function for .name_repair to stop auto-messaging
    as_tibble(.name_repair = ~make.names(., unique = TRUE)) %>%
    summarize(across(everything(), which)) %>%
    as.vector(mode = "integer")
  
  return (c(ALL_MODULES, "unknown")[match])
}
