
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
SPATIAL_SPAN <- "SPATIALSPAN"

#' @name ace_module
STROOP <- "STROOP"

#' @name ace_module
TASK_SWITCH <- "TASKSWITCH"

#' @name ace_module
TNT <- "TNT"

#' Identify ACE module from filename
#'
#' Identifies ACE module from the filename
#'
#' @keywords internal
#' @param file a character string containing the module name.
#' @return Returns the name of the ACE module if found.

identify_module <- function(file) {
  file = toupper(file)
  if (grepl(BOXED, file)) {
    return (BOXED)
  } else if (grepl(BRT, file)) {
    return (BRT)
  } else if (grepl(DISCRIMINATION, file)) {
    return (DISCRIMINATION)
  } else if (grepl(FLANKER, file)) {
    return (FLANKER)
  } else if (grepl(SAAT, file)) {
    return (SAAT)
  } else if (grepl(SPATIAL_SPAN, file)) {
    return (SPATIAL_SPAN)
  } else if (grepl(STROOP, file)) {
    return (STROOP)
  } else if (grepl(TASK_SWITCH, file)) {
    return (TASK_SWITCH)
  } else if (grepl(TNT, file)) {
    return (TNT)
  }
  return ("unknown")
}