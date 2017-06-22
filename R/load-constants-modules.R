
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

#' @name ace_module
FILTER <- "FILTER"

#' @name ace_module
BACK_SPATIAL_SPAN <- "BACKWARDSSPATIALSPAN"

#' @name ace_module
ISHIHARA <- "ISHIHARA"

#' @name ace_module
SPATIAL_CUE <- "SPATIALCUEING"

#' Identify ACE module from filename
#'
#' Identifies ACE module from the filename
#'
#' @keywords internal
#' @param file a character string containing the module name.
#' @return Returns the name of the ACE module if found.

identify_module <- function(file) {
  file = gsub(" ", "", toupper(file), fixed = TRUE) # must be matched with NO spaces in the module name
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
  } else if (grepl(BACK_SPATIAL_SPAN, file)) {
    return (BACK_SPATIAL_SPAN) # More specific module name must be listed first if using this if else chain
  } else if (grepl(SPATIAL_SPAN, file)) {
    return (SPATIAL_SPAN)
  } else if (grepl(STROOP, file)) {
    return (STROOP)
  } else if (grepl(TASK_SWITCH, file)) {
    return (TASK_SWITCH)
  } else if (grepl(TNT, file)) {
    return (TNT)
  } else if (grepl(FILTER, file)) {
    return (FILTER)
  }  else if (grepl(ISHIHARA, file)) {
    return (ISHIHARA)
  }  else if (grepl(SPATIAL_CUE, file)) {
    return (SPATIAL_CUE)
  }
  return ("unknown")
}