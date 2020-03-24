
#' aceR package path
#'
#' @export
#' @return Returns the root directory of the aceR package.

aceR_lib_path <- function() {
  base_path = system.file(package = "aceR")
  return (base_path)
}

#' aceR sample data path
#'
#' @export
#' @param subfolder which subfolder (and task type) do you want to test?
#' One of \code{c("email", "pulvinar","explorer", "sea")}. Must be specified.
#' @return Returns the path containing sample aceR data for the
#' desired subfolder

aceR_sample_data_path <- function(subfolder) {
  path = paste(aceR_lib_path(), "extdata", subfolder, sep = "/")
  return (path)
}

#' aceR description
#'
#' @export
#' @return Returns the contents of the aceR DESCRIPTION file

aceR_description <- function () {
  return (utils::packageDescription("aceR"))
}

#' aceR version
#'
#' @export
#' @return Returns the version of aceR installed on the user's machine

aceR_version <- function() {
  version = utils::packageVersion("aceR")
  return (as.character(version))
}

#' aceR methods
#'
#' @export
#' @return Returns a list of available aceR methods

aceR_methods <- function () {
  return (utils::lsf.str("package:aceR"))
}