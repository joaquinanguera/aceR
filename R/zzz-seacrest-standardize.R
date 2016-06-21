
#' Standardize Seacrest PIDs
#'
#' Standardizes Seacrest PIDs.
#'
#' @export
#' @param x A character vector containing raw seacrest pids

standardize_seacrest_pid = function(x) {
  standardized = sapply(x, function(y) { 
    id_split = stringr::str_split(y, "-")[[1]]
    id = id_split[3]
    if (nchar(id) == 1) {
      id = paste0("00", id)
    } else if (nchar(id) == 2) {
      id = paste0("0", id)
    }
    return (paste0("ADMIN-UCSF-", gsub("b", "", id)))
  })
  return (as.vector(standardized))
}