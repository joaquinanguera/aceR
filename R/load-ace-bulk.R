
#' Read & Load all ACE csv & xls files in a directory
#'
#' Wrapper function around \code{\link{load_ace_file}()} to read & parse 
#'  all ACE csv files in a directory.
#'
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom purrr map map_int
#' @importFrom tidyr nest unnest
#' 
#' @inheritParams base::list.files
#' @param verbose logical. Print details? Defaults to \code{TRUE}
#' @param recursive logical. Load files in subfolders also? Defaults to \code{TRUE}
#' @param exclude a list of patterns to exclude
#' @param which_modules Specify modules to process. Defaults to all modules.
#' @param data_type character What app data export type produced this data? One of
#' \code{c("explorer", "email", "pulvinar")}. Must be specified.
#' @return Returns a data.frame containing the content of every file in the
#'  specified \code{path}.

load_ace_bulk <- function(path = ".",
                          verbose = TRUE,
                          recursive = TRUE,
                          exclude = c(),
                          pattern = "",
                          which_modules = "",
                          data_type = c("explorer", "email", "pulvinar")) {
  stopifnot(length(data_type) == 1)
  
  csv = list.files(path = path, pattern = ".csv", recursive = recursive)
  xls = list.files(path = path, pattern = ".xls", recursive = recursive)
  files = sort(c(csv, xls))
  
  # filter_out_vec now accepts a character vector for pattern
  # but DOESN'T accept empty arg
  if (length(exclude) > 0) files = filter_out_vec(files, exclude)
  
  files = filter_vec(files, pattern)
  
  if (length(files) == 0) stop(crayon::red("no matching files"), call. = TRUE)
  
  if (path != ".") files = paste(path, files, sep = "/")
  
  files = filter_vec(files, which_modules)
  
  # Use purrr::map to vectorize the load_ace_file call :3
  out = tibble(file = files) %>%
    mutate(data = map(files, function (x) {
      if (verbose) cat(crayon::blue("Starting ", x, "\n"), sep = "")
      return (load_ace_file(x, app_type = data_type))
    }))
  
  out <- out %>%
    filter(map(data, ~nrow(.)) > 0) %>% # if extraction failed, data will have 0 rows and other commands on data will fail
    mutate(data = map(data, function(x) {
      # this will create NA-filled version of condition if it doesn't exist (in some modules)
      # We need this so that nest()/distinct() will not throw errors when condition is an invalid col
      if (!(COL_CONDITION %in% names(x))) x[[COL_CONDITION]] = NA
      x = x %>% 
        as_tibble() %>% 
        nest(data = -c(!!COL_BID, !!COL_MODULE, !!COL_CONDITION, !!COL_FILE))
      return (x)
    })) %>%
    select(-!!COL_FILE) %>%
    unnest(data) %>%
    # new de-duplication strategy: dplyr::distinct() files by bid & module should work (NOT by file)
    # NOTE: old de-duplication was done only on emailed data,
    # but this should behave properly for both emailed and database data
    distinct(!!Q_COL_BID, !!Q_COL_MODULE, !!Q_COL_CONDITION, .keep_all = TRUE) %>%
    nest(data = -!!COL_MODULE) %>%
    mutate(data = map(data, ~unnest(.x, data)),
           data = map(data, ~remove_empty_cols(.)),
           data = rlang::set_names(data, !!Q_COL_MODULE))
  
  if (data_type == "email") {
    # Set demos to the side to simulate ACE Explorer
    # Not one separate demos module, but this is how the data get put
    # in proc_by_module so that will have to expect this col in some cases
    out <- out %>%
      mutate(demos = map(data, ~.x %>%
                           select(any_of(ALL_POSSIBLE_DEMOS)) %>%
                           distinct()),
             data = map(data, ~.x %>%
                          select(-any_of(ALL_POSSIBLE_DEMOS[!(ALL_POSSIBLE_DEMOS %in% c(COL_BID, COL_BID_SHORT))]))))
  }
  
  # currently returns a tibble where data is NOT rbind.filled together into one big df
  # but kept separate by module
  if (verbose) cat(crayon::green("Finished! See possible warnings below."), sep = "\n")
  return(out)
}
