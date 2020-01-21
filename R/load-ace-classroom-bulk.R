
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
#' @param pulvinar logical. Expect raw data in Pulvinar format? Defaults to \code{TRUE}
#' @return Returns a data.frame containing the content of every file in the
#'  specified \code{path}.

load_ace_bulk <- function(path = ".",
                          verbose = TRUE,
                          recursive = TRUE,
                          exclude = c(),
                          pattern = "",
                          which_modules = "",
                          pulvinar = FALSE) {
  csv = list.files(path = path, pattern = ".csv", recursive = recursive)
  xls = list.files(path = path, pattern = ".xls", recursive = recursive)
  files = sort(c(csv, xls))
  
  # filter_out_vec now accepts a character vector for pattern
  # but DOESN'T accept empty arg
  if (length(exclude) > 0) files = filter_out_vec(files, exclude)
  
  files = filter_vec(files, pattern)
  
  if (length(files) == 0) stop("no matching files", call. = TRUE)
  
  if (path != ".") files = paste(path, files, sep = "/")
  
  files = filter_vec(files, which_modules)
  
  # Use purrr::map to vectorize the load_ace_file call :3
  out = tibble(file = files) %>%
    mutate(data = map(files, function (x) {
      if (verbose) print(x)
      return (load_ace_file(x, pulvinar = pulvinar))
    }))
  
  out <- out %>%
    filter(map(data, ~nrow(.)) > 0) %>% # if extraction failed, data will have 0 rows and other commands on data will fail
    mutate(data = map(data, ~nest(as_tibble(.x), data = -c(bid, module, file)))) %>%
    select(-file) %>%
    unnest(data) %>%
    # new de-duplication strategy: dplyr::distinct() files by bid & module should work (NOT by file)
    # NOTE: old de-duplication was done only on emailed data,
    # but this should behave properly for both emailed and database data
    distinct(bid, module, .keep_all = TRUE) %>%
    nest(data = -module) %>%
    mutate(data = map(data, ~unnest(.x, data)),
           data = rlang::set_names(data, module),
           # Set demos to the side to simulate ACE Explorer
           # Not one separate demos module, but this is how the data get put
           # in proc_by_module so that will have to expect this col in some cases
           demos = map(data, ~.x %>%
                         select(one_of(ALL_POSSIBLE_DEMOS)) %>%
                         distinct()),
           data = map(data, ~.x %>%
                        select(-one_of(ALL_POSSIBLE_DEMOS[!(ALL_POSSIBLE_DEMOS %in% c(COL_BID, COL_BID_SHORT))]))))
  
  # currently returns a tibble where data is NOT rbind.filled together into one big df
  # but kept separate by module
  return(out)
}
