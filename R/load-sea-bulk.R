
#' Read & Load all SEA csv files in a directory
#'
#' Wrapper function around \code{\link{load_sea_file}()} to read & parse 
#'  all SEA csv files in a directory.
#'
#' @export
#' @importFrom dplyr bind_rows distinct filter lag mutate tibble
#' @importFrom purrr map map2 possibly walk2
#' @importFrom tidyr nest unnest
#' 
#' @inheritParams base::list.files
#' @param verbose logical. Print details? Defaults to \code{TRUE}
#' @param recursive logical. Load files in subfolders also? Defaults to \code{TRUE}
#' @param exclude a list of patterns to exclude
#' @param which_modules Specify modules to process. Defaults to all modules.
#' @return Returns a data.frame containing the content of every file in the
#'  specified \code{path}.

load_sea_bulk <- function(path = ".",
                          verbose = TRUE,
                          recursive = TRUE,
                          exclude = c(),
                          pattern = "",
                          which_modules = "") {
  csv = list.files(path = path, pattern = ".csv", recursive = recursive)
  files = sort(csv)
  
  if (length(exclude) > 0) files = filter_out_vec(files, exclude)
  
  files = filter_vec(files, pattern)
  
  if (length(files) == 0) stop("no matching files", call. = TRUE)
  
  if (path != ".") files = paste(path, files, sep = "/")
  
  files = filter_vec(files, which_modules)
  
  
  # If this can be vectorized, why not? I live for speed
  out <- tibble(file = files) %>%
    mutate(data = map(file, possibly(~load_sea_file(.x, verbose = verbose), dplyr::tibble())),
           file = walk2(file, data, function(x, y) {
             if (nrow(y) == 0) warning(paste(x, "failed to load!"))
           })) %>%
    filter(map(data, ~nrow(.)) > 0) %>%
    select(-file) %>% # because it's already pasted inside load_ace_file
    unnest(data) %>%
    nest(-module) %>%
    mutate(data = map(data, ~.x %>%
                        remove_empty_cols() %>%
                        # coarse duplicate rejection
                        # assumes duplicate rows will be the same along at least these few columns
                        # RT should definitely be the same in duplicate rows but not otherwise
                        distinct(pid, question_id, rt, trial_onset, .keep_all = TRUE) %>%
                        # remove this once re-typing functionality has been added. only need while all cols are char
                        replace_nas("") %>%
                        group_by(pid) %>%
                        # TODO: Can you quasi-quote inside of map? I think not
                        mutate(previous_correct_button = lag(correct_button),
                               previous_correct_button = paste0("prev_", previous_correct_button)) %>%
                        ungroup()
                      # TODO: write re-typing master function
                      # re-typing columns must occur here, AFTER data has been separated by module
                      # because individual data files contain data from multiple modules
                      ),
    data = rlang::set_names(data, module))
  
  return(out)
}
