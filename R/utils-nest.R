
#' Unnest loaded ACE data into long form
#' 
#' Function to unnest data loaded with \code{\link{load_ace_bulk}()}
#' into long form.
#' 
#' @export
#' @importFrom dplyr ends_with everything one_of mutate n rename select vars
#' @importFrom magrittr %>%
#' @importFrom purrr map2
#' @importFrom rlang !!
#' @importFrom tidyr unnest
#' 
#' @param df a nested \code{\link{data.frame}} containing formatted
#' trialwise ACE data. 
#'
#' This includes data loaded with the following methods: 
#' \enumerate{
#'   \item \code{\link{load_ace_file}}
#'   \item \code{\link{load_ace_bulk}}
#' }
#' @param app_type character. What app type produced this data? One of
#' \code{c("classroom", "explorer")}. Must be specified.
#' @return Returns a data.frame with no list-columns, with one row per trial,
#' and one section of rows per module. Columns only containing valid data
#' for one or a subset of modules are all set to \code{NA} for modules to which
#' those columns do not apply.

unnest_ace_raw <- function(df, app_type = c("classroom", "explorer")) {
  stopifnot(length(app_type) == 1)
  
  if (app_type == "classroom") {
    stopifnot(all(names(df) == c(COL_MODULE, "data", "demos")))
    
    out <- df %>%
      mutate(full = map2(data, demos, dplyr::full_join, by = COL_BID))
    
  } else if (app_type == "explorer") {
    stopifnot(all(names(df) == c(COL_MODULE, "data")))
    
    out <- df %>%
      # Put demos in another column, wide-ish, so it's next to every other module
      mutate(demos = map(1:n(), ~df$data[df$module == DEMOS][[1]])) %>%
      filter(module != DEMOS) %>%
      mutate(data = map(data, ~reconstruct_pid(.x)),
             demos = map(demos,
                         ~rename_at(.x,
                                    vars(one_of(c(COL_BID, COL_TIME, COL_FILE))),
                                    ~paste0(., "_demos"))),
             full = map2(data, demos, dplyr::full_join, by = COL_PID))
  }
  
  out <- out %>%
    select(!!COL_MODULE, full) %>%
    unnest(full) %>%
    select(!!COL_MODULE, one_of(ALL_POSSIBLE_DEMOS), ends_with("_demos"), everything())
  
  return (out)
}


#' Re-nest unnested loaded ACE data
#' 
#' Function to re-nest data loaded with \code{\link{load_ace_bulk}()} and
#' unnested with \code{link{unnest_ace_raw}()} into a form suitable
#' for processing in other package functions.
#' 
#' @export
#' @importFrom dplyr arrange bind_rows ends_with filter one_of mutate pull rename_at select vars
#' @importFrom magrittr %>%
#' @importFrom purrr map map_chr pluck
#' @importFrom rlang !! type_of
#' @importFrom tibble tibble
#' @importFrom tidyr nest
#' 
#' @param df a nested \code{\link{data.frame}} containing formatted
#' trialwise ACE data.
#'
#' This includes data loaded with the following methods: 
#' \enumerate{
#'   \item \code{\link{load_ace_file}}
#'   \item \code{\link{load_ace_bulk}}
#' }
#' @param app_type character. What app type produced this data? One of
#' \code{c("classroom", "explorer")}. Must be specified.
#' @return Returns a data.frame in the nested format output by
#' \code{\link{load_ace_bulk}()} with the argument\code{app_type} set
#' to \code{"email"} or \code{"pulvinar"} for ACE Classroom data. Suitable
#' for further processing with, for example, \code{link{proc_by_module}()}.

nest_ace_raw <- function(df, app_type = c("classroom", "explorer")) {
  
  stopifnot(all(map_chr(df, rlang::type_of) != "list"))
  
  explorer_demos <- ALL_POSSIBLE_DEMOS
  explorer_demos[explorer_demos %in% c(COL_BID, COL_TIME, COL_FILE)] <- paste0(explorer_demos[explorer_demos %in% c(COL_BID, COL_TIME, COL_FILE)], "_demos")
  non_bid_demos <- ALL_POSSIBLE_DEMOS[ALL_POSSIBLE_DEMOS != COL_BID]
  non_id_demos <- ALL_POSSIBLE_DEMOS[!(ALL_POSSIBLE_DEMOS %in% c(COL_BID, COL_PID, COL_TIME, COL_FILE))]
  
  if (app_type == "classroom") {
    these_demos <- ALL_POSSIBLE_DEMOS
    these_non_id_demos <- non_bid_demos
  } else if (app_type == "explorer") {
    stopifnot(paste0(COL_BID, "_demos") %in% names(df))
    
    these_demos <- explorer_demos
    these_non_id_demos <- non_id_demos
  }
  
  out <- df %>%
    nest(data = -!!Q_COL_MODULE) %>%
    mutate(data = map(data, remove_empty_cols),
           demos = map(data, ~select(.x, one_of(these_demos))),
           data = map(data, ~select(.x, -one_of(these_non_id_demos), -ends_with("_demos"))),
           demos = map(demos, dplyr::distinct))
  
  if (app_type == "explorer") {
    temp_demos = out %>%
      pull(demos) %>%
      pluck(1) %>%
      rename_at(vars(one_of(explorer_demos[endsWith(explorer_demos, "_demos")])),
                str_sub, end = -7L) %>%
      arrange(!!Q_COL_BID) %>% 
      list() %>%
      tibble(module = DEMOS, data = .)
    
    out <- out %>%
      select(-demos) %>%
      mutate(data = map(data, ~filter(.x, !is.na(!!Q_COL_BID)))) %>%
      bind_rows(temp_demos)
  } else if (app_type == "classroom") {
    out <- out %>%
      mutate(demos = rlang::set_names(demos, !!Q_COL_MODULE))
  }
  
  out <- out %>%
    mutate(data = rlang::set_names(data, !!Q_COL_MODULE))
  
  return (out)
}
