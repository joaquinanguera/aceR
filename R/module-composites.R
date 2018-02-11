
#' Get composite ACE scores across module
#' 
#' Function to summarize individual data across \emph{multiple modules}.
#' 
#' @export
#' @importFrom dplyr mutate
#' @param x a \code{\link{data.frame}} containing ACE summary statistics, created
#' using \code{\link{proc_by_module}} with \code{output = "wide"}. NOTE: If processing
#' data containing the Filter module, this IS compatible with the \code{\link{list}}
#' of length 2, where the Filter data is separate, that is output by
#' \code{proc_by_module(output = "wide")}. NOT compatible with data from \code{output = "list"}.
#' 
#' 
#' @return Returns composite percentile scores for all modules present in data
#'  as a dataframe. 
#'  
#' @section Composite makeups:
#' Composites are calculated by the following process:
#' 1. For each module, calculate percentile rank for each individual subject
#' in that module's dataset
#' 2. For each pre-specified composite group of module performance metrics,
#' calculate average percentile rank across each metric in the composite group.
#' \emph{Note:} Percentile ranks within module are calculated for all subjects
#' with valid data in that module, but composite rank scores between module are
#' calculated only for subjects who have valid data for every module required 
#' to calculate that composite score.
#' 
#' Working memory \code{wm} component metrics:
#' Forward and backward spatial span count, filtering capacity (k)
#' for 0-distractor trials only
#' Filtering \code{filtering} component metrics:
#' Filtering k for 0-distractor trials, Flanker cost of incongruence on RT,
#' Stroop cost of incongruence on RT
#' Focus \code{focus} component metrics:
#' Boxed cost of conjunction on RT, SAAT sustained condition RT
#' Impulsivity \code{impulsivity} component metrics:
#' SAAT impulsive condition RT
#' Goal management \code{goal_mgmt} component metrics:
#' Tap-and-trace cost of dual-task on RT, task-switch cost of switching on RT

get_composites <- function(x) {
  
  composites = get_all_percentiles(df)
  
  composites = mutate(composites,
                      wm = (SPATIALSPAN.object_count_span.overall.percentile + BACKWARDSSPATIALSPAN.object_count_span.overall.percentile + FILTER.k.2.percentile) / 3,
                      filtering = (FILTER.k.2.percentile + FLANKER.rt_mean.cost.percentile + STROOP.rt_mean.cost.percentile) / 3,
                      focus = (BOXED.score.percentile + SAAT.rt_mean.sustained.percentile) / 2,
                      impulsivity = SAAT.rt_mean.impulsive.percentile,
                      goal_mgmt = (TNT.rt_mean.cost.percentile + TASKSWITCH.rt_mean.cost.percentile) /2)
  
  return(composites)
}

#' Get ALL percentiles for available ACE metrics
#' 
#' Function to return percentile ranks for ACE metrics from \emph{multiple modules}.
#' 
#' @export
#' @import dplyr
#' @param data Processed ACE data. Expects "wide" output ACE data from proc_by_module()
#' @param norm_dist which data should the percentiles be normed to? one of the following
#' \code{"self"}: no norming distribution, percentiles based on data being fed in
#' \code{"ace_t1"}: based on results from ACE Fall 2016 data
#' \code{"ace_t3"}: based on results from ACE Fall 2017 data
#' @param norm_dist_grade if specifying an ACE norming distribution, which grade's norms?
#' Grades in T1: "third", "fifth", "seventh".
#' Grades in T3: "fourth", "sixth", "eighth". Specify as ONE OF THESE STRINGS!
#' @return a df with PID, valid demographics, and percentile rank for the specified norming distribution
#' see \code{get_percentile()} for info on norming distributions

get_all_percentiles <- function(data, norm_dist = "self", norm_dist_grade = NULL) {
  # Basically this just assumes that they have all the tasks necessary.
  # TODO: Throw warning if some tasks are not there for any Ss
  
  # get into one df form
  if (!is.data.frame(data) & FILTER %in% names(data)) {
  df = full_join(data$ALL_OTHER_DATA,
                 filter(data$FILTER, FILTER.distractors == 0)) %>%
    distinct()
  } else if (is.data.frame(data)) {
    df = data
  }
  
  valid_modules = ALL_MODULES[sapply(ALL_MODULES, FUN = function(pattern, x) any(grepl(pattern, x)), x = names(df))]
  valid_modules = valid_modules[!(valid_modules %in% c("BRT", "ISHIHARA"))]
  ptile_vars = list(BACKWARDSSPATIALSPAN = "object_count_span.overall",
                    SPATIALSPAN = "object_count_span.overall",
                    FILTER = "k.2",
                    FLANKER = "rt_mean.cost",
                    STROOP = "rt_mean.cost",
                    BOXED = "score",
                    SAAT = c("rt_mean.sustained", "rt_mean.impulsive"),
                    TNT = "rt_mean.cost",
                    TASKSWITCH = "rt_mean.cost")
  # Flag to indicate whether larger magnitude of var reflects better performance (FALSE)
  # or whether "reversed", aka smaller magnitude of var reflects better performance (TRUE)
  ptile_var_reverse =  list(BACKWARDSSPATIALSPAN = FALSE,
                            SPATIALSPAN = FALSE,
                            FILTER = FALSE,
                            FLANKER = TRUE,
                            STROOP = TRUE,
                            BOXED = TRUE,
                            SAAT = TRUE,
                            TNT = TRUE,
                            TASKSWITCH = TRUE)
  
  demos = select_(df, .dots = get_valid_demos(df))
  percentiles = vector("list", 0)
  for (mod in valid_modules) {
    this_ptile = vector("list", 0)
    for (ptile_var in ptile_vars[[mod]]) {
      this_ptile[[ptile_var]] = get_percentile(df, module = mod, var = ptile_var,
                                               reverse = ptile_var_reverse[[mod]],
                                               norm_dist = norm_dist, norm_dist_grade = norm_dist_grade)
    }
    if (length(this_ptile) == 2) {
      this_ptile = full_join(this_ptile[[1]], this_ptile[[2]])
    } else if (length(this_ptile) == 1) {
      this_ptile = this_ptile[[1]]
    }
    percentiles[[mod]] = this_ptile
  }
  # use full_join() recursively? not great, but perhaps safer than calling bind_cols()
  all_ptiles = percentiles[[1]]
  for (i in 2:length(percentiles)) {
    if (length(percentiles) == 1) break
    all_ptiles = full_join(all_ptiles, percentiles[[i]])
  }
  
  all_ptiles = distinct(all_ptiles)
  
  return (right_join(demos, all_ptiles, by = COL_PID))
}

#' Get percentile for ONE ACE metric
#' 
#' Function to calculate percentile ranks for summarized data from ONE ACE metric.
#' 
#' @keywords internal
#' @param module user-specified string to be appended in the output colname
#' @param id_var name of subject identifier col in df
#' Note that this function does NOT allow a grouping variable; please only feed
#' data from ONE GROUP AT A TIME into this function. It's better for norming assumptions.
#' @param var name of data col in df. specified as a string
#' @param reverse should percentile scores be calculated in reverse order
#' (smaller number = better rank)? Defaults to \code{FALSE}.
#' @param norm_dist which data should the percentiles be normed to? one of the following
#' \code{"self"}: no norming distribution, percentiles based on data being fed in
#' \code{"ace_t1"}: based on results from ACE Fall 2016 data
#' \code{"ace_t3"}: based on results from ACE Fall 2017 data
#' @param norm_dist_grade if specifying an ACE norming distribution, which grade's norms?
#' Grades in T1: "third", "fifth", "seventh".
#' Grades in T3: "fourth", "sixth", "eighth". Specify as ONE OF THESE STRINGS!
#' Will throw error if grade not specified properly
#' @return df containing \code{id_var}, \code{group_var} (if applicable), \code{var}, and percentile rank

get_percentile <- function(df, module, id_var = "pid", var, reverse = FALSE, norm_dist = "self", norm_dist_grade = NULL) {
  # TODO: Add ability to get percentile based on an externally specified norming distribution
  df = df[!(duplicated(df$pid) | duplicated(df$pid, fromLast = T)), ] # scrubbing ALL datasets where there is more than one record per PID (keeping NEITHER) since can't be sure which is right
  var = paste(module, var, sep = ".")
  df = select_(df, id_var, var)
  
  if (norm_dist == "self") {
    
    if (!reverse) { # flipping the logical here bc arrange() defaults to ascending order
      df = arrange_(df, paste0("desc(", var, ")"))
    } else {
      df = arrange_(df, var)
    }
    
    df = df %>%
      filter_(paste0("!is.na(", var, ")")) %>% # need to rank without NAs
      mutate(inv_rank = c(1:n()) - 1, # so that the worst one gets a rank of 0
             percentile = (inv_rank / n()) * 100) %>%
      select(-inv_rank)
    
    # Returns a dataframe with: id_var, var, percentile
    
  } else {
    if (norm_dist == "ace_t1") {
      stopifnot(norm_dist_grade %in% c("third", "fifth", "seventh"))
      these_norms = ace_t1_norms
    } else if (norm_dist == "ace_t3") {
      stopifnot(norm_dist_grade %in% c("fourth", "sixth", "eighth"))
      these_norms = ace_t3_norms
    }
    
    these_norms = these_norms[[norm_dist_grade]]
    df$percentile = get_norm_rank(df[, var], norms = these_norms, var = var, reverse = reverse)
    df$percentile = ifelse(is.infinite(df$percentile), NA, df$percentile)
  }
  
  names(df)[length(df)] = paste0(var, ".percentile")
  return (df)
}

#' @keywords internal
#' go into the norming percentiles and get the closest one. if between two, go down

get_norm_rank <- function(x, norms, var, reverse) {
  if (!reverse) {
    result = sapply(x, function (y) {
      max(which(norms[, var] < y)) - 1
    })
  } else {
    result = sapply(x, function (y) {
      max(which(norms[, var] > y)) - 1
    })
  }
  return (result)
}