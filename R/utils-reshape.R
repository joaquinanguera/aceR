
#' @keywords internal
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang exprs, enquo, UQ, UQS
#' @importFrom tidyr gather, unite, spread
#' 
#' Spread key-values pairs across multiple columns.
#' 
#' Spread key-values pairs, where one key maps analogously onto multiple values,
#' into across multiple columns. Equivalent to calling \code{\link[tidyr]{spread}} separately
#' for dataframes with the same key and different values,
#' and joining them column-wise back into one dataframe.
#' 
#' @param data A data frame.
#' @param key Column names, UNQUOTED, passed in using \code{\link[tidyselect]{vars_select}}.
#' @param ... A selection of columns containing values to be spread.
#' again, unquoted. Accepts \code{\link[dplyr]{select_helpers}}.
#' @param name_order Which identifier comes first in final colname?
#' Choose \code{"key_first"} or \code{"value_first"}. Defaults to \code{"key_first"}.
#' @param sep Separator to use between values, ultimately ending up in colnames.
#' Passed to \code{\link[tidyr]{unite}}.
#' @inheritParams spread `fill, convert, drop` all pass into \code{\link[tidyr]{spread}}.
#' @return A data frame, "fully" spread by all indicated columns.
#' 
#' @examples
#' data <- expand.grid(
#' id = 1:10,
#' condition = c("a", "b")) %>%
#' mutate(value_1 = rnorm(n()),
#' value_2 = rnorm(n()))
#' 
#' data %>% super_spread(condition, value1:value2)
super_spread <- function (data, key, ..., name_order = "key_first", sep = "_", fill = NA, convert = FALSE, drop = TRUE) {
  dots <- exprs(...)
  key <- enquo(key)
  output <- data %>%
    gather(gkey, value, UQS(dots))
  
  if (name_order == "key_first") {
    output <- output %>% unite(ukey, UQ(key), gkey, sep = sep)
  } else if (name_order == "value_first") {
    output <- output %>% unite(ukey, gkey, UQ(key), sep = sep)
  }
  
  output <- output %>% spread(ukey, value, fill = fill, convert = convert, drop = drop)
  
  return (output)
}
