
#' Renames tibble column "x" to "y", dropping the old contents of "y"
#' @keywords internal
#' @importFrom dplyr rename select

rename_overwrite <- function (df, col_rename, col_drop) {
  renamed <- df %>%
    select(-{{col_drop}}) %>% 
    rename({{col_drop}} := {{col_rename}})
  
  return (renamed)
}
