
# TODO: a lot going on here, probably break it up.

#' @keywords internal

load_woodcock_transformed = function(path = ".", pattern = "transform") {
  # load files
  woodcock = load_files(path = path, pattern = pattern)
  cols = names(woodcock)
  # prepare woodcock data
  woodcock_ss = woodcock[stringr::str_detect(cols, "SS")]
  woodcock_ss_score = apply(woodcock_ss, 2, function(x) {
    score = first_number(x)
    score = remove_whitespace(score)
    score = ifelse(is.na(score), NA, as.numeric(as.character(score)))
    return (score)
  })
  woodcock_ss_score = as.data.frame(woodcock_ss_score)
  names(woodcock_ss_score) = sapply(names(woodcock_ss_score), function(x) {
    new_name = remove_special_characters(x, "")
    new_name = gsub("SS", "_SS_", new_name)
    new_name = gsub("Band", "_Band", new_name)
    return (new_name)
  })
  woodcock_ss_score = cbind(woodcock[, c(COL_PID, "age_group")], woodcock_ss_score)
  return (woodcock_ss_score)
}