
#' @keywords internal

ace_dprime <- function(acc_col, snodgrass = TRUE) {
  hits = sum(acc_col == "Hit", na.rm = T)
  targets = sum(acc_col %in% c("Hit", "Miss"), na.rm = T)
  fas = sum(acc_col == "False Alarm", na.rm = T)
  nontargets = sum(acc_col %in% c("False Alarm", "Correct Rejection"), na.rm = T)
  if (snodgrass) {
    hit_rate = snodgrass_correction_count(hits, targets)
    fa_rate = snodgrass_correction_count(fas, nontargets)
  } else {
    hit_rate = hits / targets
    fa_rate = fas / nontargets
  }
  return (qnorm(hit_rate) - qnorm(fa_rate))
}

#' @keywords internal

ace_dprime_wide <- function(hit, fa, count_hit, count_fa) {
  # ALWAYS snodgrasses, because of low trial counts
    hit_rate = snodgrass_correction_rate(hit, count_hit)
    fa_rate = snodgrass_correction_rate(fa, count_fa)
  return (qnorm(hit_rate) - qnorm(fa_rate))
}

#' @keywords internal
#' Does not multiply by target so it conforms with single-arg function setup

ace_wm_prek <- function(acc_col) {
  hits = sum(acc_col == "Hit", na.rm = T)
  targets = sum(acc_col %in% c("Hit", "Miss"), na.rm = T)
  fas = sum(acc_col == "False Alarm", na.rm = T)
  nontargets = sum(acc_col %in% c("False Alarm", "Correct Rejection"), na.rm = T)
  
  hit_rate = hits / targets
  fa_rate = fas / nontargets
  
  return ((hit_rate - fa_rate))
  
}

#' @keywords internal

ace_wm_k_wide <- function(hit, fa, targets) {
  return (targets * (hit - fa))
}

#' @keywords internal

snodgrass_correction_count <- function(num, denom) {
  return ((num + 0.50) / (denom + 1))
}

#' @keywords internal

snodgrass_correction_rate <- function(rate, n) {
  return (((rate * n) + 0.50) / (n + 1))
}
