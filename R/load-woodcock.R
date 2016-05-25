
#' @keywords internal

transform_woodcock = function(file) {
  
  # load & clean
  dat = openxlsx::read.xlsx(file, sheet = 1, colNames = FALSE)
  header = dat[1,]
  dat = dat[-1,] # get rid of header
  dat = remove_empty_rows(dat)
  
  # init output
  out = data.frame()
  
  # group by student id & transform
  sids = which(!is.na(dat[, 1]))
  num_sids = length(sids)
  grows = seq(1:num_sids)
  gdat = list()
  for (i in grows) {
    
    # group
    first = sids[i]
    last = ifelse(i == num_sids, nrow(dat), sids[i + 1] - 1) 
    sdat = dat[first:last, ]
    sid = sdat[1, 1]
    names(sdat) = header
    
    # transform

    prefix = tolower(sdat[, 3 ])
    prefix = aceR:::to_title_case(prefix)
    prefix = aceR:::replace_spaces(prefix, "")
    
    wood_dat = sdat[, 3:length(sdat)]
    wood_dat[, 1] = prefix
    id = names(wood_dat)[1]
    wood_melt = reshape::melt(wood_dat, id = id)
    
    clust_prefix = unique(wood_melt[, 1])
    clust_suffix = unique(wood_melt[, 2])
    clust_seq = seq(clust_prefix)
    clust_num = length(clust_suffix)
    corrected_order = sapply(clust_seq, function(x) {
      cname = sapply(clust_suffix, function(y) paste(clust_prefix[x], y, sep = "_"))
      return (cname)
    }) 
    corrected_order = as.vector(corrected_order)
    
    user_dat = data.frame(
      val = wood_melt$value
    )
    user_dat_t = data.frame(t(user_dat))
    names(user_dat_t) = paste(wood_melt[, 1], wood_melt[, 2], sep = "_")
    
    # merge
    wood = data.frame(
      sid = sdat[1, 1],
      date = sdat[1, 2]
    ) 
    wood = merge(wood, user_dat_t[corrected_order])
    out = plyr::rbind.fill(out, wood)

    # clean-up
    out$sid = gsub(" ", "", out$sid)
    out = out[order(as.character(out$sid)), ] 
    good_column_names = filter_out_vec(names(out), "_to")
    out = out[good_column_names]
    
    out$pid = paste("ADMIN-UCSF", out$sid, sep = "-")
    out$pid = gsub("SP", "", out$pid)
  }
  return (out)
}

#' @keywords internal

load_woodcock_transformed = function(file, suffix = NULL) {
  # load file
  woodcock = read.csv(file)
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
    if (!is.null(suffix)) {
      new_name = paste(new_name, suffix, sep = ".")
    }
    return (new_name)
  })
  woodcock_ss_score = cbind(woodcock[, c(COL_PID)], woodcock_ss_score)
  names(woodcock_ss_score)[1] = COL_PID
  return (woodcock_ss_score)
}