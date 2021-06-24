
# Generate bias variables -------------------------------------------------

calc_bias <- function(data, variable, benchmark, threshold = 0.2) {
  data <- data %>%
    mutate('{variable}_raw' := !!sym(variable) - benchmark,   # raw bias
           .after = !!sym(variable)) %>%

    mutate('{variable}_abs' := abs(!!sym(paste0(variable, '_raw'))) / benchmark,   # absolute bias
           .after = !!sym(paste0(variable, '_raw'))) %>%

    mutate('{variable}_bias' := ifelse(!!sym(paste0(variable, '_abs')) >= threshold, 1, 0),   # bias sign
           .after = !!sym(paste0(variable, '_abs'))) %>%
    mutate('{variable}_bias' := ifelse(!!sym(paste0(variable, '_bias')) == 1 &
                                         sign(!!sym(paste0(variable, '_raw'))) == -1,
                                       -1,
                                       !!sym(paste0(variable, '_bias'))))
}


# Trim and winsorize salary variables -------------------------------------

trim_winsorize <- function(data, variable, trim = 100, percentile = 0.99) {
  for (i in variable) {
    data <- data %>%
      # trim
      mutate('{i}' := replace(!!sym(i),
                              which(!!sym(i) <= trim),
                              NA_real_)) %>%
      # winsorize
      mutate('{i}' := replace(!!sym(i),
                              which(!!sym(i) > quantile(!!sym(i),
                                                        percentile,
                                                        na.rm = TRUE)),
                              quantile(!!sym(i),
                                       percentile,
                                       na.rm = TRUE)))
  }
  return(data)
}
