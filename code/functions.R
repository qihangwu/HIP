
# Generate bias variables -------------------------------------------------

calc_bias <- function(data, variable, benchmark, threshold = 0.2) {
  data <- data %>%
    mutate('{variable}_raw' := !!sym(variable) - benchmark,   # raw bias
           .after = !!sym(variable)) %>%

    mutate('{variable}_abs' := abs(!!sym(paste0(variable, '_raw'))) / benchmark,   # absolute bias
           .after = !!sym(paste0(variable, '_raw'))) %>%

    mutate('{variable}_bias' := ifelse(!!sym(paste0(variable, '_abs')) >= threshold, 1, 0),   # bias sign
           .after = !!sym(paste0(variable, '_abs'))) %>%
    mutate('{variable}_bias' := ifelse(!!sym(paste0(variable, '_bias')) &
                                         sign(!!sym(paste0(variable, '_raw'))) == -1,
                                       -1,
                                       !!sym(paste0(variable, '_bias'))))
}

