
# Generate bias variables -------------------------------------------------

calc_bias <- function(data, variable, benchmark, threshold = 0.2) {
  data <- data %>%
    # raw bias
    mutate('{variable}_raw' := !!sym(variable) - benchmark,
           .after = !!sym(variable)) %>%

    # absolute bias
    mutate('{variable}_abs' := abs(!!sym(paste0(variable, '_raw'))) / benchmark,
           .after = !!sym(paste0(variable, '_raw'))) %>%

    # sign of bias
    mutate('{variable}_bias' := ifelse(!!sym(paste0(variable, '_abs')) >= threshold,
                                       1,
                                       0),
           .after = !!sym(paste0(variable, '_abs'))) %>%
    mutate('{variable}_bias' := ifelse(!!sym(paste0(variable, '_bias')) == 1 &
                                         sign(!!sym(paste0(variable, '_raw'))) == -1,
                                       -1,
                                       !!sym(paste0(variable, '_bias'))))
  return(data)
}


# Trim and winsorize salary variables -------------------------------------

trim_winsorize <- function(data, variable, trim = 100, percentile = 0.99) {
  for (i in variable) {   # can take in vector of variable names
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

# Generate analysis figures -----------------------------------------------

# use for continuous salary variables
gen_density <- function(data = hip_analysis, variable, benchmark, pooled = TRUE) {
  # select necessary columns and melt to long format
  data <- data %>%
    select(c('wid',
             'treat1',
             'treat2a',
             'treat2b',
             'treat2',
             grep(paste0(variable, '$'), names(data)))) %>%

    rename('morning' = !!sym(variable) ,
           'weekend' = !!sym(paste0('w_', variable))) %>%

    melt(id.vars = c('wid',
                     'treat1',
                     'treat2a',
                     'treat2b',
                     'treat2'),
         variable.name = 'survey',
         value.name = 'salary')

  # create plot
  plot <- ggplot(data = data) +
    geom_density(aes(x = salary, color = survey),   # kernel is 'gaussian' by default
                 size = 0.8) +
    geom_vline(aes(xintercept = benchmark),
               color = 'red') +

    labs(title = paste0(variable, ', benchmark is ', benchmark, ' birr'),
         x = '') +   # remove x-axis label
    scale_color_manual(values = c('gray10', 'cornflowerblue')) +
    theme_minimal()

  if (pooled == TRUE) {
    plot <- plot +
      facet_wrap(~ treat1 + treat2,
                 labeller = 'label_both')
  } else if (pooled == FALSE) {
    plot <- plot +
      facet_wrap(~ treat1 + treat2a + treat2b,
                 labeller = 'label_both')
  }
  return(plot)
}

# use for discrete salary variables
gen_bar <- function(data = hip_analysis, variable, pooled = TRUE) {
  # select necessary columns, recode, rename, and melt to long format
  data <- data %>%
    select(c('wid',
             'treat1',
             'treat2a',
             'treat2b',
             'treat2',
             grep(paste0(variable, '$'), names(data)))) %>%
    mutate(across(6:7, as.factor)) %>%

    rename('morning' = !!sym(variable) ,
           'weekend' = !!sym(paste0('w_', variable))) %>%

    melt(id.vars = c('wid',
                     'treat1',
                     'treat2a',
                     'treat2b',
                     'treat2'),
         variable.name = 'survey',
         value.name = 'salary')

  plot <- ggplot(data = data) +
    geom_bar(aes(x = salary, fill = survey),
                 position = position_dodge2(preserve = 'single')) +

    labs(title = variable,
         x = '') +
    scale_fill_manual(values = c('gray10', 'cornflowerblue')) +
    theme_minimal()

  if (pooled == TRUE) {
    plot <- plot +
      facet_wrap(~ treat1 + treat2,
                 labeller = 'label_both',
                 scales = 'free') +
      scale_x_discrete(limits = c('-1', '0', '1'))
  } else if (pooled == FALSE) {
    plot <- plot +
      facet_wrap(~ treat1 + treat2a + treat2b,
                 labeller = 'label_both',
                 scales = 'free') +
      scale_x_discrete(limits = c('-1', '0', '1'))
  }
  return(plot)
}
