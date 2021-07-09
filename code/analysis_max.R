

# Setup -------------------------------------------------------------------

library(stargazer)
library(reshape2)

source('code/merge.R')


# Pooled ------------------------------------------------------------------

hip_analysis_pool <- hip_analysis %>%
  mutate(treat2 = ifelse(treat2a == 1 | treat2b == 1, 1, 0),
         .after = treat2b) %>%
  mutate(treat12 = treat1 * treat2)

## No interaction ---------------------------------------------------------



## Interaction ------------------------------------------------------------

gg_pooled <- function(variable, benchmark) {

  plot <- ggplot(data = hip_analysis_pool) +
    geom_density(aes(x = !!sym(variable)),
                 size = 0.8) +
    geom_density(aes(x = !!sym(paste0('w_', variable))),
                 size = 0.8,
                 color = 'cornflowerblue') +
    geom_vline(aes(xintercept = benchmark),
               color = 'red') +
    facet_grid(treat2 ~ treat1, labeller = 'label_both') +
    labs(title = variable,
         subtitle = 'Black is morning, blue is weekend',
         x = '') +
    theme_minimal()

  ggsave(plot = plot, file = paste0('figures/treatment_bias/pool_', variable, '.png'))
}

gg_pooled_bar <- function(vari) {

  data <- hip_analysis_pool %>%
    select(c('wid', all_of(vari), paste0('w_', vari))) %>%
    melt(id.vars = 'wid')

  plot <- ggplot(data = data) +
    geom_bar(aes(x = value, fill = variable), position = 'dodge') +
    #    facet_wrap(~treat2, labeller = 'label_both') +
    labs(title = vari,
         x = 'sign') +
    scale_fill_discrete(name = 'Survey',
                        labels = c('morning', 'weekend'))
  ggsave(plot = plot, file = paste0('figures/treatment_bias/pool_', vari, '.png'))
}

### Original --------------------------------------------------------------

pool_names_orig <- c('guess_entry_salary',
                     'guess_entry_salary_6m',
                     'guess_you_salary_1m',
                     'guess_salary_medium',
                     'guess_salary_sp',
                     'guess_you_salary_6m')

pool_benchmarks_orig <- c(750,
                          1202,
                          750,
                          1707,
                          2857,
                          1202)

for (i in 1:6) {
  gg_pooled(pool_names_orig[i], pool_benchmarks_orig[i])
}

### Raw -------------------------------------------------------------------

pool_names_raw <- c('guess_entry_salary_raw',
                    'guess_entry_salary_6m_raw',
                    'guess_you_salary_1m_raw',
                    'guess_salary_medium_raw',
                    'guess_salary_sp_raw',
                    'guess_you_salary_6m_raw')

for (i in 1:6) {
  gg_pooled(pool_names_raw[i],0)
}

### Absolute --------------------------------------------------------------

pool_names_abs <- c('guess_entry_salary_abs',
                    'guess_entry_salary_6m_abs',
                    'guess_you_salary_1m_abs',
                    'guess_salary_medium_abs',
                    'guess_salary_sp_abs',
                    'guess_you_salary_6m_abs')

for (i in 1:6) {
  gg_pooled(pool_names_abs[i],0)
}

### Bias sign -------------------------------------------------------------

pool_names_sign <- c('guess_entry_salary_bias',
                     'guess_entry_salary_6m_bias',
                     'guess_you_salary_1m_bias',
                     'guess_salary_medium_bias',
                     'guess_salary_sp_bias',
                     'guess_you_salary_6m_bias')

for (i in 1:6) {
  gg_pooled_bar(pool_names_sign[i])
}

### Regression ------------------------------------------------------------

hip_analysis_pool <- hip_analysis_pool %>%
  mutate(guess_entry_salary_diff = w_guess_entry_salary - guess_entry_salary,
         .after = guess_entry_salary_bias) %>%
  mutate(guess_salary_medium_diff = w_guess_salary_medium - guess_salary_medium,
         .after = guess_salary_medium_bias)

reg1 <- lm(w_guess_salary_medium_bias ~ treat1 + treat2 + treat12 + guess_salary_medium_bias, data = hip_analysis_pool)

reg2 <- lm(guess_salary_medium_diff ~ treat1 + treat2, data = hip_analysis_pool)

stargazer(reg1, type = 'text')

stargazer(reg2, type = 'text')


# Separated ---------------------------------------------------------------

## No interaction ---------------------------------------------------------



## Interaction ------------------------------------------------------------

hip_analysis_sep <- hip_analysis

gg_sep <- function(variable, benchmark) {

  plot <- ggplot(data = hip_analysis_sep) +
    geom_density(aes(x = !!sym(variable)),
                 size = 0.8) +
    geom_density(aes(x = !!sym(paste0('w_', variable))),
                 size = 0.8,
                 color = 'cornflowerblue') +
    geom_vline(aes(xintercept = benchmark),
               color = 'red') +
    facet_wrap(~treat1 + treat2a + treat2b, labeller = 'label_both') +
    labs(title = variable,
         subtitle = 'Black is morning, blue is weekend',
         x = '') +
    theme_minimal()

  ggsave(plot = plot, file = paste0('figures/treatment_bias/sep_', variable, '.png'))
}

### Original --------------------------------------------------------------

sep_names_orig <- c('guess_entry_salary',
                    'guess_entry_salary_6m',
                    'guess_you_salary_1m',
                    'guess_salary_medium',
                    'guess_salary_sp',
                    'guess_you_salary_6m')

sep_benchmarks_orig <- c(750,
                         1202,
                         750,
                         1707,
                         2857,
                         1202)

for (i in 1:6) {
  gg_sep(sep_names_orig[i], sep_benchmarks_orig[i])
}

### Raw -------------------------------------------------------------------

sep_names_raw <- c('guess_entry_salary_raw',
                   'guess_entry_salary_6m_raw',
                   'guess_you_salary_1m_raw',
                   'guess_salary_medium_raw',
                   'guess_salary_sp_raw',
                   'guess_you_salary_6m_raw')

for (i in 1:6) {
  gg_sep(sep_names_raw[i], 0)
}

### Absolute --------------------------------------------------------------

sep_names_abs <- c('guess_entry_salary_abs',
                   'guess_entry_salary_6m_abs',
                   'guess_you_salary_1m_abs',
                   'guess_salary_medium_abs',
                   'guess_salary_sp_abs',
                   'guess_you_salary_6m_abs')

for (i in 1:6) {
  gg_sep(sep_names_abs[i], 0)
}

### Bias sign -------------------------------------------------------------



### Regression ------------------------------------------------------------




