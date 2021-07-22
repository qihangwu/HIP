
# Setup -------------------------------------------------------------------

library(stargazer)
library(reshape2)
library(lfe)

source('code/merge.R')

## Add pooled variable ----------------------------------------------------

hip_analysis <- hip_analysis %>%
  mutate(treat2 = ifelse(treat2a == 1 | treat2b == 1, 1, 0),
         .after = treat2b)

## Vectors ----------------------------------------------------------------

orig_names <- c('guess_entry_salary',
                'guess_entry_salary_6m',
                'guess_you_salary_1m',
                'guess_salary_medium',
                'guess_salary_sp',
                'guess_you_salary_6m')

orig_benchmarks <- c(750,
                     1202,
                     750,
                     1707,
                     2857,
                     1202)

raw_names <- c('guess_entry_salary_raw',
               'guess_entry_salary_6m_raw',
               'guess_you_salary_1m_raw',
               'guess_salary_medium_raw',
               'guess_salary_sp_raw',
               'guess_you_salary_6m_raw')

abs_names <- c('guess_entry_salary_abs',
               'guess_entry_salary_6m_abs',
               'guess_you_salary_1m_abs',
               'guess_salary_medium_abs',
               'guess_salary_sp_abs',
               'guess_you_salary_6m_abs')

bias_names <- c('guess_entry_salary_bias',
                'guess_entry_salary_6m_bias',
                'guess_you_salary_1m_bias',
                'guess_salary_medium_bias',
                'guess_salary_sp_bias',
                'guess_you_salary_6m_bias')


# Pooled ------------------------------------------------------------------

## Interaction ------------------------------------------------------------

### Figures ---------------------------------------------------------------

for (i in 1:6) {
  gen_density(variable = orig_names[i], benchmark = orig_benchmarks[i], pooled = TRUE)
  ggsave(file = paste0('figures/treatment_bias/pool_', orig_names[i], '.png'))
}

for (i in 1:6) {   # identical to 'orig' except shifted
  gen_density(variable = raw_names[i], benchmark = 0, pooled = TRUE)
  ggsave(file = paste0('figures/treatment_bias/pool_', raw_names[i], '.png'))
}

for (i in 1:6) {
  gen_density(variable = abs_names[i], benchmark = 0, pooled = TRUE)
  ggsave(file = paste0('figures/treatment_bias/pool_', abs_names[i], '.png'))
}

for (i in 1:6) {
  gen_bar(variable = bias_names[i], pooled = TRUE)
  ggsave(file = paste0('figures/treatment_bias/pool_', bias_names[i], '.png'))
}

### Regression ------------------------------------------------------------

formula_pool <- paste0('w_',
                       orig_names,
                       ' ~ ',
                       'treat1 + treat2 + ',
                       orig_names,
                       ' | firm + today_day | 0 | firm + today_day')

formula_pool_int <- paste0('w_',
                           orig_names,
                           ' ~ ',
                           'treat1 * treat2 + ',
                           orig_names,
                           ' | firm + today_day | 0 | firm + today_day')

reg_pool <- vector(mode = 'list', length = 6)

for (i in 1:6) {
  reg_pool[[i]] <- felm(as.formula(formula_pool[i]), data = hip_analysis)
}

reg_pool_int <- vector(mode = 'list', length = 6)

for (i in 1:6) {
  reg_pool_int[[i]] <- felm(as.formula(formula_pool_int[i]), data = hip_analysis)
}

## No interaction ---------------------------------------------------------




# Separated ---------------------------------------------------------------

## Interaction ------------------------------------------------------------

### Figures ---------------------------------------------------------------

for (i in 1:6) {
  gen_density(variable = orig_names[i], benchmark = orig_benchmarks[i], pooled = FALSE)
  ggsave(file = paste0('figures/treatment_bias/sep_', orig_names[i], '.png'))
}

for (i in 1:6) {   # identical to 'orig' except shifted
  gen_density(variable = raw_names[i], benchmark = 0, pooled = FALSE)
  ggsave(file = paste0('figures/treatment_bias/sep_', raw_names[i], '.png'))
}

for (i in 1:6) {
  gen_density(variable = abs_names[i], benchmark = 0, pooled = FALSE)
  ggsave(file = paste0('figures/treatment_bias/sep_', abs_names[i], '.png'))
}

for (i in 1:6) {
  gen_bar(variable = bias_names[i], pooled = FALSE)
  ggsave(file = paste0('figures/treatment_bias/sep_', bias_names[i], '.png'))
}

### Regression ------------------------------------------------------------

formula_sep <- paste0('w_',
                       orig_names,
                       ' ~ ',
                       'treat1 + treat2a + treat2b + ',
                       orig_names,
                       ' | firm + today_day | 0 | firm + today_day')

formula_sep_int <- paste0('w_',
                           orig_names,
                           ' ~ ',
                           'treat1 * (treat2a + treat2b) + ',
                           orig_names,
                           ' | firm + today_day | 0 | firm + today_day')

reg_sep <- vector(mode = 'list', length = 6)

for (i in 1:6) {
  reg_sep[[i]] <- felm(as.formula(formula_sep[i]), data = hip_analysis)
}

reg_sep_int <- vector(mode = 'list', length = 6)

for (i in 1:6) {
  reg_sep_int[[i]] <- felm(as.formula(formula_sep_int[i]), data = hip_analysis)
}

## No interaction ---------------------------------------------------------


