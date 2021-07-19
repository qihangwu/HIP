
# Setup -------------------------------------------------------------------

library(stargazer)
library(reshape2)
library(lfe)

source('code/merge.R')

## Add interaction variable -----------------------------------------------

hip_analysis <- hip_analysis %>%
  mutate(treat2 = ifelse(treat2a == 1 | treat2b == 1, 1, 0),
         .after = treat2b) %>%
  mutate(treat12 = treat1 * treat2,
         .after = treat2)

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

hip_analysis_pool <- hip_analysis_pool %>%
  mutate(guess_entry_salary_diff = w_guess_entry_salary - guess_entry_salary,
         .after = guess_entry_salary_bias) %>%
  mutate(guess_salary_medium_diff = w_guess_salary_medium - guess_salary_medium,
         .after = guess_salary_medium_bias)

reg1 <- lm(w_guess_salary_medium_bias ~ treat1 + treat2 + treat12 + guess_salary_medium_bias, data = hip_analysis_pool)

reg2 <- lm(guess_salary_medium_diff ~ treat1 + treat2, data = hip_analysis_pool)

stargazer(reg1, type = 'text')

stargazer(reg2, type = 'text')

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


## No interaction ---------------------------------------------------------



### Regression ------------------------------------------------------------

reg_fe <- felm(w_guess_entry_salary_raw ~
                 treat1 + treat2a + treat2b + guess_entry_salary_raw |
                 firm + today_day |
                 0 |
                 firm + today_day,
               data = hip_analysis_sep)

reg_fe1 <- felm(w_guess_entry_salary_raw ~
                 treat1 + treat2a + treat2b + guess_entry_salary_raw |
                 firm + today_day |
                 0 |
                 0,
               data = hip_analysis_sep)

stargazer(reg_fe, type = 'text')

stargazer(reg_fe1, type = 'text')
