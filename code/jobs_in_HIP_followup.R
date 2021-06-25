
# Setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)

source('code/functions.R')

# hip <- read_excel('data/weekdayend_all.xlsx')   # 295 obs. of 381 variables
hip <- read_excel('data/weekdayend_all2.xlsx')   # 525 obs. of 382 variables

dict <- read_csv('dictionary.csv')


# IB (weekend) ------------------------------------------------------------

w_IB_names <- dict %>%
  filter(subcategory == 'IB_w') %>%
  pull(name)

## Recoding ---------------------------------------------------------------

hip_w_IB <- hip %>%
  select(all_of(w_IB_names)) %>%
  mutate(across(w_IB_names[seq(1, 13, 2)], as.integer)) %>%
  mutate(across(w_IB_names[seq(2, 14, 2)], as.factor)) %>%
  mutate(across(w_IB_names[seq(2, 14, 2)], ~recode(.,
                                             `0` = 'Very sure',
                                             `1` = 'Slightly sure',
                                             `2` = 'Slightly not sure',
                                             `3` = 'Not sure at all')))

## Trim errors ------------------------------------------------------------

hip_w_IB <- hip_w_IB %>%
  mutate(w_guess_hip_day = replace(w_guess_hip_day,
                                   which(w_guess_hip_day > 7L),
                                   NA)) %>%
  mutate(w_guess_hip_night = replace(w_guess_hip_night,
                                     which(w_guess_hip_night > 30L),
                                     NA)) %>%
  mutate(w_guess_hip_transp = replace(w_guess_hip_transp,
                                      which(w_guess_hip_transp > 22L),
                                      NA)) %>%
  mutate(w_guess_hip_lunch = replace(w_guess_hip_lunch,
                                     which(w_guess_hip_lunch > 22L),
                                     NA))


# IC (weekend) ------------------------------------------------------------

w_IC_names <- dict %>%
  filter(subcategory == 'IC_w') %>%
  pull(name)

## Recoding ---------------------------------------------------------------

hip_w_IC <- hip %>%
  select(all_of(w_IC_names)) %>%
  mutate(across(w_IC_names[c(1, 3, 5, 8)], as.integer)) %>%
  mutate(across(w_IC_names[c(2, 4, 6, 7)], as.factor)) %>%
  mutate(across(w_IC_names[c(2, 4, 6)], ~recode(.,
                                          `0` = 'Very sure',
                                          `1` = 'Slightly sure',
                                          `2` = 'Slightly not sure',
                                          `3` = 'Not sure at all'))) %>%
  mutate(w_guess_entry_pct_you = recode(w_guess_entry_pct_you,
                                        `0` = 'Likely',
                                        `1` = 'Somewhat likely',
                                        `2` = 'Somewhat unlikely',
                                        `3` = 'Very unlikely'))

## Winsorizing and trimming outliers --------------------------------------

hip_w_IC <- hip_w_IC %>%

  mutate(w_guess_entry_salary = replace(
    w_guess_entry_salary,
    which(w_guess_entry_salary > quantile(w_guess_entry_salary, 0.99, na.rm = TRUE)),
    quantile(w_guess_entry_salary, 0.99, na.rm = TRUE))) %>%
  mutate(w_guess_entry_salary = replace(
    w_guess_entry_salary,
    which(w_guess_entry_salary < quantile(w_guess_entry_salary, 0.01, na.rm = TRUE)),
    NA_real_)) %>%

  mutate(w_guess_entry_salary_6m = replace(
    w_guess_entry_salary_6m,
    which(w_guess_entry_salary_6m > quantile(w_guess_entry_salary_6m, 0.99, na.rm = TRUE)),
    quantile(w_guess_entry_salary_6m, 0.99, na.rm = TRUE))) %>%
  mutate(w_guess_entry_salary_6m = replace(
    w_guess_entry_salary_6m,
    which(w_guess_entry_salary_6m < quantile(w_guess_entry_salary_6m, 0.01, na.rm = TRUE)),
    NA_real_)) %>%

  mutate(w_guess_you_salary_1m = replace(
    w_guess_you_salary_1m,
    which(w_guess_you_salary_1m > quantile(w_guess_you_salary_1m, 0.99, na.rm = TRUE)),
    quantile(w_guess_you_salary_1m, 0.99, na.rm = TRUE))) %>%
  mutate(w_guess_you_salary_1m = replace(
    w_guess_you_salary_1m,
    which(w_guess_you_salary_1m < quantile(w_guess_you_salary_1m, 0.01, na.rm = TRUE)),
    NA_real_))

## Bias -------------------------------------------------------------------

hip_w_IC <- calc_bias(data = hip_w_IC,
                      variable = w_IC_names[1],
                      benchmark = 750,
                      threshold = 0.2)

hip_w_IC <- calc_bias(data = hip_w_IC,
                      variable = w_IC_names[3],
                      benchmark = 1202,
                      threshold = 0.2)

hip_w_IC <- calc_bias(data = hip_w_IC,
                      variable = w_IC_names[8],
                      benchmark = 750,
                      threshold = 0.2)


# ID (weekend) ------------------------------------------------------------

w_ID_names <- dict %>%
  filter(subcategory == 'ID_w') %>%
  pull(name)

## Recoding ---------------------------------------------------------------

hip_w_ID <- hip %>%
  select(all_of(w_ID_names)) %>%
  mutate(across(w_ID_names[c(1, 3, 5, 7, 11)], as.integer)) %>%
  mutate(across(w_ID_names[c(2, 4, 6, 8, 9, 10)], as.factor)) %>%
  mutate(across(w_ID_names[c(2, 4, 6, 8)], ~recode(.,
                                             `0` = 'Very sure',
                                             `1` = 'Slightly sure',
                                             `2` = 'Slightly not sure',
                                             `3` = 'Not sure at all'))) %>%
  mutate(across(w_ID_names[9:10], ~recode(.,
                                    `0` = 'Likely',
                                    `1` = 'Somewhat likely',
                                    `2` = 'Somewhat unlikely',
                                    `3` = 'Very unlikely')))

## Winsorizing and trimming outliers --------------------------------------

hip_w_ID <- hip_w_ID %>%

  mutate(w_guess_salary_medium = replace(
    w_guess_salary_medium,
    which(w_guess_salary_medium > quantile(w_guess_salary_medium, 0.99, na.rm = TRUE)),
    quantile(w_guess_salary_medium, 0.99, na.rm = TRUE))) %>%
  mutate(w_guess_salary_medium = replace(
    w_guess_salary_medium,
    which(w_guess_salary_medium < 100L),   # too many above 1st percentile
    NA_real_)) %>%

  mutate(w_guess_salary_sp = replace(
    w_guess_salary_sp,
    which(w_guess_salary_sp > quantile(w_guess_salary_sp, 0.99, na.rm = TRUE)),
    quantile(w_guess_salary_sp, 0.99, na.rm = TRUE))) %>%
  mutate(w_guess_salary_sp = replace(
    w_guess_salary_sp,
    which(w_guess_salary_sp < quantile(w_guess_salary_sp, 0.01, na.rm = TRUE)),
    NA_real_)) %>%

  mutate(w_guess_you_salary_6m = replace(
    w_guess_you_salary_6m,
    which(w_guess_you_salary_6m > quantile(w_guess_you_salary_6m, 0.99, na.rm = TRUE)),
    quantile(w_guess_you_salary_6m, 0.99, na.rm = TRUE))) %>%
  mutate(w_guess_you_salary_6m = replace(
    w_guess_you_salary_6m,
    which(w_guess_you_salary_6m < quantile(w_guess_you_salary_6m, 0.01, na.rm = TRUE)),
    NA_real_))

## Trim errors ------------------------------------------------------------

hip_w_ID <- hip_w_ID %>%
  mutate(w_guess_promote_medium = replace(w_guess_promote_medium,
                                          which(w_guess_promote_medium > 100L),
                                          NA)) %>%
  mutate(w_guess_promote_sp = replace(w_guess_promote_sp,
                                      which(w_guess_promote_sp > 100L),
                                      NA))

## Bias -------------------------------------------------------------------

hip_w_ID <- calc_bias(data = hip_w_ID,
                      variable = w_ID_names[5],
                      benchmark = 1707,
                      threshold = 0.2)

hip_w_ID <- calc_bias(data = hip_w_ID,
                      variable = w_ID_names[7],
                      benchmark = 2857,
                      threshold = 0.2)

hip_w_ID <- calc_bias(data = hip_w_ID,
                      variable = w_ID_names[11],
                      benchmark = 1202,
                      threshold = 0.2)


# IA (evening) ------------------------------------------------------------

w_IA_names <- dict %>%
  filter(subcategory == 'IA_w') %>%
  pull(name)

## Recoding ---------------------------------------------------------------

hip_w_IA <- hip %>%
  select(all_of(w_IA_names)) %>%
  mutate(across(all_of(w_IA_names), as.factor)) %>%
  mutate(across(w_IA_names[c(1, 2, 3, 5, 6, 8, 9, 11, 12, 14)], ~recode(.,
                                                            `0` = 'Frequently every day',
                                                            `1` = 'Sometimes every day',
                                                            `2` = 'Once every day',
                                                            `3` = 'A few times a week',
                                                            `4` = 'Once a week',
                                                            `6` = 'Never',
                                                            `-8` = "I'm not sure"))) %>%
  mutate(across(w_IA_names[c(1, 2, 3, 5, 6, 8, 9, 11, 12, 14)], ~fct_relevel(.,
                                                                 "I'm not sure",
                                                                 after = Inf))) %>%
  mutate(across(w_IA_names[c(4, 7, 10, 13)], ~recode(.,
                                         `0` = 'Not welcomed at all',
                                         `1` = 'Somewhat not welcomed',
                                         `2` = 'Somewhat welcomed',
                                         `3` = 'Very welcomed'))) %>%
  mutate(interact_break = recode(interact_break,
                                 `0` = 'No',
                                 `1` = 'Yes',
                                 `100` = 'Not sure'))





