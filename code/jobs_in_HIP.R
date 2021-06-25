
# Setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)

source('code/functions.R')

# hip <- read_excel('data/weekdayend_all.xlsx')   # 295 obs. of 381 variables
hip <- read_excel('data/weekdayend_all2.xlsx')   # 525 obs. of 382 variables

dict <- read_csv('dictionary.csv')


# IA (morning) ------------------------------------------------------------

IA_names <- dict %>%
  filter(subcategory == 'IA_m') %>%
  pull(name)

## Recoding ---------------------------------------------------------------

hip_IA <- hip %>%
  select(all_of(IA_names)) %>%
  mutate(across(IA_names[c(1, 4, 6)], as.factor)) %>%
  mutate(guess_hip_task = recode(guess_hip_task,
                                 `0` = 'Sewing',
                                 `1` = 'Quality check',
                                 `2` = 'Team leaders',
                                 `3` = 'Supervisors'#, `100` = 'others'
                                 )) %>%
  mutate(hip_knowppl_like = recode(hip_knowppl_like,
                                   `0` = 'Almost all of them',
                                   `1` = 'Some of them',
                                   `2` = 'Very few of them',
                                   `3` = 'Almost none of them')) %>%
  mutate(guess_hip_turnover_sure = recode(guess_hip_turnover_sure,
                                          `0` = 'Very sure',
                                          `1` = 'Slightly sure',
                                          `2` = 'Slightly not sure',
                                          `3` = 'Not sure at all')) %>%
  mutate(guess_hip_task_other = as.character(guess_hip_task_other)) %>%
  mutate(across(IA_names[c(3, 5)], as.integer))


# IB (morning) ------------------------------------------------------------

IB_names <- dict %>%
  filter(subcategory == 'IB_m') %>%
  pull(name)

## Recoding ---------------------------------------------------------------

hip_IB <- hip %>%
  select(all_of(IB_names)) %>%
  mutate(across(IB_names[seq(1, 13, 2)], as.integer)) %>%
  mutate(across(IB_names[seq(2, 14, 2)], as.factor)) %>%
  mutate(across(IB_names[seq(2, 14, 2)], ~recode(.,
                                                 `0` = 'Very sure',
                                                 `1` = 'Slightly sure',
                                                 `2` = 'Slightly not sure',
                                                 `3` = 'Not sure at all')))


## Trim errors --------------------------------------------------------------

hip_IB <- hip_IB %>%
  mutate(guess_hip_day = replace(guess_hip_day,
                                 which(guess_hip_day > 7L),
                                 NA)) %>%
  mutate(guess_hip_lunch = replace(guess_hip_lunch,
                                   which(guess_hip_lunch > 22L),
                                   NA)) %>%
  mutate(guess_hip_night = replace(guess_hip_night,
                                   which(guess_hip_night > 30L),
                                   NA))


# IC (morning) ------------------------------------------------------------

IC_names <- dict %>%
  filter(subcategory == 'IC_m') %>%
  pull(name)

## Recoding ---------------------------------------------------------------

hip_IC <- hip %>%
  select(all_of(IC_names)) %>%
  mutate(across(IC_names[c(1, 3, 5, 8)], as.integer)) %>%
  mutate(across(IC_names[c(2, 4, 6, 7)], as.factor)) %>%
  mutate(across(IC_names[c(2, 4, 6)], ~recode(.,
                                              `0` = 'Very sure',
                                              `1` = 'Slightly sure',
                                              `2` = 'Slightly not sure',
                                              `3` = 'Not sure at all'))) %>%
  mutate(guess_entry_pct_you = recode(guess_entry_pct_you,
                                      `0` = 'Likely',
                                      `1` = 'Somewhat likely',
                                      `2` = 'Somewhat unlikely',
                                      `3` = 'Very unlikely'))

## Trim errors ------------------------------------------------------------

hip_IC <- hip_IC %>%
  mutate(guess_entry_pct = replace(guess_entry_pct,
                                   which(guess_entry_pct > 100L),
                                   NA))

## Trim and winsorize outliers --------------------------------------------

hip_IC <- trim_winsorize(data = hip_IC,
                         variable = IC_names[c(1, 3, 8)],
                         # guess_entry_salary
                         # guess_entry_salary_6m
                         # guess_you_salary_1m
                         trim = 100,
                         percentile = 0.99)

## Bias -------------------------------------------------------------------

hip_IC <- calc_bias(data = hip_IC,
                    variable = IC_names[1],
                    benchmark = 750,
                    threshold = 0.2)

hip_IC <- calc_bias(data = hip_IC,
                    variable = IC_names[3],
                    benchmark = 1202,
                    threshold = 0.2)

hip_IC <- calc_bias(data = hip_IC,
                    variable = IC_names[8],
                    benchmark = 750,
                    threshold = 0.2)


# ID (morning) ------------------------------------------------------------

ID_names <- dict %>%
  filter(subcategory == 'ID_m') %>%
  pull(name)

## Recoding ---------------------------------------------------------------

hip_ID <- hip %>%
  select(all_of(ID_names)) %>%
  mutate(across(ID_names[c(1, 3, 5, 7, 11)], as.integer)) %>%
  mutate(across(ID_names[c(2, 4, 6, 8, 9, 10)], as.factor)) %>%
  mutate(across(ID_names[c(2, 4, 6, 8)], ~recode(.,
                                                 `0` = 'Very sure',
                                                 `1` = 'Slightly sure',
                                                 `2` = 'Slightly not sure',
                                                 `3` = 'Not sure at all'))) %>%
  mutate(across(ID_names[9:10], ~recode(.,
                                        `0` = 'Likely',
                                        `1` = 'Somewhat likely',
                                        `2` = 'Somewhat unlikely',
                                        `3` = 'Very unlikely')))

## Trim errors ------------------------------------------------------------

hip_ID <- hip_ID %>%
  mutate(guess_promote_medium = replace(guess_promote_medium,
                                        which(guess_promote_medium > 100L),
                                        NA)) %>%
  mutate(guess_promote_sp = replace(guess_promote_sp,
                                    which(guess_promote_sp > 100L),
                                    NA))

## Trim and winsorize outliers --------------------------------------------

hip_ID <- trim_winsorize(data = hip_ID,
                         variable = ID_names[c(5, 7, 11)],
                         # guess_salary_medium
                         # guess_salary_sp
                         # guess_you_salary_6m
                         trim = 100,
                         percentile = 0.99)

## Bias -------------------------------------------------------------------

hip_ID <- calc_bias(data = hip_ID,
                    variable = ID_names[5],
                    benchmark = 1707,
                    threshold = 0.2)

hip_ID <- calc_bias(data = hip_ID,
                    variable = ID_names[7],
                    benchmark = 2857,
                    threshold = 0.2)

hip_ID <- calc_bias(data = hip_ID,
                    variable = ID_names[11],
                    benchmark = 1202,
                    threshold = 0.2)


# IT (morning) ------------------------------------------------------------

IT_names <- dict %>%
  filter(subcategory == 'IT_m') %>%
  pull(name)

IT_names_entry <- IT_names[1:5]

IT_names_promote <- IT_names[6:10]

## info_entry_choice* -----------------------------------------------------

hip_IT_entry <- hip %>%
  select(all_of(IT_names_entry))

for (i in 1:5) {
  hip_IT_entry <- hip_IT_entry %>%
    mutate(!!sym(IT_names_entry[i]) := recode(!!sym(IT_names_entry[i]),
                                               `0` = 200,
                                               `1` = 10 * 2^(i - 1)))
}

hip_IT_entry <- hip_IT_entry %>%
  rowwise() %>%
  mutate(info_entry_choice = min(across(all_of(IT_names_entry)), na.rm = TRUE))


## info_promote_choice* ---------------------------------------------------

hip_IT_promote <- hip %>%
  select(all_of(IT_names_promote))

for (i in 1:5) {
  hip_IT_promote <- hip_IT_promote %>%
    mutate(!!sym(IT_names_promote[i]) := recode(!!sym(IT_names_promote[i]),
                                              `0` = 200,
                                              `1` = 10 * 2^(i - 1)))
}

hip_IT_promote <- hip_IT_promote %>%
  rowwise() %>%
  mutate(info_promote_choice = min(across(all_of(IT_names_promote)), na.rm = TRUE))





