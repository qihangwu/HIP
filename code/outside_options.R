
# Setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)

# setwd('')   # unnecessary to set working directory if you opened `HIP.Rproj`

hip <- read_excel('data/weekdayend_all.xlsx')   # 295 obs. of 381 variables

# hip <- read_excel('data/weekdayend_all2.xlsx')   # 525 obs. of 382 variables


# OA (morning) ------------------------------------------------------------

OA <- c('guess_out_salary',
        'guess_out_hour',
        'guess_out_day',
        'guess_out_extra',
        'guess_out_night',
        'guess_out_transp',
        'guess_out_lunch',
        'guess_out_attend')

OA_ms <- c('minimal_salary_a',
           'minimal_salary_b',
           'minimal_salary_c',
           'minimal_salary_d',
           'minimal_salary_e',
           'minimal_salary_f',
           'minimal_salary_g',
           'minimal_salary_h',
           'minimal_salary_i',
           'minimal_salary_j',
           'minimal_salary_k',
           'minimal_salary_l',
           'minimal_salary_m',
           'minimal_salary_n',
           'minimal_salary_o')

## Initial cleaning -------------------------------------------------------

### All but `minimal_salary_*` --------------------------------------------

hip_OA <- hip %>%
  select(all_of(OA)) %>%
  mutate(across(OA[1:5], as.integer)) %>%
  mutate(across(OA[6:8], as.factor)) %>%
  mutate(across(OA[6:8], ~recode(.,
                                 `0` = 'Likely',
                                 `1` = 'Somewhat likely',
                                 `2` = 'Somewhat unlikely',
                                 `3` = 'Very unlikely')
                ))

### `minimal_salary_*` ----------------------------------------------------

hip_OA_ms <- hip %>%
  select(all_of(OA_ms))

# replace each `1` in `minimal_salary_*` with its corresponding value
# e.g. each `1` in `minimal_salary_a` becomes `600`
# and replace each `0` with `NA` of type double
for (i in 1:15) {
  hip_OA_ms <- hip_OA_ms %>%
    mutate(!!sym(OA_ms[i]) := recode(!!sym(OA_ms[i]),
                                     `0` = NA_real_,
                                     `1` = (i + 5) * 100))
}

# create new variable `minimal_salary`
# that is the minimum value across all `minimal_salary_*` columns
hip_OA_ms <- hip_OA_ms %>%
  rowwise() %>%
  mutate(minimal_salary = min(across(all_of(OA_ms)), na.rm = TRUE))


## Figures ----------------------------------------------------------------

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_salary), binwidth = 500) +
  ggsave('figures/outside_options/OA1.png')

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_hour), binwidth = 1) +
  ggsave('figures/outside_options/OA2.png')

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_day), binwidth = 1) +
  ggsave('figures/outside_options/OA3.png')

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_extra), binwidth = 1) +
  ggsave('figures/outside_options/OA4.png')

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_night), binwidth = 1) +
  ggsave('figures/outside_options/OA4b.png')

ggplot(data = hip_OA) +
  geom_bar(aes(x = guess_out_transp)) +
  ggsave('figures/outside_options/OA5.png')

ggplot(data = hip_OA) +
  geom_bar(aes(x = guess_out_lunch)) +
  ggsave('figures/outside_options/OA6.png')

ggplot(data = hip_OA) +
  geom_bar(aes(x = guess_out_attend)) +
  ggsave('figures/outside_options/OA7.png')

# constructed variable
ggplot(data = hip_OA_ms) +
  geom_histogram(aes(x = minimal_salary), binwidth = 100) +
  ggsave('figures/outside_options/OA8.png')


# OB (morning) ------------------------------------------------------------

OB <- c('guess_out_salary_1y',
        'guess_out_salary_1y_com',
        'guess_out_promote',
        'guess_out_salary_super',
        'guess_out_promote_com')

## Initial cleaning -------------------------------------------------------

hip_OB <- hip %>%
  select(all_of(OB)) %>%
  mutate(across(OB[c(1, 3, 4)], as.integer)) %>%
  mutate(across(OB[c(2, 5)], as.factor)) %>%
  mutate(across(OB[c(2, 5)], ~recode(.,
                                     `0` = 'No',
                                     `1` = 'Yes',
                                     `100` = 'Not sure')))

## Figures ----------------------------------------------------------------

ggplot(data = hip_OB) +
  geom_histogram(aes(x = guess_out_salary_1y), binwidth = 500) +
  ggsave('figures/outside_options/OB2.png')

ggplot(data = hip_OB) +
  geom_bar(aes(x = guess_out_salary_1y_com)) +
  ggsave('figures/outside_options/OB2c.png')

ggplot(data = hip_OB) +
  geom_histogram(aes(x = guess_out_promote), binwidth = 10) +
  ggsave('figures/outside_options/OB3.png')

ggplot(data = hip_OB) +
  geom_histogram(aes(x = guess_out_salary_super), binwidth = 500) +
  ggsave('figures/outside_options/OB4.png')

ggplot(data = hip_OB) +
  geom_bar(aes(x = guess_out_promote_com)) +
  ggsave('figures/outside_options/OB4c.png')


# OC (morning) ------------------------------------------------------------

OC <- c('jobaspect_first',
        'jobaspect_first_other',
        'jobaspect_second',
        'jobaspect_second_other',
        'jobaspect_third',
        'jobaspect_third_other')

## Initial cleaning -------------------------------------------------------

hip_OC <- hip %>%
  select(all_of(OC)) %>%
  mutate(across(OC[c(1, 3, 5)], as.factor)) %>%
  mutate(across(OC[c(1, 3, 5)], ~recode(.,
                                        `10` = 'Salary as an entry-level worker in the first month',
                                        `11` = 'Salary as an entry-level worker after 6 months',
                                        `12` = 'Chance of promotion to a higher level after 6 months',
                                        `13` = 'Salary in the higher level',
                                        `2` = 'Provide good work benefit',
                                        `3` = 'Reasonable work hours',
                                        `4` = 'The task is interesting',
                                        `5` = 'Skill development',
                                        `6` = 'Good management'#, `100` = 'Others'
                                        ))) %>%
  mutate(across(OC[c(1, 3, 5)], ~fct_relevel(.,
                                             'Salary as an entry-level worker in the first month',
                                             'Salary as an entry-level worker after 6 months',
                                             'Chance of promotion to a higher level after 6 months',
                                             'Salary in the higher level',
                                             'Provide good work benefit',
                                             'Reasonable work hours',
                                             'The task is interesting',
                                             'Skill development',
                                             'Good management'#, 'Others'
                                             ))) %>%
  mutate(across(OC[c(2, 4, 6)], as.character))

## Figures ----------------------------------------------------------------

ggplot(data = hip_OC) +
  geom_bar(aes(x = jobaspect_first)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  ggsave('figures/outside_options/OC1i.png')

ggplot(data = hip_OC) +
  geom_bar(aes(x = jobaspect_second)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  ggsave('figures/outside_options/OC1ii.png')

ggplot(data = hip_OC) +
  geom_bar(aes(x = jobaspect_third)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  ggsave('figures/outside_options/OC1iii.png')








