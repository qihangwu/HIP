
# Setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)

# hip <- read_excel('data/weekdayend_all.xlsx')   # 295 obs. of 381 variables
hip <- read_excel('data/weekdayend_all2.xlsx')   # 525 obs. of 382 variables

dict <- read_csv('dictionary.csv')


# IA (morning) ------------------------------------------------------------

IA_names <- dict %>%
  filter(subcategory == 'IA_m') %>%
  pull(name)

## Cleaning ---------------------------------------------------------------

### Recoding --------------------------------------------------------------

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

## Figures ----------------------------------------------------------------

ggplot(data = hip_IA) +
  geom_bar(aes(x = guess_hip_task)) +
  ggsave('figures/jobs_in_hip/IA1.png')

ggplot(data = hip_IA) +
  geom_histogram(aes(x = hip_knowppl), binwidth = 5) +
  ggsave('figures/jobs_in_hip/IA2.png')

ggplot(data = hip_IA) +
  geom_bar(aes(x = hip_knowppl_like)) +
  ggsave('figures/jobs_in_hip/IA3.png')

ggplot(data = hip_IA) +
  geom_histogram(aes(x = guess_hip_turnover), binwidth = 5) +
  ggsave('figures/jobs_in_hip/IA5.png')

ggplot(data = hip_IA) +
  geom_bar(aes(x = guess_hip_turnover_sure)) +
  ggsave('figures/jobs_in_hip/IA5s.png')


# IB (morning) ------------------------------------------------------------

IB_names <- dict %>%
  filter(subcategory == 'IB_m') %>%
  pull(name)

## Cleaning ---------------------------------------------------------------

### Recoding --------------------------------------------------------------

hip_IB <- hip %>%
  select(all_of(IB_names)) %>%
  mutate(across(IB_names[seq(1, 13, 2)], as.integer)) %>%
  mutate(across(IB_names[seq(2, 14, 2)], as.factor)) %>%
  mutate(across(IB_names[seq(2, 14, 2)], ~recode(.,
                                                 `0` = 'Very sure',
                                                 `1` = 'Slightly sure',
                                                 `2` = 'Slightly not sure',
                                                 `3` = 'Not sure at all')))


### Trim errors -------------------------------------------------------------

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

## Figures ----------------------------------------------------------------

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_hour), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB1.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_hour_sure)) +
  ggsave('figures/jobs_in_HIP/IB1s.png')

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_day), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB2.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_day_sure)) +
  ggsave('figures/jobs_in_HIP/IB2s.png')

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_extra), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB3.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_extra_sure)) +
  ggsave('figures/jobs_in_HIP/IB3s.png')

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_night), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB4.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_night_sure)) +
  ggsave('figures/jobs_in_HIP/IB4s.png')

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_transp), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB5.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_transp_sure)) +
  ggsave('figures/jobs_in_HIP/IB5s.png')

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_lunch), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB6.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_lunch_sure)) +
  ggsave('figures/jobs_in_HIP/IB6s.png')

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_attend), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB7.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_attend_sure)) +
  ggsave('figures/jobs_in_HIP/IB7s.png')


# IC (morning) ------------------------------------------------------------

IC_names <- dict %>%
  filter(subcategory == 'IC_m') %>%
  pull(name)

## Cleaning ---------------------------------------------------------------

### Recoding --------------------------------------------------------------

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

### Trim errors -----------------------------------------------------------

hip_IC <- hip_IC %>%
  mutate(guess_entry_pct = replace(guess_entry_pct,
                                   which(guess_entry_pct > 100L),
                                   NA))

## Figures ----------------------------------------------------------------

ggplot(data = hip_IC) +
  geom_histogram(aes(x = guess_entry_salary), binwidth = 500) +
  ggsave('figures/jobs_in_HIP/IC1.png')

ggplot(data = hip_IC) +
  geom_bar(aes(x = guess_entry_salary_sure)) +
  ggsave('figures/jobs_in_HIP/IC1s.png')

ggplot(data = hip_IC) +
  geom_histogram(aes(x = guess_entry_salary_6m), binwidth = 500) +
  ggsave('figures/jobs_in_HIP/IC2.png')

ggplot(data = hip_IC) +
  geom_bar(aes(x = guess_entry_salary_6m_sure)) +
  ggsave('figures/jobs_in_HIP/IC2s.png')

ggplot(data = hip_IC) +
  geom_histogram(aes(x = guess_entry_pct), binwidth = 10) +
  ggsave('figures/jobs_in_HIP/IC3.png')

ggplot(data = hip_IC) +
  geom_bar(aes(x = guess_entry_pct_sure)) +
  ggsave('figures/jobs_in_HIP/IC3s.png')

ggplot(data = hip_IC) +
  geom_bar(aes(x = guess_entry_pct_you)) +
  ggsave('figures/jobs_in_HIP/IC4.png')

ggplot(data = hip_IC) +
  geom_histogram(aes(x = guess_you_salary_1m), binwidth = 500) +
  ggsave('figures/jobs_in_HIP/IC5.png')


# ID (morning) ------------------------------------------------------------

ID_names <- dict %>%
  filter(subcategory == 'ID_m') %>%
  pull(name)

## Cleaning ---------------------------------------------------------------

### Recoding --------------------------------------------------------------

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

### Trim errors -----------------------------------------------------------

hip_ID <- hip_ID %>%
  mutate(guess_promote_medium = replace(guess_promote_medium,
                                        which(guess_promote_medium > 100L),
                                        NA)) %>%
  mutate(guess_promote_sp = replace(guess_promote_sp,
                                    which(guess_promote_sp > 100L),
                                    NA)) %>%
  mutate(guess_salary_sp = replace(guess_salary_sp,
                                   which(guess_salary_sp > 50000L),
                                   NA))

## Figures ----------------------------------------------------------------

ggplot(data = hip_ID) +
  geom_histogram(aes(x = guess_promote_medium), binwidth = 10) +
  ggsave('figures/jobs_in_HIP/ID1.png')

ggplot(data = hip_ID) +
  geom_bar(aes(x = guess_promote_medium_sure)) +
  ggsave('figures/jobs_in_HIP/ID1s.png')

ggplot(data = hip_ID) +
  geom_histogram(aes(x = guess_promote_sp), binwidth = 10) +
  ggsave('figures/jobs_in_HIP/ID2.png')

ggplot(data = hip_ID) +
  geom_bar(aes(x = guess_promote_sp_sure)) +
  ggsave('figures/jobs_in_HIP/ID2s.png')

ggplot(data = hip_ID) +
  geom_histogram(aes(x = guess_salary_medium), binwidth = 500) +
  ggsave('figures/jobs_in_HIP/ID3.png')

ggplot(data = hip_ID) +
  geom_bar(aes(x = guess_salary_medium_sure)) +
  ggsave('figures/jobs_in_HIP/ID3s.png')

ggplot(data = hip_ID) +
  geom_histogram(aes(x = guess_salary_sp), binwidth = 500) +
  ggsave('figures/jobs_in_HIP/ID4.png')

ggplot(data = hip_ID) +
  geom_bar(aes(x = guess_salary_sp_sure)) +
  ggsave('figures/jobs_in_HIP/ID4s.png')

ggplot(data = hip_ID) +
  geom_bar(aes(x = guess_you_promote_medium)) +
  ggsave('figures/jobs_in_HIP/ID5.png')

ggplot(data = hip_ID) +
  geom_bar(aes(x = guess_you_promote_sp)) +
  ggsave('figures/jobs_in_HIP/ID6.png')

ggplot(data = hip_ID) +
  geom_histogram(aes(x = guess_you_salary_6m), binwidth = 500) +
  ggsave('figures/jobs_in_HIP/ID7.png')


# IT (morning) ------------------------------------------------------------

# need to construct 2 new vars!!

IT_names <- dict %>%
  filter(subcategory == 'IT_m') %>%
  pull(name)

## Cleaning ---------------------------------------------------------------

### Recoding --------------------------------------------------------------

hip_IT <- hip %>%
  select(all_of(IT_names)) %>%
  mutate(across(all_of(IT_names), as.logical))

## Figures ----------------------------------------------------------------

IT_codes <- dict %>%
  filter(subcategory == 'IT_m') %>%
  pull(code)

for (i in 1:10) {
  temp <- ggplot(data = hip_IT) +
    geom_bar(aes_string(x = IT_names[i]))

  ggsave(plot = temp, file = paste0('figures/jobs_in_hip/', IT_codes[i], '.png'))
}




