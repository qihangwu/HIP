library(tidyverse)
library(readxl)

setwd('C:/Users/myzha/Documents/GitHub/HIP')

hip <- read_excel('data/weekdayend_all.xlsx')

#### IB ####

w_IB <- c('w_guess_hip_hour',
          'w_guess_hip_hour_sure',
          'w_guess_hip_day',
          'w_guess_hip_day_sure',
          'w_guess_hip_extra',
          'w_guess_hip_extra_sure',
          'w_guess_hip_night',
          'w_guess_hip_night_sure',
          'w_guess_hip_transp',
          'w_guess_hip_transp_sure',
          'w_guess_hip_lunch',
          'w_guess_hip_lunch_sure',
          'w_guess_hip_attend',
          'w_guess_hip_attend_sure')

hip_w_IB <- hip %>%
  select(all_of(w_IB)) %>%
  mutate(across(w_IB[seq(1, 13, 2)], as.integer)) %>%
  mutate(across(w_IB[seq(2, 14, 2)], as.factor)) %>%
  mutate(across(w_IB[seq(2, 14, 2)], ~recode(.,
                                             `0` = 'very sure',
                                             `1` = 'slightly sure',
                                             `2` = 'slightly not sure',
                                             `3` = 'not sure at all')))

w_IB_codes <- c('IB1', 'IB1s', 'IB2', 'IB2s', 'IB3', 'IB3s', 'IB4', 'IB4s', 'IB5', 'IB5s', 'IB6', 'IB6s', 'IB7', 'IB7s')

for (i in seq(1, 13, 2)) {
  temp <- ggplot(data = hip_w_IB) +
    geom_histogram(aes_string(x = w_IB[i]), binwidth = 1)
  
  ggsave(plot = temp, file = paste0('figures/jobs_in_HIP_followup/w_', w_IB_codes[i], '.png'))
}

for (i in seq(2, 14, 2)) {
  temp <- ggplot(data = hip_w_IB) +
    geom_bar(aes_string(x = w_IB[i]))
  
  ggsave(plot = temp, file = paste0('figures/jobs_in_HIP_followup/w_', w_IB_codes[i], '.png'))
}


#### IC ####

w_IC <- c('w_guess_entry_salary',
          'w_guess_entry_salary_sure',
          'w_guess_entry_salary_6m',
          'w_guess_entry_salary_6m_sure',
          'w_guess_entry_pct',
          'w_guess_entry_pct_sure',
          'w_guess_entry_pct_you',
          'w_guess_you_salary_1m')

hip_w_IC <- hip %>%
  select(all_of(w_IC)) %>%
  mutate(across(w_IC[c(1, 3, 5, 8)], as.integer)) %>%
  mutate(across(w_IC[c(2, 4, 6, 7)], as.factor)) %>%
  mutate(across(w_IC[c(2, 4, 6)], ~recode(.,
                                          `0` = 'very sure',
                                          `1` = 'slightly sure',
                                          `2` = 'slightly not sure',
                                          `3` = 'not sure at all'))) %>%
  mutate(w_guess_entry_pct_you = recode(w_guess_entry_pct_you,
                                        `0` = 'likely',
                                        `1` = 'somewhat likely',
                                        `2` = 'somewhat unlikely',
                                        `3` = 'very unlikely'))

ggplot(data = hip_w_IC) +
  geom_histogram(aes(x = w_guess_entry_salary), binwidth = 500) +
  ggsave('figures/jobs_in_HIP_followup/w_IC1.png')

ggplot(data = hip_w_IC) +
  geom_bar(aes(x = w_guess_entry_salary_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_IC1s.png')

ggplot(data = hip_w_IC) +
  geom_histogram(aes(x = w_guess_entry_salary_6m), binwidth = 500) +
  ggsave('figures/jobs_in_HIP_followup/w_IC2.png')

ggplot(data = hip_w_IC) +
  geom_bar(aes(x = w_guess_entry_salary_6m_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_IC2s.png')

ggplot(data = hip_w_IC) +
  geom_histogram(aes(x = w_guess_entry_pct), binwidth = 10) +
  ggsave('figures/jobs_in_HIP_followup/w_IC3.png')

ggplot(data = hip_w_IC) +
  geom_bar(aes(x = w_guess_entry_pct_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_IC3s.png')

ggplot(data = hip_w_IC) +
  geom_bar(aes(x = w_guess_entry_pct_you)) +
  ggsave('figures/jobs_in_HIP_followup/w_IC4.png')

ggplot(data = hip_w_IC) +
  geom_histogram(aes(x = w_guess_you_salary_1m), binwidth = 500) +
  ggsave('figures/jobs_in_HIP_followup/w_IC5.png')


#### ID ####




#### IA ####
