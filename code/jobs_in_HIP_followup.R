library(tidyverse)
library(readxl)

setwd('C:/Users/myzha/Documents/GitHub/HIP')

hip <- read_excel('data/weekdayend_all.xlsx')
hip2 <- read_excel('data/weekdayend_all2.xlsx')

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

w_ID <- c('w_guess_promote_medium',
          'w_guess_promote_medium_sure',
          'w_guess_promote_sp',
          'w_guess_promote_sp_sure',
          'w_guess_salary_medium',
          'w_guess_salary_medium_sure',
          'w_guess_salary_sp',
          'w_guess_salary_sp_sure',
          'w_guess_you_promote_medium',
          'w_guess_you_promote_sp',
          'w_guess_you_salary_6m')

hip_w_ID <- hip %>%
  select(all_of(w_ID)) %>%
  mutate(across(w_ID[c(1, 3, 5, 7, 11)], as.integer)) %>%
  mutate(across(w_ID[c(2, 4, 6, 8, 9, 10)], as.factor)) %>%
  mutate(across(w_ID[c(2, 4, 6, 8)], ~recode(.,
                                             `0` = 'very sure',
                                             `1` = 'slightly sure',
                                             `2` = 'slightly not sure',
                                             `3` = 'not sure at all'))) %>%
  mutate(across(w_ID[9:10], ~recode(.,
                                    `0` = 'likely',
                                    `1` = 'somewhat likely',
                                    `2` = 'somewhat unlikely',
                                    `3` = 'very unlikely')))

ggplot(data = hip_w_ID) +
  geom_histogram(aes(x = w_guess_promote_medium), binwidth = 10) +
  ggsave('figures/jobs_in_HIP_followup/w_ID1.png')

ggplot(data = hip_w_ID) +
  geom_bar(aes(x = w_guess_promote_medium_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_ID1s.png')

ggplot(data = hip_w_ID) +
  geom_histogram(aes(x = w_guess_promote_sp), binwidth = 10) +
  ggsave('figures/jobs_in_HIP_followup/w_ID2.png')

ggplot(data = hip_w_ID) +
  geom_bar(aes(x = w_guess_promote_sp_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_ID2s.png')

ggplot(data = hip_w_ID) +
  geom_histogram(aes(x = w_guess_salary_medium), binwidth = 500) +
  ggsave('figures/jobs_in_HIP_followup/w_ID3.png')

ggplot(data = hip_w_ID) +
  geom_bar(aes(x = w_guess_salary_medium_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_ID3s.png')

ggplot(data = hip_w_ID) +
  geom_histogram(aes(x = w_guess_salary_sp), binwidth = 500) +
  ggsave('figures/jobs_in_HIP_followup/w_ID4.png')

ggplot(data = hip_w_ID) +
  geom_bar(aes(x = w_guess_salary_sp_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_ID4s.png')

ggplot(data = hip_w_ID) +
  geom_bar(aes(x = w_guess_you_promote_medium)) +
  ggsave('figures/jobs_in_HIP_followup/w_ID5.png')

ggplot(data = hip_w_ID) +
  geom_bar(aes(x = w_guess_you_promote_sp)) +
  ggsave('figures/jobs_in_HIP_followup/w_ID6.png')

ggplot(data = hip_w_ID) +
  geom_histogram(aes(x = w_guess_you_salary_6m), binwidth = 500) +
  ggsave('figures/jobs_in_HIP_followup/w_ID7.png')


#### IA ####

w_IA <- c('interact_roommate',
          'interact_coworkers',
          'interact_coworkers_out',
          'interact_coworkers_wel',
          'interact_coworkers_salary',
          'interact_supervisor',
          'interact_supervisor_wel',
          'interact_supervisor_salary',
          'interact_hrmanager',
          'interact_hrmanager_wel',
          'interact_hrmanager_salary',
          'interact_manager',
          'interact_manager_wel',
          'interact_manager_salary',
          'interact_break')

hip_w_IA <- hip %>%
  select(all_of(w_IA)) %>%
  mutate(across(all_of(w_IA), as.factor)) %>%
  mutate(across(c(1, 2, 3, 5, 6, 8, 9, 11, 12, 14), ~recode(.,
                                                            `0` = 'frequently every day',
                                                            `1` = 'sometimes every day',
                                                            `2` = 'once every day',
                                                            `3` = 'a few times a week',
                                                            `4` = 'once a week',
                                                            `6` = 'never',
                                                            `-8` = "I'm not sure"))) %>%
  mutate(across(c(1, 2, 3, 5, 6, 8, 9, 11, 12, 14), ~fct_relevel(.,
                                                                 "I'm not sure",
                                                                 after = Inf))) %>%
  mutate(across(c(4, 7, 10, 13), ~recode(.,
                                         `0` = 'not welcomed at all',
                                         `1` = 'somewhat not welcomed',
                                         `2` = 'somewhat welcomed',
                                         `3` = 'very welcomed'))) %>%
  mutate(interact_break = recode(interact_break,
                                 `0` = 'no',
                                 `1` = 'yes',
                                 `100` = 'not sure'))

w_IA_codes <- c('w_IA0', 'w_IA1', 'w_IA2', 'w_IA2n', 'w_IA2o', 'w_IA3', 'w_IA3n', 'w_IA3o', 'w_IA4', 'w_IA4n', 'w_IA4o', 'w_IA5', 'w_IA5n', 'w_IA5o', 'w_IA6')

for (i in 1:15) {
  temp <- ggplot(data = hip_w_IA) +
    geom_bar(aes_string(x = w_IA[i]))
  
  ggsave(plot = temp, file = paste0('figures/jobs_in_HIP_followup/', w_IA_codes[i], '.png'))
}





