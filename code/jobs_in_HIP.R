library(tidyverse)
library(readxl)

setwd('C:/Users/myzha/Documents/GitHub/HIP')

hip <- read_excel('data/weekdayend_all.xlsx')
hip2 <- read_excel('data/weekdayend_all2.xlsx')

#### IA ####

IA <- c('guess_hip_task',
        'guess_hip_task_other',
        'hip_knowppl',
        'hip_knowppl_like',
        'guess_hip_turnover',
        'guess_hip_turnover_sure')

hip_IA <- hip %>%
  select(all_of(IA)) %>%
  mutate(across(IA[c(1, 4, 6)], as.factor)) %>%
  mutate(guess_hip_task = recode(guess_hip_task,
                                 `0` = 'sewing',
                                 `1` = 'quality check',
                                 `2` = 'team leaders',
                                 `3` = 'supervisors'#, `100` = 'others'
                                 )) %>%
  mutate(hip_knowppl_like = recode(hip_knowppl_like,
                                   `0` = 'almost all of them',
                                   `1` = 'some of them',
                                   `2` = 'very few of them',
                                   `3` = 'almost none of them')) %>%
  mutate(guess_hip_turnover_sure = recode(guess_hip_turnover_sure,
                                          `0` = 'very sure',
                                          `1` = 'slightly sure',
                                          `2` = 'slightly not sure',
                                          `3` = 'not sure at all')) %>%
  mutate(guess_hip_task_other = as.character(guess_hip_task_other)) %>%
  mutate(across(IA[c(3, 5)], as.integer))

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


#### IB ####

IB <- c('guess_hip_hour',
        'guess_hip_hour_sure',
        'guess_hip_day',
        'guess_hip_day_sure',
        'guess_hip_extra',
        'guess_hip_extra_sure',
        'guess_hip_night',
        'guess_hip_night_sure',
        'guess_hip_transp',
        'guess_hip_transp_sure',
        'guess_hip_lunch',
        'guess_hip_lunch_sure',
        'guess_hip_attend',
        'guess_hip_attend_sure')

hip_IB <- hip %>%
  select(all_of(IB)) %>%
  mutate(across(IB[seq(1, 13, 2)], as.integer)) %>%
  mutate(across(IB[seq(2, 14, 2)], as.factor)) %>%
  mutate(across(IB[seq(2, 14, 2)], ~recode(.,
                                           `0` = 'very sure',
                                           `1` = 'slightly sure',
                                           `2` = 'slightly not sure',
                                           `3` = 'not sure at all')))

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


#### IC ####

IC <- c('guess_entry_salary',
        'guess_entry_salary_sure',
        'guess_entry_salary_6m',
        'guess_entry_salary_6m_sure',
        'guess_entry_pct',
        'guess_entry_pct_sure',
        'guess_entry_pct_you',
        'guess_you_salary_1m')

hip_IC <- hip %>%
  select(all_of(IC)) %>%
  mutate(across(IC[c(1, 3, 5, 8)], as.integer)) %>%
  mutate(across(IC[c(2, 4, 6, 7)], as.factor)) %>%
  mutate(across(IC[c(2, 4, 6)], ~recode(.,
                                        `0` = 'very sure',
                                        `1` = 'slightly sure',
                                        `2` = 'slightly not sure',
                                        `3` = 'not sure at all'))) %>%
  mutate(guess_entry_pct_you = recode(guess_entry_pct_you,
                                      `0` = 'likely',
                                      `1` = 'somewhat likely',
                                      `2` = 'somewhat unlikely',
                                      `3` = 'very unlikely'))

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

#### ID ####

ID <- c('guess_promote_medium',
        'guess_promote_medium_sure',
        'guess_promote_sp',
        'guess_promote_sp_sure',
        'guess_salary_medium',
        'guess_salary_medium_sure',
        'guess_salary_sp',
        'guess_salary_sp_sure',
        'guess_you_promote_medium',
        'guess_you_promote_sp',
        'guess_you_salary_6m')

hip_ID <- hip %>%
  select(all_of(ID)) %>%
  mutate(across(ID[c(1, 3, 5, 7, 11)], as.integer)) %>%
  mutate(across(ID[c(2, 4, 6, 8, 9, 10)], as.factor)) %>%
  mutate(across(ID[c(2, 4, 6, 8)], ~recode(.,
                                           `0` = 'very sure',
                                           `1` = 'slightly sure',
                                           `2` = 'slightly not sure',
                                           `3` = 'not sure at all'))) %>%
  mutate(across(ID[9:10], ~recode(.,
                                  `0` = 'likely',
                                  `1` = 'somewhat likely',
                                  `2` = 'somewhat unlikely',
                                  `3` = 'very unlikely')))

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


#### IT ####

IT <- c('info_entry_choice1',
        'info_entry_choice2',
        'info_entry_choice3',
        'info_entry_choice4',
        'info_entry_choice5',
        'info_promote_choice1',
        'info_promote_choice2',
        'info_promote_choice3',
        'info_promote_choice4',
        'info_promote_choice5')

hip_IT <- hip %>%
  select(all_of(IT)) %>%
  mutate(across(all_of(IT), as.logical))

IT_codes <- c('IT1a', 'IT1b', 'IT1c', 'IT1d', 'IT1e', 'IT2a', 'IT2b', 'IT2c', 'IT2d', 'IT2e')

for (i in 1:10) {
  temp <- ggplot(data = hip_IT) +
    geom_bar(aes_string(x = IT[i]))
  
  ggsave(plot = temp, file = paste0('figures/jobs_in_hip/', IT_codes[i], '.png'))
}









