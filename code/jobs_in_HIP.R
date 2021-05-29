library(tidyverse)
library(readxl)

setwd('C:/Users/myzha/Documents/GitHub/HIP')

hip <- read_excel('data/weekdayend_all.xlsx')

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




#### ID ####








