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
  ggsave('figures/IA1.png')

ggplot(data = hip_IA) +
  geom_histogram(aes(x = hip_knowppl), binwidth = 5) +
  ggsave('figures/IA2.png')

ggplot(data = hip_IA) +
  geom_bar(aes(x = hip_knowppl_like)) +
  ggsave('figures/IA3.png')

ggplot(data = hip_IA) +
  geom_histogram(aes(x = guess_hip_turnover), binwidth = 5) +
  ggsave('figures/IA5.png')

ggplot(data = hip_IA) +
  geom_bar(aes(x = guess_hip_turnover_sure)) +
  ggsave('figures/IA5s.png')


