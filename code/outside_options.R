library(tidyverse)
library(readxl)

setwd('C:/Users/myzha/Documents/GitHub/HIP')

hip <- read_excel('data/weekdayend_all.xlsx')

#### OA ####

OA <- c('guess_out_salary',
        'guess_out_hour',
        'guess_out_day',
        'guess_out_extra',
        'guess_out_night',
        'guess_out_transp',
        'guess_out_lunch',
        'guess_out_attend',
        'minimal_salary_a',
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

hip_OA <- hip %>% 
  select(all_of(OA)) %>% 
  mutate(across(OA[1:5], as.integer)) %>%
  mutate(across(OA[6:8], as.factor)) %>%
  mutate(across(OA[6:8], ~recode(.,
                                 `0` = 'likely',
                                 `1` = 'somewhat likely',
                                 `2` = 'somewhat unlikely',
                                 `3` = 'very unlikely')))

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_salary), binwidth = 500) +
  ggsave('figures/OA1.png')

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_hour), binwidth = 1) +
  ggsave('figures/OA2.png')

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_day), binwidth = 1) +
  ggsave('figures/OA3.png')

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_extra), binwidth = 1) +
  ggsave('figures/OA4.png')

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_night), binwidth = 1) +
  ggsave('figures/OA4b.png')

ggplot(data = hip_OA) +
  geom_bar(aes(x = guess_out_transp)) +
  ggsave('figures/OA5.png')

ggplot(data = hip_OA) +
  geom_bar(aes(x = guess_out_lunch)) +
  ggsave('figures/OA6.png')

ggplot(data = hip_OA) +
  geom_bar(aes(x = guess_out_attend)) +
  ggsave('figures/OA7.png')


#### OB ####

OB <- c('guess_out_salary_1y',
        'guess_out_salary_1y_com',
        'guess_out_promote',
        'guess_out_salary_super',
        'guess_out_promote_com')

hip_OB <- hip %>%
  select(all_of(OB)) %>%
  mutate(across(OB[c(1, 3, 4)], as.integer)) %>%
  mutate(across(OB[c(2, 5)], as.factor)) %>%
  mutate(across(OB[c(2, 5)], ~recode(.,
                                     `0` = 'no',
                                     `1` = 'yes',
                                     `100` = 'not sure')))

ggplot(data = hip_OB) +
  geom_histogram(aes(x = guess_out_salary_1y), binwidth = 500) +
  ggsave('figures/OB2.png')

ggplot(data = hip_OB) +
  geom_bar(aes(x = guess_out_salary_1y_com)) +
  ggsave('figures/OB2c.png')

ggplot(data = hip_OB) +
  geom_histogram(aes(x = guess_out_promote), binwidth = 10) +
  ggsave('figures/OB3.png')

ggplot(data = hip_OB) +
  geom_histogram(aes(x = guess_out_salary_super), binwidth = 500) +
  ggsave('figures/OB4.png')

ggplot(data = hip_OB) +
  geom_bar(aes(x = guess_out_promote_com)) +
  ggsave('figures/OB4c.png')


#### OC ####

OC <- c('jobaspect_first',
        'jobaspect_first_other',
        'jobaspect_second',
        'jobaspect_second_other',
        'jobaspect_third',
        'jobaspect_third_other')

hip_OC <- hip %>%
  select(all_of(OC)) %>%
  mutate(across(OC[c(1, 3, 5)], as.factor)) %>%
  mutate(across(OC[c(1, 3, 5)], ~recode(.,
                                        `10` = 'salary as an entry-level worker in the first month',
                                        `11` = 'salary as an entry-level worker after 6 months',
                                        `12` = 'chance of promotion to a higher level after 6 months',
                                        `13` = 'salary in the higher level',
                                        `2` = 'provide good work benefit',
                                        `3` = 'reasonable work hours',
                                        `4` = 'the task is interesting',
                                        `5` = 'skill development',
                                        `6` = 'good management'#, `100` = 'others'
                                        ))) %>%
  mutate(across(OC[c(1, 3, 5)], ~fct_relevel(.,
                                             'salary as an entry-level worker in the first month',
                                             'salary as an entry-level worker after 6 months',
                                             'chance of promotion to a higher level after 6 months',
                                             'salary in the higher level',
                                             'provide good work benefit',
                                             'reasonable work hours',
                                             'the task is interesting',
                                             'skill development',
                                             'good management'#, 'others'
                                             ))) %>%
  mutate(across(OC[c(2, 4, 6)], as.character))

ggplot(data = hip_OC) +
  geom_bar(aes(x = jobaspect_first)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  ggsave('figures/OC1i.png')

ggplot(data = hip_OC) +
  geom_bar(aes(x = jobaspect_second)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  ggsave('figures/OC1ii.png')

ggplot(data = hip_OC) +
  geom_bar(aes(x = jobaspect_third)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  ggsave('figures/OC1iii.png')















