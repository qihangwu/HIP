library(tidyverse)
library(readxl)

setwd('C:/Users/myzha/Documents/GitHub/HIP')

hip <- read_excel('data/weekdayend_all.xlsx')

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
  geom_histogram(aes(x = guess_out_salary), binwidth = 500)

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_hour), binwidth = 1)

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_day), binwidth = 1)

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_extra), binwidth = 1)

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_night), binwidth = 1)

ggplot(data = hip_OA) +
  geom_bar(aes(x = guess_out_transp))

ggplot(data = hip_OA) +
  geom_bar(aes(x = guess_out_lunch))

ggplot(data = hip_OA) +
  geom_bar(aes(x = guess_out_attend))











