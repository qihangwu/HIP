library(tidyverse)
library(readxl)
library(ggplot2)
setwd("~/HIP")

data = read_excel('weekdayend_all.xlsx') 

#### DM_e (Evening) ####
### Generate two variables of Amharic and Sidamaigna and 3rd variable of non-Amharic 

data$lang_sidama = ifelse(data$language_0, "1", "0")
data$lang_amhar = ifelse(data$language_5, "1", "0")
data$lang_other = 


DM_e = c('lang_sidama',
         'lang_amhar',
         'language_other',
         'religion',
         'religion_other',
         'current_live_region',
         'current_live_city',
         'current_live_woreda',
         'current_live_kebele',
         'origin_region',
         'origin_city',
         'origin_woreda',
         'origin_kebele')

data_DM = data %>%
  select(all_of(DM_e)) %>%
  mutate(across(DM_e[c(1:5)], as.factor)) %>%
  mutate(across(DM_e[c()], as.character)) %>%
  mutate(across(DM_e[c(1:13)], ~recode(.,
                     `0` = 'No',
                     `1` = 'Yes'))) %>%
  mutate(religion = recode(religion,
                               `0` = 'Protestant',
                               `1` = 'Orthodox',
                               `2` = 'Catholic',
                               `3` = 'Muslim',
                               `4` = 'Traditional religions',
                               `100` = 'Others')) 
str(data_DM)

ggplot(data = data_DM) +
  geom_bar(aes(x = language_0)) +
  ggsave('DM4a.png')

ggplot(data = data_DM) +
  geom_bar(aes(x = language_5)) +
  ggsave('DM4b.png')

ggplot(data = data_DM) +
  geom_bar(aes(x = religion)) +
  ggsave('DM5.png')


#### WF (Evening) ####

## Job way, pick most important ones and create around 3 or more main variables like the language variable

WF = c('search_job_way_0',
         'search_job_way_1',
         'search_job_way_2',
         'search_job_way_3',
         'search_job_way_4',
         'search_job_way_5',
         'search_job_way_6',
         'search_job_way_7',
         'search_job_way_8',
         'search_job_way_other',
         'search_job_day',
         'search_job_hour',
         'search_job_min',
         'income_total',
         'income_from_family',
         'income_from_friend',
         'income_to_family',
         'expend_food',
         'expend_transp',
         'expend_rent',
         'expend_cosmetic',
         'health_nofood',
         'health_ill')

data_WF = data %>%
  select(all_of(WF)) %>%
  mutate(across(WF[c(1:10, 17, 22, 23 )], as.factor)) %>%
  mutate(across(WF[c(11:13)], as.integer)) %>%
  mutate(across(WF[c(1:10, 22, 23)], ~recode(.,
                                       `0` = 'No',
                                       `1` = 'Yes'))) %>%
  mutate(income_to_family = recode(income_to_family,
                                         `0` = 'No',
                                         `1` = 'Yes',
                                        `100` = 'Not Sure'))

str(data_WF)

ggplot(data = data_WF) +
  geom_bar(aes(x = search_job_way_5)) +
  ggsave('WF1_5.png')

ggplot(data = data_WF) +
  geom_bar(aes(x = search_job_way_6)) +
  ggsave('WF1_6.png')

ggplot(data = data_WF) +
  geom_histogram(aes(x = search_job_day), binwidth = 1) +
  ggsave('WF1b.png')

ggplot(data = data_WF) +
  geom_histogram(aes(x = search_job_hour), binwidth = 5) +
  ggsave('WF1c.png')

ggplot(data = data_WF) +
  geom_histogram(aes(x = search_job_min), binwidth = 1) +
  ggsave('WF1d.png')

ggplot(data = data_WF) +
  geom_histogram(aes(x = income_total), binwidth = 1000) +
  ggsave('WF2.png')

ggplot(data = data_WF) +
  geom_histogram(aes(x = income_from_family), binwidth = 1000) +
  ggsave('WF3.png')

ggplot(data = data_WF) +
  geom_histogram(aes(x = income_from_friend), binwidth = 100) +
  ggsave('WF4.png')

ggplot(data = data_WF) +
  geom_bar(aes(x = income_to_family), binwidth = 1000) +
  ggsave('WF5.png')

ggplot(data = data_WF) +
  geom_histogram(aes(x = expend_food), binwidth = 100) +
  ggsave('WF6.png')

ggplot(data = data_WF) +
  geom_histogram(aes(x = expend_transp), binwidth = 50) +
  ggsave('WF7.png')

ggplot(data = data_WF) +
  geom_histogram(aes(x = expend_rent), binwidth = 100) +
  ggsave('WF8.png')

ggplot(data = data_WF) +
  geom_histogram(aes(x = expend_cosmetic), binwidth = 50) +
  ggsave('WF9.png')

ggplot(data = data_WF) +
  geom_bar(aes(x = health_nofood)) +
  ggsave('WF10.png')

ggplot(data = data_WF) +
  geom_bar(aes(x = health_ill)) +
  ggsave('WF11.png')


#### JH (Evening) ####

JH = c('history_yesno',
       'history_age',
       'history_manual',
       'history_salary',
       'history_hour',
       'history_day',
       'history_quitreason',
       'history_quitreason_other')

data_JH = data %>%
  select(all_of(JH)) %>%
  mutate(across(JH[c(1, 3, 7)], as.factor)) %>%
  mutate(across(JH[c(5, 6)], as.integer)) %>%
  mutate(across(JH[c(7)], as.character)) %>%
  mutate(across(JH[c(1, 3)], ~recode(.,
                                             `0` = 'No',
                                             `1` = 'Yes'))) %>%
  mutate(history_quitreason = recode(history_quitreason,
                                   `0` = 'Because the firm fired me.',
                                   `1` = 'Because the job was not interesting.',
                                   `2` = 'Because the job did not pay well.',
                                   `3` = 'Because the job was very tiring',
                                   `5` = 'Because the job did not provide good benefits.',
                                   `6` = 'Because the employer treated me badly.',
                                  `100` = 'Others'))

str(data_JH) 

ggplot(data = data_JH) +
  geom_bar(aes(x = history_yesno)) +
  ggsave('JH1.png')

ggplot(data = data_JH) +
  geom_histogram(aes(x = history_age), binwidth = 1) +
  ggsave('JH2.png')

ggplot(data = data_JH) +
  geom_bar(aes(x = history_manual)) +
  ggsave('JH3a.png')

ggplot(data = data_JH) +
  geom_histogram(aes(x = history_salary), binwidth = 100) +
  ggsave('JH4.png')

ggplot(data = data_JH) +
  geom_histogram(aes(x = history_hour), binwidth = 1) +
  ggsave('JH5.png')

ggplot(data = data_JH) +
  geom_histogram(aes(x = history_day), binwidth = 1) +
  ggsave('JH6.png')

ggplot(data = data_JH) +
  geom_bar(aes(x = history_quitreason)) +
  ggsave('JH7.png')


