library(tidyverse)
library(readxl)
library(ggplot2)
setwd("~/HIP")

data = read_excel('weekdayend_all.xlsx') 

#### AT (Evening) ####

# Question regarding AT11 AT12language variable(s)

AT = c('tipi1',
       'tipi2',
       'tipi3',
       'tipi4',
       'tipi5',
       'tipi6',
       'tipi7',
       'tipi8',
       'tipi9',
       'tipi10')

data_AT = data %>%
  select(all_of(AT)) %>%
  mutate(across(AT[c(1:10)], as.factor)) %>%
  mutate(across(AT[c(1:10)], ~recode(.,
                                     `0` = 'Completely Agree',
                                     `1` = 'Somewhat Agree',
                                     `2` = 'Neither agree nor disagree',
                                     `3` = 'Somewhat Disagree',
                                     `4` = 'Completely Disagree'))) 

str(data_AT) 

ggplot(data = data_AT) +
  geom_bar(aes(x = tipi1)) +
  ggsave('AT1.png')

ggplot(data = data_AT) +
  geom_bar(aes(x = tipi2)) +
  ggsave('AT2.png')

ggplot(data = data_AT) +
  geom_bar(aes(x = tipi3)) +
  ggsave('AT3.png')

ggplot(data = data_AT) +
  geom_bar(aes(x = tipi4)) +
  ggsave('AT4.png')

ggplot(data = data_AT) +
  geom_bar(aes(x = tipi5)) +
  ggsave('AT5.png')

ggplot(data = data_AT) +
  geom_bar(aes(x = tipi6)) +
  ggsave('AT6.png')

ggplot(data = data_AT) +
  geom_bar(aes(x = tipi7)) +
  ggsave('AT7.png')

ggplot(data = data_AT) +
  geom_bar(aes(x = tipi8)) +
  ggsave('AT8.png')

ggplot(data = data_AT) +
  geom_bar(aes(x = tipi9)) +
  ggsave('AT9.png')

ggplot(data = data_AT) +
  geom_bar(aes(x = tipi10)) +
  ggsave('AT10.png')


#### WM (Evening) ####

# Question regarding series21_yesno - series37_yesno
WM = c('series11_yesno',
       'series12_yesno',
       'series13_yesno',
       'series14_yesno',
       'series15_yesno',
       'series16_yesno',
       'series17_yesno')

data_WM = data %>%
  select(all_of(WM)) %>%
  mutate(across(WM[c(1:7)], as.factor)) %>%
  mutate(across(WM[c(1:7)], ~recode(.,
                                     `0` = 'Incorrect',
                                     `1` = 'Correct'))) Wm

str(data_WM) 

ggplot(data = data_WM) +
  geom_bar(aes(x = series11_yesno)) +
  ggsave('WM1.png')

ggplot(data = data_WM) +
  geom_bar(aes(x = series12_yesno)) +
  ggsave('WM2.png')

ggplot(data = data_WM) +
  geom_bar(aes(x = series13_yesno)) +
  ggsave('WM3.png')

ggplot(data = data_WM) +
  geom_bar(aes(x = series14_yesno)) +
  ggsave('WM4.png')

ggplot(data = data_WM) +
  geom_bar(aes(x = series15_yesno)) +
  ggsave('WM5.png')

ggplot(data = data_WM) +
  geom_bar(aes(x = series16_yesno)) +
  ggsave('WM6.png')

ggplot(data = data_WM) +
  geom_bar(aes(x = series17_yesno)) +
  ggsave('WM7.png')
