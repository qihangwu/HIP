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

data$tot_score_WM1 = data$series11_yesno + data$series12_yesno + data$series13_yesno + data$series14_yesno + data$series15_yesno + 
  data$series16_yesno + data$series17_yesno

WM1 = c('series11_yesno',
        'series12_yesno',
        'series13_yesno',
        'series14_yesno',
        'series15_yesno',
        'series16_yesno',
        'series17_yesno',
        'tot_score_WM1')

data_WM1 = data %>%
  select(all_of(WM1)) %>%
  mutate(across(WM1[c(1:7)], as.factor)) %>%
  mutate(across(WM1[c(8)], as.integer)) %>%
  mutate(across(WM1[c(1:7)], ~recode(.,
                                     `0` = 'Incorrect',
                                     `1` = 'Correct'))) 

min(data$tot_score_WM1, na.rm=T)
max(data$tot_score_WM1, na.rm=T)

str(data_WM1) 


ggplot(data = data_WM1) +
  geom_bar(aes(x = series11_yesno)) +
  ggsave('WM11.png')

ggplot(data = data_WM1) +
  geom_bar(aes(x = series12_yesno)) +
  ggsave('WM2.png')

ggplot(data = data_WM1) +
  geom_bar(aes(x = series13_yesno)) +
  ggsave('WM3.png')

ggplot(data = data_WM1) +
  geom_bar(aes(x = series14_yesno)) +
  ggsave('WM4.png')

ggplot(data = data_WM1) +
  geom_bar(aes(x = series15_yesno)) +
  ggsave('WM5.png')

ggplot(data = data_WM1) +
  geom_bar(aes(x = series16_yesno)) +
  ggsave('WM6.png')

ggplot(data = data_WM1) +
  geom_bar(aes(x = series17_yesno)) +
  ggsave('WM7.png')

data$tot_score_WM2 = data$series21_yesno + data$series22_yesno + data$series23_yesno + data$series24_yesno + data$series25_yesno + 
  data$series26_yesno + data$series27_yesno

WM2 = c('series21_yesno',
        'series22_yesno',
        'series23_yesno',
        'series24_yesno',
        'series25_yesno',
        'series26_yesno',
        'series27_yesno',
        'tot_score_WM2')

data_WM2 = data %>%
  select(all_of(WM2)) %>%
  mutate(across(WM2[c(1:7)], as.factor)) %>%
  mutate(across(WM2[c(1:7)], ~recode(.,
                                     `0` = 'Incorrect',
                                     `1` = 'Correct'))) 

min(data$tot_score_WM2, na.rm=T)
max(data$tot_score_WM2, na.rm=T)

ggplot(data = data_WM2) +
  geom_bar(aes(x = series21_yesno)) +
  ggsave('WM21.png')

ggplot(data = data_WM2) +
  geom_bar(aes(x = series22_yesno)) +
  ggsave('WM22.png')

ggplot(data = data_WM2) +
  geom_bar(aes(x = series23_yesno)) +
  ggsave('WM23.png')

ggplot(data = data_WM2) +
  geom_bar(aes(x = series24_yesno)) +
  ggsave('WM24.png')

ggplot(data = data_WM2) +
  geom_bar(aes(x = series25_yesno)) +
  ggsave('WM25.png')

ggplot(data = data_WM2) +
  geom_bar(aes(x = series26_yesno)) +
  ggsave('WM26.png')

ggplot(data = data_WM2) +
  geom_bar(aes(x = series27_yesno)) +
  ggsave('WM27.png')


data$tot_score_WM3 = data$series31_yesno + data$series32_yesno + data$series33_yesno + data$series34_yesno + data$series35_yesno + 
  data$series36_yesno + data$series37_yesno

WM3 = c('series31_yesno',
        'series32_yesno',
        'series33_yesno',
        'series34_yesno',
        'series35_yesno',
        'series36_yesno',
        'series37_yesno',
        'tot_score_WM3')
data_WM3__norm = function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

norm_WM3 = as.data.frame(apply(data[1:7], 2, data_WM3__norm))


data_WM3 = data %>%
  select(all_of(WM3)) %>%
  mutate(across(WM3[c(1:7)], as.factor)) %>%
  mutate(across(WM3[c(1:7)], ~recode(.,
                                     `0` = 'Incorrect',
                                     `1` = 'Correct'))) 

min(data$tot_score_WM3, na.rm=T)
max(data$tot_score_WM3, na.rm=T)

ggplot(data = data_WM3) +
  geom_bar(aes(x = series31_yesno)) +
  ggsave('WM31.png')

ggplot(data = data_WM3) +
  geom_bar(aes(x = series32_yesno)) +
  ggsave('WM32.png')

ggplot(data = data_WM3) +
  geom_bar(aes(x = series33_yesno)) +
  ggsave('WM33.png')

ggplot(data = data_WM3) +
  geom_bar(aes(x = series34_yesno)) +
  ggsave('WM34.png')

ggplot(data = data_WM3) +
  geom_bar(aes(x = series35_yesno)) +
  ggsave('WM35.png')

ggplot(data = data_WM3) +
  geom_bar(aes(x = series36_yesno)) +
  ggsave('WM36.png')

ggplot(data = data_WM3) +
  geom_bar(aes(x = series37_yesno)) +
  ggsave('WM37.png')
