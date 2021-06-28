library(tidyverse)
library(readxl)
library(ggplot2)
library(Factoshiny)
library(FactoMineR)
library(matrixStats)


data = read_excel('weekdayend_all2.xlsx') 

#### A11-A36 (Grading - Behavioral Test (BT)) ####

# Question regarding 1 is correct, zero is wrong

BT = c('a11',
       'a12',
       'a13',
       'a14',
       'a15',
       'a16',
       'a21',
       'a22',
       'a23',
       'a24',
       'a25',
       'a26',
       'a31',
       'a32',
       'a33',
       'a34',
       'a35',
       'a36')


data_BT = data %>%
  select(all_of(BT)) %>%
  mutate(across(BT[c(1:18)], as.factor)) %>%
  mutate(a11 = recode(a11,
                      `0` = '100 birrs today.',
                      `1` = 'Lottery today, 80% chance of 125 birrs, 20% of getting nothing.',
                      `-9` = 'Missing')) %>%
  mutate(a12 = recode(a12,
                      `0` = '100 birrs today.',
                      `1` = 'Lottery today, 70% chance of 143 birrs, 30% of getting nothing.',
                      `-9` = 'Missing')) %>%
  mutate(a13 = recode(a13,
                      `0` = '100 birrs today.',
                      `1` = 'Lottery today, 60% chance of 167 birrs, 40% of getting nothing.',
                      `-9` = 'Missing')) %>%
  mutate(a14 = recode(a14,
                      `0` = '100 birrs today.',
                      `1` = 'Lottery today, 50% chance of 200 birrs, 50% of getting nothing.',
                      `-9` = 'Missing')) %>%
  mutate(a15 = recode(a15,
                      `0` = '100 birrs today.',
                      `1` = 'Lottery today, 40% chance of 250 birrs, 60% of getting nothing.',
                      `-9` = 'Missing')) %>%
  mutate(a16 = recode(a16,
                      `0` = '100 birrs today.',
                      `1` = 'Lottery today, 30% chance of 333 birrs, 70% of getting nothing.',
                      `-9` = 'Missing')) %>%
  mutate(a21 = recode(a21,
                      `0` = '100 birrs today.',
                      `1` = '105 birrs in a year.',
                      `-9` = 'Missing')) %>%
  mutate(a22 = recode(a22,
                      `0` = '100 birrs today.',
                      `1` = '110 birrs in a year.',
                      `-9` = 'Missing')) %>%
  mutate(a23 = recode(a23,
                      `0` = '100 birrs today.',
                      `1` = '120 birrs in a year.',
                      `-9` = 'Missing')) %>%
  mutate(a24 = recode(a24,
                      `0` = '100 birrs today.',
                      `1` = '130 birrs in a year.',
                      `-9` = 'Missing')) %>%  
  mutate(a25 = recode(a25,
                      `0` = '100 birrs today.',
                      `1` = '140 birrs in a year.',
                      `-9` = 'Missing')) %>%
  mutate(a26 = recode(a26,
                      `0` = '100 birrs today.',
                      `1` = '150 birrs in a year.',
                      `-9` = 'Missing')) %>%
  mutate(a31 = recode(a31,
                      `0` = 'Get 100 birrs in 6 months.',
                      `1` = 'Get 105 birrs in a year.',
                      `-9` = 'Missing')) %>%
  mutate(a32 = recode(a32,
                      `0` = 'Get 100 birrs in 6 months.',
                      `1` = 'Get 110 birrs in a year.',
                      `-9` = 'Missing')) %>%
  mutate(a33 = recode(a33,
                      `0` = 'Get 100 birrs in 6 months.',
                      `1` = 'Get 120 birrs in a year.',
                      `-9` = 'Missing')) %>%  
  mutate(a34 = recode(a34,
                      `0` = 'Get 100 birrs in 6 months.',
                      `1` = 'Get 130 birrs in a year.',
                      `-9` = 'Missing')) %>%
  mutate(a35 = recode(a35,
                      `0` = 'Get 100 birrs in 6 months.',
                      `1` = 'Get 140 birrs in a year.',
                      `-9` = 'Missing')) %>%
  mutate(a36 = recode(a36,
                      `0` = 'Get 100 birrs in 6 months.',
                      `1` = 'Get 150 birrs in a year.',
                      `-9` = 'Missing'))

str(BT)


ggplot(data = data_BT) +
  geom_bar(aes(x = a11)) +
  ggsave('a11.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a12)) +
  ggsave('a12.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a13)) +
  ggsave('a13.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a14)) +
  ggsave('a14.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a15)) +
  ggsave('a15.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a16)) +
  ggsave('a16.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a21)) +
  ggsave('a21.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a22)) +
  ggsave('a22.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a23)) +
  ggsave('a23.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a24)) +
  ggsave('a24.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a25)) +
  ggsave('a25.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a26)) +
  ggsave('a26.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a31)) +
  ggsave('a31.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a32)) +
  ggsave('a32.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a33)) +
  ggsave('a33.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a34)) +
  ggsave('a34.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a35)) +
  ggsave('a35.png')

ggplot(data = data_BT) +
  geom_bar(aes(x = a36)) +
  ggsave('a36.png')

#### B1-B12 (Grading - Cognitive Test (ct)) ####


ct = c('b1',
       'b2',
       'b3',
       'b4',
       'b5',
       'b6',
       'b7',
       'b8',
       'b9',
       'b10',
       'b11',
       'b12')

data_ct = data %>%
  select(all_of(ct)) %>%
  mutate(across(ct[c(1:12)], as.factor)) %>%
  mutate(across(ct[c(1)], ~recode(.,
                                  `1` = 'Incorrect',
                                  `2` = 'Correct',
                                  `3` = 'Incorrect',
                                  `4` = 'Incorrect',
                                  `5` = 'Incorrect',
                                  `6` = 'Incorrect'))) %>%
  mutate(across(ct[c(2)], ~recode(.,
                                  `1` = 'Incorrect',
                                  `2` = 'Incorrect',
                                  `3` = 'Incorrect',
                                  `4` = 'Incorrect',
                                  `5` = 'Incorrect',
                                  `6` = 'Correct'))) %>%
  mutate(across(ct[c(3)], ~recode(.,
                                  `1` = 'Correct',
                                  `2` = 'Incorrect',
                                  `3` = 'Incorrect',
                                  `4` = 'Incorrect',
                                  `5` = 'Incorrect',
                                  `6` = 'Incorrect'))) %>%
  mutate(across(ct[c(4)], ~recode(.,
                                  `1` = 'Incorrect',
                                  `2` = 'Correct',
                                  `3` = 'Incorrect',
                                  `4` = 'Incorrect',
                                  `5` = 'Incorrect',
                                  `6` = 'Incorrect'))) %>%
  mutate(across(ct[c(5)], ~recode(.,
                                  `1` = 'Correct',
                                  `2` = 'Incorrect',
                                  `3` = 'Incorrect',
                                  `4` = 'Incorrect',
                                  `5` = 'Incorrect',
                                  `6` = 'Incorrect'))) %>%
  mutate(across(ct[c(6)], ~recode(.,
                                  `1` = 'Incorrect',
                                  `2` = 'Incorrect',
                                  `3` = 'Correct',
                                  `4` = 'Incorrect',
                                  `5` = 'Incorrect',
                                  `6` = 'Incorrect'))) %>% 
  mutate(across(ct[c(7)], ~recode(.,
                                  `1` = 'Incorrect',
                                  `2` = 'Incorrect',
                                  `3` = 'Incorrect',
                                  `4` = 'Incorrect',
                                  `5` = 'Correct',
                                  `6` = 'Incorrect'))) %>% 
  mutate(across(ct[c(8)], ~recode(.,
                                  `1` = 'Incorrect',
                                  `2` = 'Incorrect',
                                  `3` = 'Incorrect',
                                  `4` = 'Incorrect',
                                  `5` = 'Incorrect',
                                  `6` = 'Correct'))) %>%
  mutate(across(ct[c(9)], ~recode(.,
                                  `1` = 'Incorrect',
                                  `2` = 'Incorrect',
                                  `3` = 'Incorrect',
                                  `4` = 'Correct',
                                  `5` = 'Incorrect',
                                  `6` = 'Incorrect'))) %>%
  mutate(across(ct[c(10)], ~recode(.,
                                   `1` = 'Incorrect',
                                   `2` = 'Incorrect',
                                   `3` = 'Correct',
                                   `4` = 'Incorrect',
                                   `5` = 'Incorrect',
                                   `6` = 'Incorrect'))) %>%
  mutate(across(ct[c(11)], ~recode(.,
                                   `1` = 'Incorrect',
                                   `2` = 'Incorrect',
                                   `3` = 'Correct',
                                   `4` = 'Incorrect',
                                   `5` = 'Incorrect',
                                   `6` = 'Incorrect'))) %>%
  mutate(across(ct[c(12)], ~recode(.,
                                   `1` = 'Incorrect',
                                   `2` = 'Incorrect',
                                   `3` = 'Incorrect',
                                   `4` = 'Incorrect',
                                   `5` = 'Correct',
                                   `6` = 'Incorrect'))) %>%
  mutate(across(ct[c(12)], ~recode(.,
                                   `1` = 'Incorrect',
                                   `2` = 'Incorrect',
                                   `3` = 'Incorrect',
                                   `4` = 'Incorrect',
                                   `5` = 'Correct',
                                   `6` = 'Incorrect')))%>%
  mutate(across(ct[c(1:12)], ~recode(.,
                                     `Correct` = '1',
                                     `Incorrect` = '0')))%>%
  mutate(across(ct[c(1:12)], as.integer))


data_ct$tot_score_ct = data_ct$b1 + data_ct$b2 + data_ct$b3 + data_ct$b4 + data_ct$b5 + data_ct$b6 + data_ct$b7 + data_ct$b8 + data_ct$b9 + 
  data_ct$b10 + data_ct$b11 + data_ct$b12

data_ct$B1 = data_ct$b1 - 1 
data_ct$B2 = data_ct$b2 - 1 
data_ct$B3 = data_ct$b3 - 1 
data_ct$B4 = data_ct$b4 - 1 
data_ct$B5 = data_ct$b5 - 1 
data_ct$B6 = data_ct$b6 - 1 
data_ct$B7 = data_ct$b7 - 1 
data_ct$B8 = data_ct$b8 - 1 
data_ct$B9 = data_ct$b9 - 1 
data_ct$B10 = data_ct$b10 - 1 
data_ct$B11 = data_ct$b11 - 1 
data_ct$B12 = data_ct$b12 - 1 

data_ct$tot_score_ct = data_ct$B1 + data_ct$B2 + data_ct$B3 + data_ct$B4 + data_ct$B5 + data_ct$B6 + data_ct$B7 + data_ct$B8 + data_ct$B9 + 
  data_ct$B10 + data_ct$B11 + data_ct$B12

ct = c('B1',
       'B2',
       'B3',
       'B4',
       'B5',
       'B6',
       'B7',
       'B8',
       'B9',
       'B10',
       'B11',
       'B12',
       'tot_score_ct')

data_CT = data_ct %>%
  select(all_of(ct))

summary(data_CT)

data$tot_score_CT = data_ct$tot_score_ct 

data$normalize_CT = (data$tot_score_CT - mean(data$tot_score_CT, na.rm = TRUE)) / sd(data$tot_score_CT, na.rm = TRUE)
data$normalize_CT

summary(data$normalize_CT)

ggplot(data = data_CT) +
  geom_bar(aes(x = B1)) +
  ggsave('B1.png')

ggplot(data = data_CT) +
  geom_bar(aes(x = B2)) +
  ggsave('B2.png')

ggplot(data = data_CT) +
  geom_bar(aes(x = B3)) +
  ggsave('B3.png')

ggplot(data = data_CT) +
  geom_bar(aes(x = B4)) +
  ggsave('B4.png')

ggplot(data = data_CT) +
  geom_bar(aes(x = B5)) +
  ggsave('B5.png')

ggplot(data = data_CT) +
  geom_bar(aes(x = B6)) +
  ggsave('B6.png')

ggplot(data = data_CT) +
  geom_bar(aes(x = B7)) +
  ggsave('B7.png')

ggplot(data = data_CT) +
  geom_bar(aes(x = B8)) +
  ggsave('B8.png')

ggplot(data = data_CT) +
  geom_bar(aes(x = B9)) +
  ggsave('B9.png')

ggplot(data = data_CT) +
  geom_bar(aes(x = B10)) +
  ggsave('B10.png')

ggplot(data = data_CT) +
  geom_bar(aes(x = B11)) +
  ggsave('B11.png')

ggplot(data = data_CT) +
  geom_bar(aes(x = B12)) +
  ggsave('B12.png')


#### B1-B12 (Grading - Dexterity (DX)) ####

data$card_score = ifelse(data$card_qual == 0, data$card/25,
                         ifelse(data$card_qual == 1, data$card/22.5, 
                                ifelse(data$card_qual == 2, data$card/20,
                                       '0')))

data$needle_score = ifelse(data$needle_qual == 0, data$needle/2.5,
                           ifelse(data$needle_qual == 1, data$needle/1.5, 
                                  ifelse(data$needle_qual == 2, data$needle/0.5,
                                         '0')))

DX = c('card_qual',
       'card_score',
       'needle_qual',
       'needle_score')

data_DX = data %>%
  select(all_of(DX)) %>%
  mutate(across(DX[c(1:2)], as.factor)) %>%
  mutate(card_qual= recode(card_qual,
                           `0` = 'Two piles have the same number of cards.',
                           `1` = 'Two piles have similar number of cards but not exactly the same.',
                           `2` = 'Two piles have very differnt number of cards.',
                           `-9` = 'Missing')) %>%
  
  mutate(needle_qual= recode(needle_qual,
                             `0` = 'Worker pulled through 2 or 3 needles.',
                             `1` = 'Worker pulled 1 needle.',
                             `2` = 'Worker pulled 0 needle.',
                             `-9` = 'Missing')) 

str(data_DX) 

ggplot(data = data_DX) +
  geom_bar(aes(x = card_qual)) +
  ggsave('DX1.png')

ggplot(data = data_DX) +
  geom_bar(aes(x = needle_qual)) +
  ggsave('DX2.png')
