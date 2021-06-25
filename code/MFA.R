library(tidyverse)
library(readxl)
library(ggplot2)
setwd("~/HIP")
library(Factoshiny)
library(FactoMineR)
library(matrixStats)

rm(list = ls())

data = read_excel('weekdayend_all2.xlsx') 


# Part A

data$high_educ = ifelse(data$educ > 2, "Higher than 10th grade education", "10th grade education or lower")


# Part B

data$first_job= data$history_yesno

# Part C

data$tot_score_WM1 = data$series11_yesno + data$series12_yesno + data$series13_yesno + data$series14_yesno + data$series15_yesno + 
  data$series16_yesno + data$series17_yesno
data$tot_score_WM2 = data$series21_yesno + data$series22_yesno + data$series23_yesno + data$series24_yesno + data$series25_yesno + 
  data$series26_yesno + data$series27_yesno
data$tot_score_WM3 = data$series31_yesno + data$series32_yesno + data$series33_yesno + data$series34_yesno + data$series35_yesno + 
  data$series36_yesno + data$series37_yesno

TS_WM = c('tot_score_WM1',
          'tot_score_WM2',
          'tot_score_WM3')

data_TS_WM = data %>%
  select(all_of(TS_WM)) %>%
  mutate(across(TS_WM[c(1:3)], as.numeric))

data_TS_WM$row_min_WM = rowMins(as.matrix(data_TS_WM[,c(1,2,3)]), na.rm = TRUE)

summary(data_TS_WM)

data$normalize_WM1 = (data$tot_score_WM1 - mean(data$tot_score_WM1, na.rm = TRUE)) / sd(data$tot_score_WM1, na.rm = TRUE)
data$normalize_WM2 = (data$tot_score_WM2 - mean(data$tot_score_WM2, na.rm = TRUE)) / sd(data$tot_score_WM2, na.rm = TRUE)
data$normalize_WM3 = (data$tot_score_WM3 - mean(data$tot_score_WM3, na.rm = TRUE)) / sd(data$tot_score_WM3, na.rm = TRUE)

data_TS_WM$normalize_WM  = (data_TS_WM$row_min_WM- mean(data_TS_WM$row_min_WM, na.rm = TRUE)) / sd(data_TS_WM$row_min_WM, na.rm = TRUE)

data$normalize_WM = data_TS_WM$normalize_WM

# Part D

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

data_ct$tot_score_CT = data_ct$B1 + data_ct$B2 + data_ct$B3 + data_ct$B4 + data_ct$B5 + data_ct$B6 + data_ct$B7 + data_ct$B8 + data_ct$B9 + 
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
       'tot_score_CT')

data_CT = data_ct %>%
  select(all_of(ct))

summary(data_CT)

data$tot_score_CT = data_ct$tot_score_CT 

data$normalize_CT = (data$tot_score_CT - mean(data$tot_score_CT, na.rm = TRUE)) / sd(data$tot_score_CT, na.rm = TRUE)
data$normalize_CT

summary(data$normalize_CT)


# Part E 

data$card_score = ifelse(data$card_qual == 0, data$card/25,
                         ifelse(data$card_qual == 1, data$card/22.5, 
                                ifelse(data$card_qual == 2, data$card/20,
                                       '0')))

data$needle_score = ifelse(data$needle_qual == 0, data$needle/2.5,
                           ifelse(data$needle_qual == 1, data$needle/1.5, 
                                  ifelse(data$needle_qual == 2, data$needle/0.5,
                                         '0')))
# Subset data

MFA = c('high_educ',
        'first_job',
        'normalize_WM',
        'tot_score_CT',
        'card_score',
        'needle_score')

data_MFA = data %>%
  select(all_of(MFA))%>%
  mutate(across(MFA[c(1, 2)], as.factor)) %>%
  mutate(across(MFA[c(3, 4, 5, 6)], as.integer))


result = Factoshiny(data_MFA)

Factoshiny(result)

newDF <- data_MFA[,c("normalize_WM","tot_score_CT","card_score","needle_score","high_educ","first_job")]
res.MFA<-MFA(newDF,group=c(4,2), type=c("s","n"),name.group=c("Gr 1","Gr 2"),graph=FALSE)
plot.MFA(res.MFA, choix="ind",lab.par=FALSE,title="Individual factor map")
plot.MFA(res.MFA, choix="var",habillage='group',title="Correlation circle")
plot.MFA(res.MFA, choix="group")
plot.MFA(res.MFA, choix="axes",habillage='group')

newDF <- data_MFA[,c("normalize_WM","tot_score_CT","card_score","needle_score","high_educ","first_job")]
res.MFA<-MFA(newDF,group=c(4,2), type=c("s","n"),name.group=c("Gr 1","Gr 2"),graph=FALSE)
summary(res.MFA)

newDF <- data_MFA[,c("normalize_WM","tot_score_CT","card_score","needle_score","high_educ","first_job")]
res.MFA<-MFA(newDF,group=c(4,2), type=c("s","n"),name.group=c("Gr 1","Gr 2"),graph=FALSE)
dimdesc(res.MFA)

