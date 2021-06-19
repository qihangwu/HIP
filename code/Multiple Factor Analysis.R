library(tidyverse)
library(readxl)
library(ggplot2)
setwd("~/HIP")
install.packages("Factoshiny")
library(Factoshiny)
library(FactoMineR)

rm(list = ls())

data = read_excel('weekdayend_all.xlsx') 


data$high_educ = ifelse(data$educ > 2, "Higher than 10th grade education", "10th grade education or lower")


data$tot_score_WM1 = data$series11_yesno + data$series12_yesno + data$series13_yesno + data$series14_yesno + data$series15_yesno + 
  data$series16_yesno + data$series17_yesno
data$tot_score_WM2 = data$series21_yesno + data$series22_yesno + data$series23_yesno + data$series24_yesno + data$series25_yesno + 
  data$series26_yesno + data$series27_yesno
data$tot_score_WM3 = data$series31_yesno + data$series32_yesno + data$series33_yesno + data$series34_yesno + data$series35_yesno + 
  data$series36_yesno + data$series37_yesno

data$tot_score_CT = data$b1 + data$b2 + data$b3 + data$b4 + data$b5 + data$b6 + data$b7 + data$b8 + data$b9 + 
  data$b10 + data$b11 + data$b12

min(data$tot_score_WM1, na.rm=T)
min(data$tot_score_WM2, na.rm=T)
min(data$tot_score_WM3, na.rm=T)

data$card_score = ifelse(data$card_qual == 0, data$card/25,
                         ifelse(data$card_qual == 1, data$card/22.5, 
                                ifelse(data$card_qual == 2, data$card/20,
                                       '0')))

data$needle_score = ifelse(data$needle_qual == 0, data$needle/2.5,
                           ifelse(data$needle_qual == 1, data$needle/1.5, 
                                  ifelse(data$needle_qual == 2, data$needle/0.5,
                                         '0')))


MFA = c('high_educ',
        'history_yesno',
        'tot_score_WM1',
        'tot_score_WM2',
        'tot_score_WM3',
        'tot_score_CT',
        'card_score',
        'needle_score')

data_MFA = data %>%
select(all_of(MFA))%>%
  mutate(across(MFA[c(1, 2)], as.factor)) %>%
  mutate(across(MFA[c(3, 4, 5, 6, 7, 8)], as.integer))


result = Factoshiny(data_MFA)

Factoshiny(result)

newDF <- data_MFA[,c("tot_score_WM1","tot_score_WM2","tot_score_WM3","tot_score_CT","card_score","needle_score","high_educ","history_yesno")]
res.MFA<-MFA(newDF,group=c(6,2), type=c("s","n"),name.group=c("Gr 1","Gr 2"),graph=FALSE)
plot.MFA(res.MFA, choix="ind",lab.par=FALSE,title="Individual factor map")
plot.MFA(res.MFA, choix="var",habillage='group',title="Correlation circle")
plot.MFA(res.MFA, choix="group")
plot.MFA(res.MFA, choix="axes",habillage='group')

newDF <- data_MFA[,c("tot_score_WM1","tot_score_WM2","tot_score_WM3","tot_score_CT","card_score","needle_score","high_educ","history_yesno")]
res.MFA<-MFA(newDF,group=c(6,2), type=c("s","n"),name.group=c("Gr 1","Gr 2"),graph=FALSE)
summary(res.MFA)

newDF <- data_MFA[,c("tot_score_WM1","tot_score_WM2","tot_score_WM3","tot_score_CT","card_score","needle_score","high_educ","history_yesno")]
res.MFA<-MFA(newDF,group=c(6,2), type=c("s","n"),name.group=c("Gr 1","Gr 2"),graph=FALSE)
dimdesc(res.MFA)

