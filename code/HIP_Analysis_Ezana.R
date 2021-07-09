library(stargazer)
library(reshape2)
library(VGAM)
library(lmtest)
library(sandwich)

source('code/outside_options.R')
source('code/jobs_in_HIP.R')
source('code/jobs_in_HIP_followup.R')
source('code/DM_JA_TR_M.R')
source('code/DM_WF_JH_E.R')
source('code/Type_AT_WM_M.R')
source('code/Grading_BT_CT_DX.R')
source('code/merge.R')

# Multifactor Analysis Start ----------------------------------------


data_analysis <- data %>%
  select(c('wid', 'treat1', 'treat2a', 'treat2b')) %>%
  cbind(data_DMm) %>%
  cbind(data_JH) %>%
  cbind(data_TS_WM) %>%
  cbind(data_CT) %>%
  cbind(data_DX)

MFA = c('high_educ',
        'first_job',
        'normalize_WM',
        'tot_score_CT',
        'card_score',
        'needle_score')

data_MFA = data_analysis %>%
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


# HIP Analysis Start ---------------------------------------------------


# Total Cognitive Test (tot_score_CT) Score as Outcome Variable -----------------------------------------------

reg_educ_CT1 = lm(tot_score_CT ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_CT1)
coeftest(reg_educ_CT1, vcov = vcovHC(reg_educ_CT1 , type="HC1"))
plot(reg_educ_CT1)

reg_job_CT2 = lm(tot_score_CT ~ first_job, data = hip_analysis_pool)
summary(reg_job_CT2)
coeftest(reg_educ_CT2, vcov = vcovHC(reg_educ_CT2 , type="HC1"))
plot(reg_job_CT2)

reg_t1_CT3 = lm(tot_score_CT ~ treat1, data = hip_analysis_pool)
summary(reg_t1_CT3)
coeftest(reg_t1_CT3, vcov = vcovHC(reg_t1_CT3 , type="HC1"))
plot(reg_t1_CT3)

reg_t2_CT4 =  lm(tot_score_CT ~ treat2, data = hip_analysis_pool)
summary(reg_t2_CT4)
coeftest(reg_t2_CT4, vcov = vcovHC(reg_t2_CT4 , type="HC1"))
plot(reg_t2_CT4)

reg_t2a_CT5 =  lm(tot_score_CT ~ treat2a, data = hip_analysis_pool)
summary(reg_t2a_CT5)
coeftest(reg_treat2a_CT5, vcov = vcovHC(reg_treat2a_CT5 , type="HC1"))
plot(reg_treat2b_CT5)

reg_t2b_CT6 =  lm(tot_score_CT ~ treat2b, data = hip_analysis_pool)
summary(reg_t2b_CT6)
coeftest(reg_t2b_CT6, vcov = vcovHC(reg_t2b_CT6 , type="HC1"))
plot(reg_t2b_CT6)

reg_t12_CT7 =  lm(tot_score_CT ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12_CT7)
coeftest(reg_t12_CT7, vcov = vcovHC(reg_t12_CT7 , type="HC1"))
plot(reg_educ_CT7)

reg_all_CT8 = lm(tot_score_CT ~ treat1*treat2 + treat1 + treat2 + first_job +high_educ, data = hip_analysis_pool)
summary(reg_all_CT8)
coeftest(reg_all_CT8, vcov = vcovHC(reg_all_CT8 , type="HC1"))
plot(reg_all_CT8)

# Normalize Working Memory (normalize_WM) Score as Outcome Variable -----------------------------------------------

reg_educ_WM1 = lm(normalize_WM ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_WM1)
coeftest(reg_educ_WM1, vcov = vcovHC(reg_educ_WM1 , type="HC1"))
plot(reg_educ_WM1)

reg_job_WM2 = lm(normalize_WM ~ first_job, data = hip_analysis_pool)
summary(reg_job_WM2)
coeftest(reg_job_WM2, vcov = vcovHC(reg_job_WM2 , type="HC1"))
plot(reg_job_WM2)


reg_t1_WM3 = lm(normalize_WM ~ treat1, data = hip_analysis_pool)
summary(reg_t1_WM3)
coeftest(reg_t1_WM3, vcov = vcovHC(reg_t1_WM3 , type="HC1"))
plot(reg_t1_WM3)


reg_t2_WM4 =  lm(normalize_WM ~ treat2, data = hip_analysis_pool)
summary(reg_treat2_WM4)
coeftest(reg_t2_WM4, vcov = vcovHC(reg_t2_WM4 , type="HC1"))
plot(reg_treat2_WM4)


reg_t2a_WM5 =  lm(normalize_WM ~ treat2a, data = hip_analysis_pool)
summary(reg_t2a_WM5)
coeftest(reg_t2a_WM5, vcov = vcovHC(reg_t2a_WM5 , type="HC1"))
plot(reg_treat2a_WM5)

reg_t2b_WM6 =  lm(normalize_WM ~ treat2b, data = hip_analysis_pool)
summary(reg_t2b_WM6)
coeftest(reg_t2b_WM6, vcov = vcovHC(reg_t2b_WW6 , type="HC1"))
plot(reg_t2b_WM6)


reg_t12_WM7 =  lm(normalize_WM ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12_WM7)
coeftest(reg_t12_WM7, vcov = vcovHC(reg_t12_WM7, type="HC1"))
plot(reg_t12_WM7)


reg_treat12b_WM8 =  lm(normalize_WM ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_treat12b_WM8)
coeftest(reg_t12b_WM8, vcov = vcovHC(reg_t12b_WM8 , type="HC1"))
plot(reg_treat12b_WM8)

reg_all_WM9 = lm(normalize_WM ~ treat1*treat2 + treat1 + treat2 + first_job +high_educ, data = hip_analysis_pool)
summary(reg_all_WM9)
coeftest(reg_all_WM9, vcov = vcovHC(reg_all_WM9 , type="HC1"))
plot(reg_all_WM9)

# Card Score (card_score) as Outcome Variable -----------------------------------------------


reg_educ_CS1 = lm(card_score ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_CS1)
coeftest(reg_educ_CS1, vcov = vcovHC(reg_educ_CS1 , type="HC1"))
plot(reg_educ_CS1)

reg_job_CS2 = lm(card_score  ~ first_job, data = hip_analysis_pool)
summary(reg_job_WM2)
coeftest(reg_educ_CS2, vcov = vcovHC(reg_educ_CS2 , type="HC1"))
plot(reg_job_CS2)


reg_t1_CS3 = lm(card_score  ~ treat1, data = hip_analysis_pool)
summary(reg_t1_CS3)
coeftest(reg_t1_CS3, vcov = vcovHC(reg_t1_CS3 , type="HC1"))
plot(reg_t1_CS3)


reg_t2_CS4 =  lm(card_score ~ treat2, data = hip_analysis_pool)
summary(reg_t2_CS4)
coeftest(reg_t2_CS4, vcov = vcovHC(reg_t2_CS4 , type="HC1"))
plot(reg_t2_CS4)

reg_t2a_CS5 =  lm(card_score ~ treat2a, data = hip_analysis_pool)
summary(reg_t2a_CS5)
coeftest(reg_t2a_CS1, vcov = vcovHC(reg_t2a_CS1 , type="HC1"))
plot(reg_t2a_CS5)


reg_t2b_CS6 =  lm(card_score ~ treat2b, data = hip_analysis_pool)
summary(reg_t2b_CS6)
coeftest(reg_t2b_CS6, vcov = vcovHC(reg_t2b_CS6 , type="HC1"))
plot(reg_t2b_CS6)


reg_t12_CS7 =  lm(card_score ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12_CS7)
coeftest(reg_t12_CS7, vcov = vcovHC(reg_t12_CS7 , type="HC1"))
plot(reg_t12_CS7)

reg_t12b_CS8 =  lm(card_score ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_t12b_CS8)
coeftest(reg_t12b_CS8, vcov = vcovHC(reg_t12b_CS8 , type="HC1"))
plot(reg_t12b_CS8)

reg_all_CS9 = glm(card_score ~ treat1*treat2 + treat1 + treat2 + first_job +high_educ, family = binomial, data = hip_analysis_pool)
summary(reg_all_CS9)
coeftest(reg_all_CS9, vcov = vcovHC(reg_all_CS9 , type="HC1"))
plot(reg_all_CS9)


# Needle Score (needle_score) as Outcome Variable -----------------------------------------------


reg_educ_NS1 = lm(needle_score ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_NS1)
coeftest(reg_educ_NS1, vcov = vcovHC(reg_educ_NS1 , type="HC1"))
plot(reg_educ_NS1)

reg_job_NS2 = lm(needle_score ~ first_job, data = hip_analysis_pool)
summary(reg_job_NS2)
coeftest(reg_educ_NS2, vcov = vcovHC(reg_educ_NS2 , type="HC1"))
plot(reg_job_NS2)

reg_t1_NS3 = lm(needle_score ~ treat1, data = hip_analysis_pool)
summary(reg_t1_NS3)
coeftest(reg_t1_NS3, vcov = vcovHC(reg_t1_NS3 , type="HC1"))
plot(reg_t1_NS3)

reg_t2_NS4 =  lm(needle_score ~ treat2, data = hip_analysis_pool)
summary(reg_t2_NS4)
coeftest(reg_t2_NS4, vcov = vcovHC(reg_t2_NS4 , type="HC1"))
plot(reg_t2_NS4)


reg_t2a_NS5 =  lm(needle_score ~ treat2a, data = hip_analysis_pool)
summary(reg_t2a_NS5)
coeftest(reg_t2a_NS5, vcov = vcovHC(reg_t2a_NS5 , type="HC1"))
plot(reg_t2a_NS5)

reg_t2b_NS6 =  lm(needle_score ~ treat2b, data = hip_analysis_pool)
summary(reg_t2b_NS6)
coeftest(reg_t2b_NS6, vcov = vcovHC(reg_t2b_NS6 , type="HC1"))
plot(reg_t2b_NS6)

reg_t12_NS7 =  lm(needle_score ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12_NS7)
coeftest(reg_t12_NS7, vcov = vcovHC(reg_t12_NS7 , type="HC1"))
plot(reg_t12_NS7)

reg_t12b_NS8 =  lm(needle_score ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_t12b_NS8)
coeftest(reg_t12b_NS8, vcov = vcovHC(reg_t12b_NS8 , type="HC1"))
plot(reg_t12b_NS8)

reg_all_NS9 = lm(needle_score ~ treat1*treat2 + treat1 + treat2 + first_job +high_educ, data = hip_analysis_pool)
summary(reg_all_NS9)
coeftest(reg_all_NS9, vcov = vcovHC(reg_all_NS9 , type="HC1"))
plot(reg_all_NS9)


# VGAM on high_educ and first_job
# Generate MFA on treatment
# Make 270 value as missing (NA - Corrected already)
# put all variables in one regression

reg_heduc_fjob  =  glm(first_job ~ high_educ, data = hip_analysis_pool)
summary(reg_heduc_fjob )
coeftest(reg_heduc_fjob , vcov = vcovHC(reg_heduc_fjob , type="HC1"))
plot(reg_heduc_fjob)


reg_fjob_heduc =  glm(high_educ ~ first_job, family = binomial, data = hip_analysis_pool)
summary(reg_fjob_heduc)
coeftest(reg_fjob_heduc, vcov = vcovHC(reg_fjob_heduc, type="HC1"))
plot(reg_fjob_heduc)
