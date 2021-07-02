library(stargazer)
library(reshape2)

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
# put all variables in one regression

reg_educ_CT1 = lm(tot_score_CT ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_CT1)
coeftest(reg_educ_CT1)
plot(reg_educ_CT1)
abline(reg_educ_CT1)

reg_job_CT2 = lm(tot_score_CT ~ first_job, data = hip_analysis_pool)
summary(reg_job_CT2)
coeftest(reg_educ_CT2)
plot(reg_job_CT2)
abline(reg_job_CT2)

reg_educ_CT3 = lm(tot_score_CT ~ treat1, data = hip_analysis_pool)
summary(reg_educ_CT3)
coeftest(reg_educ_CT3)
plot(reg_educ_CT3)
abline(reg_educ_CT3)

reg_educ_CT4 =  lm(tot_score_CT ~ treat2, data = hip_analysis_pool)
summary(reg_educ_CT4)
coeftest(reg_educ_CT4)
plot(reg_educ_CT4)
abline(reg_educ_CT4)

reg_educ_CT5 =  lm(tot_score_CT ~ treat2a, data = hip_analysis_pool)
summary(reg_educ_CT5)
plot(reg_educ_CT5)
abline(reg_educ_CT5)

reg_educ_CT6 =  lm(tot_score_CT ~ treat2b, data = hip_analysis_pool)
summary(reg_educ_CT6)
plot(reg_educ_CT6)
abline(reg_educ_CT6)

reg_educ_CT6 =  lm(tot_score_CT ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_educ_CT6)
plot(reg_educ_CT6)
abline(reg_educ_CT6)


# Normalize Working Memory (normalize_WM) Score as Outcome Variable -----------------------------------------------
# Make 270 value as missing (NA)

reg_educ_WM1 = lm(normalize_WM ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_WM1)
plot(reg_educ_WM1)
abline(reg_educ_WM1)

reg_job_WM2 = lm(normalize_WM ~ first_job, data = hip_analysis_pool)
summary(reg_job_WM2)
plot(reg_job_WM2)
abline(reg_job_WM2)

reg_treat1_WM3 = lm(normalize_WM ~ treat1, data = hip_analysis_pool)
summary(reg_treat1_WM3)
plot(reg_treat1_WM3)
abline(reg_treat1_WM3)

reg_treat2_WM4 =  lm(normalize_WM ~ treat2, data = hip_analysis_pool)
summary(reg_treat2_WM4)
plot(reg_treat2_WM4)
abline(reg_treat2_WM4)

reg_treat2a_WM5 =  lm(normalize_WM ~ treat2a, data = hip_analysis_pool)
summary(reg_treat2a_WM5)
plot(reg_treat2a_WM5)
abline(reg_treat2a_WM5)

reg_treat2b_WM6 =  lm(normalize_WM ~ treat2b, data = hip_analysis_pool)
summary(reg_treat2b_WM6)
plot(reg_treat2b_WM6)
abline(reg_treat2b_WM6)

reg_treat12_WM7 =  lm(normalize_WM ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_treat12_WM7)
plot(reg_treat12_WM7)
abline(reg_treat12_WM7)

reg_treat12b_WM8 =  lm(normalize_WM ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_treat12b_WM8)
plot(reg_treat12b_WM8)
abline(reg_treat12b_WM8)


# Card Score (card_score) as Outcome Variable -----------------------------------------------


reg_educ_CS1 = lm(card_score ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_CS1)
plot(reg_educ_CS1)
abline(reg_educ_CS1)

reg_job_CS2 = lm(card_score  ~ first_job, data = hip_analysis_pool)
summary(reg_job_WM2)
plot(reg_job_CS2)
abline(reg_job_CS2)

reg_treat1_CS3 = lm(card_score  ~ treat1, data = hip_analysis_pool)
summary(reg_treat1_CS3)
plot(reg_treat1_CS3)
abline(reg_treat1_CS3)

reg_treat2_CS4 =  lm(card_score ~ treat2, data = hip_analysis_pool)
summary(reg_treat2_CS4)
plot(reg_treat2_CS4)
abline(reg_treat2_CS4)

reg_treat2a_CS5 =  lm(card_score ~ treat2a, data = hip_analysis_pool)
summary(reg_treat2a_CS5)
plot(reg_treat2a_CS5)
abline(reg_treat2a_CS5)

reg_treat2b_CS6 =  lm(card_score ~ treat2b, data = hip_analysis_pool)
summary(reg_treat2b_CS6)
plot(reg_treat2b_CS6)
abline(reg_treat2b_CS6)

reg_treat12_CS7 =  lm(card_score ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_treat12_CS7)
plot(reg_treat12_CS7)
abline(reg_treat12_CS7)

reg_treat12b_CS8 =  lm(card_score ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_treat12b_CS8)
plot(reg_treat12b_CS8)
abline(reg_treat12b_CS8)


# Needle Score (needle_score) as Outcome Variable -----------------------------------------------


reg_educ_NS1 = lm(needle_score ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_NS1)
plot(reg_educ_NS1)
abline(reg_educ_NS1)

reg_job_NS2 = lm(needle_score ~ first_job, data = hip_analysis_pool)
summary(reg_job_NS2)
plot(reg_job_NS2)
abline(reg_job_NS2)

reg_treat1_NS3 = lm(needle_score ~ treat1, data = hip_analysis_pool)
summary(reg_treat1_NS3)
plot(reg_treat1_NS3)
abline(reg_treat1_NS3)

reg_treat2_NS4 =  lm(needle_score ~ treat2, data = hip_analysis_pool)
summary(reg_treat2_NS4)
plot(reg_treat2_NS4)
abline(reg_treat2_NS4)

reg_treat2a_NS5 =  lm(needle_score ~ treat2a, data = hip_analysis_pool)
summary(reg_treat2a_NS5)
plot(reg_treat2a_NS5)
abline(reg_treat2a_NS5)

reg_treat2b_NS6 =  lm(needle_score ~ treat2b, data = hip_analysis_pool)
summary(reg_treat2b_NS6)
plot(reg_treat2b_NS6)
abline(reg_treat2b_NS6)

reg_treat12_NS7 =  lm(needle_score ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_treat12_NS7)
plot(reg_treat12_NS7)
abline(reg_treat12_NS7)

reg_treat12b_NS8 =  lm(needle_score ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_treat12b_NS8)
plot(reg_treat12b_NS8)
abline(reg_treat12b_NS8)



# VGAM on high_educ and first_job
# Generate MFA on treatment

reg =  glm(high_educ ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_educ_CT6)
plot(reg_educ_CT6)
abline(reg_educ_CT6)
