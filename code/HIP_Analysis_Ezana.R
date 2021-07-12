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
source('code/analysis_max.R')

# HIP Analysis Start ---------------------------------------------------

hip_analysis <- hip %>%
  select(c('wid', 'treat1', 'treat2a', 'treat2b')) %>%
  cbind(hip_IC[c(1:4, 6:9, 14:17)]) %>%
  cbind(hip_w_IC[c(1:4, 6:9, 14:17)]) %>%
  cbind(hip_ID[c(5:8, 10:13, 17:20)]) %>%
  cbind(hip_w_ID[c(5:8, 10:13, 17:20)]) %>%
  cbind(data_DMm) %>%
  cbind(data_DMe) %>%
  cbind(data_WF) %>%
  cbind(data_AT) %>%
  cbind(data_JA) %>%
  cbind(data_TR) %>%
  cbind(data_JH) %>%
  cbind(data_TS_WM) %>%
  cbind(data_CT) %>%
  cbind(data_DX)


hip_analysis_pool <- hip_analysis %>%
  mutate(treat2 = ifelse(treat2a == 1 | treat2b == 1, 1, 0),
         .after = treat2b) %>%
  mutate(treat12 = treat1 * treat2)


data_analysis <- data %>%
  select(c('wid', 'treat1', 'treat2a', 'treat2b')) %>%
  cbind(data_DMm) %>%
  cbind(data_JH) %>%
  cbind(data_TS_WM) %>%
  cbind(data_CT) %>%
  cbind(data_DX)


# Multifactor Analysis Start ----------------------------------------


MFA = c('high_educ',
        'first_job',
        'normalize_WM',
        'tot_score_CT',
        'card_score',
        'needle_score')

data_MFA = data_analysis %>%
  select(all_of(MFA))%>%
  mutate(across(MFA[c(1, 2)], as.factor)) %>%
  mutate(across(MFA[c(3, 4, 5, 6)], as.numeric))

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


# Total Cognitive Test (tot_score_CT) Score as Outcome Variable -----------------------------------------------

reg_educ_CT = lm(tot_score_CT ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_CT)
coeftest(reg_educ_CT, vcov = vcovHC(reg_educ_CT , type="HC1"))
plot(reg_educ_CT)

reg_job_CT = lm(tot_score_CT ~ first_job, data = hip_analysis_pool)
summary(reg_job_CT)
coeftest(reg_job_CT, vcov = vcovHC(reg_job_CT , type="HC1"))
plot(reg_job_CT)

reg_t1_CT = lm(tot_score_CT ~ treat1, data = hip_analysis_pool)
summary(reg_t1_CT)
coeftest(reg_t1_CT, vcov = vcovHC(reg_t1_CT , type="HC1"))
plot(reg_t1_CT)

reg_t2_CT =  lm(tot_score_CT ~ treat2, data = hip_analysis_pool)
summary(reg_t2_CT)
coeftest(reg_t2_CT, vcov = vcovHC(reg_t2_CT , type="HC1"))
plot(reg_t2_CT)

reg_t2a_CT =  lm(tot_score_CT ~ treat2a, data = hip_analysis_pool)
summary(reg_t2a_CT)
coeftest(reg_t2a_CT, vcov = vcovHC(reg_t2a_CT , type="HC1"))
plot(reg_t2a_CT)

reg_t2b_CT =  lm(tot_score_CT ~ treat2b, data = hip_analysis_pool)
summary(reg_t2b_CT)
coeftest(reg_t2b_CT, vcov = vcovHC(reg_t2b_CT , type="HC1"))
plot(reg_t2b_CT)

reg_t12_CT =  lm(tot_score_CT ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12_CT)
coeftest(reg_t12_CT, vcov = vcovHC(reg_t12_CT , type="HC1"))
plot(reg_t12_CT)

reg_t12a_CT =  lm(tot_score_CT ~ treat1*treat2a, data = hip_analysis_pool)
summary(reg_t12a_CT)
coeftest(reg_t12a_CT, vcov = vcovHC(reg_t12a_CT , type="HC1"))
plot(reg_t12a_CT)

reg_t12b_CT =  lm(tot_score_CT ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_t12b_CT)
coeftest(reg_t12b_CT, vcov = vcovHC(reg_t12b_CT , type="HC1"))
plot(reg_t12b_CT)

reg_all_CT = lm(tot_score_CT ~ treat1*treat2 + normalize_WM + card_score +needle_score + first_job +high_educ, data = hip_analysis_pool)
summary(reg_all_CT)
coeftest(reg_all_CT, vcov = vcovHC(reg_all_CT , type="HC1"))
plot(reg_all_CT)

stargazer(reg_educ_CT, reg_job_CT, reg_t1_CT, reg_t2_CT, reg_t2a_CT, reg_t2b_CT, reg_t12_CT, reg_t12a_CT, reg_t12b_CT, reg_all_CT, type = 'text')

rob_se_CT <- list(sqrt(diag(vcovHC(reg_educ_CT, type = "HC1"))),
               sqrt(diag(vcovHC(reg_job_CT, type = "HC1"))),
               sqrt(diag(vcovHC(reg_t1_CT, type = "HC1"))),
               sqrt(diag(vcovHC(reg_t2_CT, type = "HC1"))),
               sqrt(diag(vcovHC(reg_t2a_CT, type = "HC1"))),
               sqrt(diag(vcovHC(reg_t2b_CT, type = "HC1"))),
               sqrt(diag(vcovHC(reg_t12_CT, type = "HC1"))),
               sqrt(diag(vcovHC(reg_t12a_CT, type = "HC1"))),
               sqrt(diag(vcovHC(reg_t12b_CT, type = "HC1"))),
               sqrt(diag(vcovHC(reg_all_CT, type = "HC1"))))

stargazer(reg_educ_CT, reg_job_CT, reg_t1_CT, reg_t2_CT, reg_t2a_CT, reg_t2b_CT, reg_t12_CT, reg_t12a_CT, reg_t12b_CT, reg_all_CT, type = "text", se = rob_se_CT)

# Normalize Working Memory (normalize_WM) Score as Outcome Variable -----------------------------------------------

reg_educ_WM = lm(normalize_WM ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_WM)
coeftest(reg_educ_WM, vcov = vcovHC(reg_educ_WM , type="HC1"))
plot(reg_educ_WM)

reg_job_WM = lm(normalize_WM ~ first_job, data = hip_analysis_pool)
summary(reg_job_WM)
coeftest(reg_job_WM, vcov = vcovHC(reg_job_WM , type="HC1"))
plot(reg_job_WM)

reg_t1_WM = lm(normalize_WM ~ treat1, data = hip_analysis_pool)
summary(reg_t1_WM)
coeftest(reg_t1_WM, vcov = vcovHC(reg_t1_WM , type="HC1"))
plot(reg_t1_WM)

reg_t2_WM =  lm(normalize_WM ~ treat2, data = hip_analysis_pool)
summary(reg_t2_WM)
coeftest(reg_t2_WM, vcov = vcovHC(reg_t2_WM , type="HC1"))
plot(reg_t2_WM)

reg_t2a_WM =  lm(normalize_WM ~ treat2a, data = hip_analysis_pool)
summary(reg_t2a_WM)
coeftest(reg_t2a_WM, vcov = vcovHC(reg_t2a_WM , type="HC1"))
plot(reg_t2a_WM)

reg_t2b_WM =  lm(normalize_WM ~ treat2b, data = hip_analysis_pool)
summary(reg_t2b_WM)
coeftest(reg_t2b_WM, vcov = vcovHC(reg_t2b_WM , type="HC1"))
plot(reg_t2b_WM)

reg_t12_WM =  lm(normalize_WM ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12_WM)
coeftest(reg_t12_WM, vcov = vcovHC(reg_t12_WM, type="HC1"))
plot(reg_t12_WM)

reg_t12a_WM =  lm(normalize_WM ~ treat1*treat2a, data = hip_analysis_pool)
summary(reg_t12a_WM)
coeftest(reg_t12_WM, vcov = vcovHC(reg_t12a_WM, type="HC1"))
plot(reg_t12a_WM)

reg_t12b_WM =  lm(normalize_WM ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_t12b_WM)
coeftest(reg_t12b_WM, vcov = vcovHC(reg_t12b_WM , type="HC1"))
plot(reg_t12b_WM)

reg_all_WM = lm(normalize_WM ~ treat1*treat2 + tot_score_CT + card_score +needle_score + first_job +high_educ, data = hip_analysis_pool)
summary(reg_all_WM)
coeftest(reg_all_WM, vcov = vcovHC(reg_all_WM , type="HC1"))
plot(reg_all_WM)

stargazer(reg_educ_WM, reg_job_WM, reg_t1_WM, reg_t2_WM, reg_t2a_WM, reg_t2b_WM, reg_t12_WM, reg_t12a_WM, reg_t12b_WM, reg_all_WM, type = 'text')

rob_se_WM <- list(sqrt(diag(vcovHC(reg_educ_WM, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_job_WM, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t1_WM, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t2_WM, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t2a_WM, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t2b_WM, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t12_WM, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t12a_WM, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t12b_WM, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_all_WM, type = "HC1"))))

stargazer(reg_educ_WM, reg_job_WM, reg_t1_WM, reg_t2_WM, reg_t2a_WM, reg_t2b_WM, reg_t12_WM, reg_t12a_WM, reg_t12b_WM, reg_all_WM, type = "text", se = rob_se_WM)


# Card Score (card_score) as Outcome Variable -----------------------------------------------


reg_educ_CS = lm(card_score ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_CS)
coeftest(reg_educ_CS, vcov = vcovHC(reg_educ_CS , type="HC1"))
plot(reg_educ_CS)

reg_job_CS = lm(card_score  ~ first_job, data = hip_analysis_pool)
summary(reg_job_CS)
coeftest(reg_job_CS, vcov = vcovHC(reg_job_CS , type="HC1"))
plot(reg_job_CS)

reg_t1_CS = lm(card_score  ~ treat1, data = hip_analysis_pool)
summary(reg_t1_CS)
coeftest(reg_t1_CS, vcov = vcovHC(reg_t1_CS , type="HC1"))
plot(reg_t1_CS)

reg_t2_CS =  lm(card_score ~ treat2, data = hip_analysis_pool)
summary(reg_t2_CS)
coeftest(reg_t2_CS, vcov = vcovHC(reg_t2_CS , type="HC1"))
plot(reg_t2_CS)

reg_t2a_CS =  lm(card_score ~ treat2a, data = hip_analysis_pool)
summary(reg_t2a_CS)
coeftest(reg_t2a_CS, vcov = vcovHC(reg_t2a_CS , type="HC1"))
plot(reg_t2a_CS)

reg_t2b_CS =  lm(card_score ~ treat2b, data = hip_analysis_pool)
summary(reg_t2b_CS)
coeftest(reg_t2b_CS, vcov = vcovHC(reg_t2b_CS , type="HC1"))
plot(reg_t2b_CS)

reg_t12_CS =  lm(card_score ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12_CS)
coeftest(reg_t12_CS, vcov = vcovHC(reg_t12_CS , type="HC1"))
plot(reg_t12_CS)

reg_t12a_CS =  lm(card_score ~ treat1*treat2a, data = hip_analysis_pool)
summary(reg_t12a_CS)
coeftest(reg_t12_CS, vcov = vcovHC(reg_t12a_CS , type="HC1"))
plot(reg_t12a_CS)

reg_t12b_CS =  lm(card_score ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_t12b_CS)
coeftest(reg_t12b_CS, vcov = vcovHC(reg_t12b_CS , type="HC1"))
plot(reg_t12b_CS)

reg_all_CS = lm(card_score ~ treat1*treat2 + tot_score_CT + normalize_WM + needle_score + first_job +high_educ, data = hip_analysis_pool)
summary(reg_all_CS)
coeftest(reg_all_CS, vcov = vcovHC(reg_all_CS , type="HC1"))
plot(reg_all_CS)

stargazer(reg_educ_CS, reg_job_CS, reg_t1_CS, reg_t2_CS, reg_t2a_CS, reg_t2b_CS, reg_t12_CS, reg_t12a_CS, reg_t12b_CS, reg_all_CS9, type = 'text')

rob_se_CS <- list(sqrt(diag(vcovHC(reg_educ_CS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_job_CS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t1_CS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t2_CS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t2a_CS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t2b_CS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t12_CS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t12a_CS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t12b_CS8, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_all_CS9, type = "HC1"))))

stargazer(reg_educ_CS, reg_job_CS, reg_t1_CS, reg_t2_CS, reg_t2a_CS, reg_t2b_CS, reg_t12_CS, reg_t12a_CS, reg_t12b_CS, reg_all_CS9, type = "text", se = rob_se_CS)



# Needle Score (needle_score) as Outcome Variable -----------------------------------------------


reg_educ_NS = lm(needle_score ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_NS)
coeftest(reg_educ_NS, vcov = vcovHC(reg_educ_NS , type="HC1"))
plot(reg_educ_NS)

reg_job_NS = lm(needle_score ~ first_job, data = hip_analysis_pool)
summary(reg_job_NS)
coeftest(reg_job_NS, vcov = vcovHC(reg_job_NS , type="HC1"))
plot(reg_job_NS)

reg_t1_NS = lm(needle_score ~ treat1, data = hip_analysis_pool)
summary(reg_t1_NS)
coeftest(reg_t1_NS, vcov = vcovHC(reg_t1_NS , type="HC1"))
plot(reg_t1_NS)

reg_t2_NS =  lm(needle_score ~ treat2, data = hip_analysis_pool)
summary(reg_t2_NS)
coeftest(reg_t2_NS, vcov = vcovHC(reg_t2_NS , type="HC1"))
plot(reg_t2_NS)

reg_t2a_NS =  lm(needle_score ~ treat2a, data = hip_analysis_pool)
summary(reg_t2a_NS)
coeftest(reg_t2a_NS, vcov = vcovHC(reg_t2a_NS , type="HC1"))
plot(reg_t2a_NS)

reg_t2b_NS =  lm(needle_score ~ treat2b, data = hip_analysis_pool)
summary(reg_t2b_NS)
coeftest(reg_t2b_NS, vcov = vcovHC(reg_t2b_NS , type="HC1"))
plot(reg_t2b_NS)

reg_t12_NS =  lm(needle_score ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12_NS)
coeftest(reg_t12_NS, vcov = vcovHC(reg_t12_NS , type="HC1"))
plot(reg_t12_NS)

reg_t12a_NS =  lm(needle_score ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12a_NS)
coeftest(reg_t12a_NS, vcov = vcovHC(reg_t12a_NS , type="HC1"))
plot(reg_t12a_NS)

reg_t12b_NS =  lm(needle_score ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_t12b_NS)
coeftest(reg_t12b_NS, vcov = vcovHC(reg_t12b_NS , type="HC1"))
plot(reg_t12b_NS)

reg_all_NS = lm(needle_score ~ treat1*treat2 + tot_score_CT + card_score + normalize_WM + first_job +high_educ, data = hip_analysis_pool)
summary(reg_all_NS)
coeftest(reg_all_NS, vcov = vcovHC(reg_all_NS , type="HC1"))
plot(reg_all_NS)

stargazer(reg_educ_NS, reg_job_NS, reg_t1_NS, reg_t2_NS, reg_t2a_NS, reg_t2b_NS, reg_t12_NS, reg_t12a_NS, reg_t12b_NS, reg_all_NS, type = 'text')

rob_se_NS <- list(sqrt(diag(vcovHC(reg_educ_NS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_job_NS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t1_NS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t2_NS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t2a_NS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t2b_NS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t12_NS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t12a_NS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t12b_NS, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_all_NS, type = "HC1"))))

stargazer(reg_educ_NS, reg_job_NS, reg_t1_NS, reg_t2_NS, reg_t2a_NS, reg_t2b_NS, reg_t12_NS, reg_t12a_NS, reg_t12b_NS, reg_all_NS, type = "text", se = rob_se_NS)


# VGAM on high_educ and first_job  -----------------------------------------------

reg_heduc_fjob  =  glm(first_job ~ high_educ, data = hip_analysis_pool)
summary(reg_heduc_fjob )
coeftest(reg_heduc_fjob , vcov = vcovHC(reg_heduc_fjob , type="HC1"))
plot(reg_heduc_fjob)


reg_fjob_heduc =  glm(high_educ ~ first_job, family = binomial, data = hip_analysis_pool)
summary(reg_fjob_heduc)
coeftest(reg_fjob_heduc, vcov = vcovHC(reg_fjob_heduc, type="HC1"))
plot(reg_fjob_heduc)

stargazer(reg_heduc_fjob, reg_fjob_heduc, type = 'text')

rob_se_educ_job <- list(sqrt(diag(vcovHC(reg_heduc_fjob, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_fjob_heduc, type = "HC1"))))

stargazer(reg_heduc_fjob, reg_fjob_heduc, type = "text", se = rob_se_educ_job)



# Generate MFA on treatment -----------------------------------------------

reg_t1_MFA = lm(cbind(first_job, high_educ, card_score, normalize_WM, tot_score_CT, needle_score) ~ treat1, data = hip_analysis_pool)
summary(reg_t1_MFA)
coeftest(reg_t1_MFA, vcov = vcovHC(reg_t1_MFA, type="HC1"))
plot(reg_t1_MFA)

reg_t2_MFA = lm(cbind(first_job, high_educ, card_score, normalize_WM, tot_score_CT, needle_score) ~ treat2, data = hip_analysis_pool)
summary(reg_t2_MFA)
coeftest(reg_t2_MFA, vcov = vcovHC(reg_t2_MFA, type="HC1"))
plot(reg_t2_MFA)

reg_t2a_MFA = lm(cbind(first_job, high_educ, card_score, normalize_WM, tot_score_CT, needle_score) ~ treat2a, data = hip_analysis_pool)
summary(reg_t2a_MFA)
coeftest(reg_t2a_MFA, vcov = vcovHC(reg_t2a_MFA, type="HC1"))
plot(reg_t2a_MFA)

reg_t2b_MFA = lm(cbind(first_job, high_educ, card_score, normalize_WM, tot_score_CT, needle_score) ~ treat2b, data = hip_analysis_pool)
summary(reg_t2b_MFA)
coeftest(reg_t2b_MFA, vcov = vcovHC(reg_t2b_MFA, type="HC1"))
plot(reg_t2b_MFA)

reg_t12_MFA = lm(cbind(first_job, high_educ, card_score, normalize_WM, tot_score_CT, needle_score) ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12_MFA)
coeftest(reg_t12_MFA, vcov = vcovHC(reg_t12_MFA, type="HC1"))
plot(reg_t12_MFA)

reg_t12a_MFA = lm(cbind(first_job, high_educ, card_score, normalize_WM, tot_score_CT, needle_score) ~ treat1*treat2a, data = hip_analysis_pool)
summary(reg_t12a_MFA)
coeftest(reg_t12a_MFA, vcov = vcovHC(reg_t12a_MFA, type="HC1"))
plot(reg_t12a_MFA)

reg_t12b_MFA = lm(cbind(first_job, high_educ, card_score, normalize_WM, tot_score_CT, needle_score) ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_t12b_MFA)
coeftest(reg_t12b_MFA, vcov = vcovHC(reg_t12b_MFA, type="HC1"))
plot(reg_t12b_MFA)


# Guess Salary Bias (Entry, Medium, SP) as Outcome Variable -----------------------------------------------

reg_educ_GSB = lm(cbind(guess_entry_salary_bias, guess_salary_medium_bias, guess_salary_sp_bias) ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_GSB)
coeftest(reg_educ_GSB, vcov = vcovHC(reg_educ_GSB , type="HC1"))
plot(reg_educ_GSB)

reg_job_GSB = lm(cbind(guess_entry_salary_bias, guess_salary_medium_bias, guess_salary_sp_bias) ~ first_job, data = hip_analysis_pool)
summary(reg_job_GSB)
coeftest(reg_job_GSB, vcov = vcovHC(reg_job_GSB , type="HC1"))
plot(reg_job_GSB)

reg_t1_GSB = lm(cbind(guess_entry_salary_bias, guess_salary_medium_bias, guess_salary_sp_bias) ~ treat1, data = hip_analysis_pool)
summary(reg_t1_GSB)
coeftest(reg_t1_GSB, vcov = vcovHC(reg_t1_GSB , type="HC1"))
plot(reg_t1_GSB)

reg_t2_GSB =  lm(cbind(guess_entry_salary_bias, guess_salary_medium_bias, guess_salary_sp_bias) ~ treat2, data = hip_analysis_pool)
summary(reg_t2_GSB)
coeftest(reg_t2_GSB, vcov = vcovHC(reg_t2_GSB , type="HC1"))
plot(reg_t2_GSB)

reg_t2a_GSB =  lm(cbind(guess_entry_salary_bias, guess_salary_medium_bias, guess_salary_sp_bias) ~ treat2a, data = hip_analysis_pool)
summary(reg_t2a_GSB)
coeftest(reg_t2a_GSB, vcov = vcovHC(reg_t2a_GSB , type="HC1"))
plot(reg_t2a_GSB)

reg_t2b_GSB =  lm(cbind(guess_entry_salary_bias, guess_salary_medium_bias, guess_salary_sp_bias) ~ treat2b, data = hip_analysis_pool)
summary(reg_t2b_GSB)
coeftest(reg_t2b_GSB, vcov = vcovHC(reg_t2b_GSB , type="HC1"))
plot(reg_t2b_GSB)

reg_t12_GSB =  lm(cbind(guess_entry_salary_bias, guess_salary_medium_bias, guess_salary_sp_bias) ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12_GSB)
coeftest(reg_t12_GSB, vcov = vcovHC(reg_t12_GSB , type="HC1"))
plot(reg_t12_GSB)

reg_t12a_GSB =  lm(cbind(guess_entry_salary_bias, guess_salary_medium_bias, guess_salary_sp_bias) ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12a_GSB)
coeftest(reg_t12a_GSB, vcov = vcovHC(reg_t12a_GSB , type="HC1"))
plot(reg_t12a_GSB)

reg_t12b_GSB =  lm(cbind(guess_entry_salary_bias, guess_salary_medium_bias, guess_salary_sp_bias) ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_t12b_GSB)
coeftest(reg_t12b_GSB, vcov = vcovHC(reg_t12b_GSB , type="HC1"))
plot(reg_t12b_GSB)

reg_all_GSB = lm(cbind(guess_entry_salary_bias, guess_salary_medium_bias, guess_salary_sp_bias)~ treat1*treat2 + tot_score_CT + card_score + normalize_WM + first_job +high_educ, data = hip_analysis_pool)
summary(reg_all_GSB)
coeftest(reg_all_GSB, vcov = vcovHC(reg_all_GSB , type="HC1"))
plot(reg_all_GSB)

# Guess Salary Raw (Entry, Medium, SP) as Outcome Variable -----------------------------------------------

reg_educ_GSR = lm(cbind(guess_entry_salary_raw, guess_salary_medium_raw, guess_salary_sp_raw) ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_GSR)
coeftest(reg_educ_GSR, vcov = vcovHC(reg_educ_GSR , type="HC1"))
plot(reg_educ_GSR)

reg_job_GSR = lm(cbind(guess_entry_salary_raw, guess_salary_medium_raw, guess_salary_sp_raw) ~ first_job, data = hip_analysis_pool)
summary(reg_job_GSR)
coeftest(reg_job_GSR, vcov = vcovHC(reg_job_GSB , type="HC1"))
plot(reg_job_GSR)

reg_t1_GSR = lm(cbind(guess_entry_salary_raw, guess_salary_medium_raw, guess_salary_sp_raw) ~ treat1, data = hip_analysis_pool)
summary(reg_t1_GSR)
coeftest(reg_t1_GSR, vcov = vcovHC(reg_t1_GSR , type="HC1"))
plot(reg_t1_GSR)

reg_t2_GSR =  lm(cbind(guess_entry_salary_raw, guess_salary_medium_raw, guess_salary_sp_raw) ~ treat2, data = hip_analysis_pool)
summary(reg_t2_GSR)
coeftest(reg_t2_GSR, vcov = vcovHC(reg_t2_GSR , type="HC1"))
plot(reg_t2_GSR)

reg_t2a_GSR =  lm(cbind(guess_entry_salary_raw, guess_salary_medium_raw, guess_salary_sp_raw) ~ treat2a, data = hip_analysis_pool)
summary(reg_t2a_GSR)
coeftest(reg_t2a_GSR, vcov = vcovHC(reg_t2a_GSR , type="HC1"))
plot(reg_t2a_GSR)

reg_t2b_GSR =  lm(cbind(guess_entry_salary_raw, guess_salary_medium_raw, guess_salary_sp_raw) ~ treat2b, data = hip_analysis_pool)
summary(reg_t2b_GSR)
coeftest(reg_t2b_GSR, vcov = vcovHC(reg_t2b_GSR , type="HC1"))
plot(reg_t2b_GSR)

reg_t12_GSR =  lm(cbind(guess_entry_salary_raw, guess_salary_medium_raw, guess_salary_sp_raw) ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12_GSR)
coeftest(reg_t12_GSR, vcov = vcovHC(reg_t12_GSR , type="HC1"))
plot(reg_t12_GSR)

reg_t12a_GSR =  lm(cbind(guess_entry_salary_raw, guess_salary_medium_raw, guess_salary_sp_raw) ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12a_GSR)
coeftest(reg_t12a_GSR, vcov = vcovHC(reg_t12a_GSR , type="HC1"))
plot(reg_t12a_GSR)

reg_t12b_GSR =  lm(cbind(guess_entry_salary_raw, guess_salary_medium_raw, guess_salary_sp_raw) ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_t12b_GSR)
coeftest(reg_t12b_GSR, vcov = vcovHC(reg_t12b_GSR , type="HC1"))
plot(reg_t12b_GSR)

reg_all_GSR = lm(cbind(guess_entry_salary_raw, guess_salary_medium_raw, guess_salary_sp_raw) ~ treat1*treat2 + tot_score_CT + card_score + normalize_WM + first_job +high_educ, data = hip_analysis_pool)
summary(reg_all_GSR)
coeftest(reg_all_GSR, vcov = vcovHC(reg_all_GSR , type="HC1"))
plot(reg_all_GSR)


# Guess Salary Abs (Entry, Medium, SP) as Outcome Variable -----------------------------------------------

reg_educ_GSA = lm(cbind(guess_entry_salary_abs, guess_salary_medium_abs, guess_salary_sp_abs) ~ high_educ, data = hip_analysis_pool)
summary(reg_educ_GSA)
coeftest(reg_educ_GSA, vcov = vcovHC(reg_educ_GSA , type="HC1"))
plot(reg_educ_GSA)

reg_job_GSA = lm(cbind(guess_entry_salary_abs, guess_salary_medium_abs, guess_salary_sp_abs) ~ first_job, data = hip_analysis_pool)
summary(reg_job_GSA)
coeftest(reg_job_GSA, vcov = vcovHC(reg_job_GSA , type="HC1"))
plot(reg_job_GSA)

reg_t1_GSA = lm(cbind(guess_entry_salary_abs, guess_salary_medium_abs, guess_salary_sp_abs) ~ treat1, data = hip_analysis_pool)
summary(reg_t1_GSA)
coeftest(reg_t1_GSA, vcov = vcovHC(reg_t1_GSA , type="HC1"))
plot(reg_t1_GSA)

reg_t2_GSA =  lm(cbind(guess_entry_salary_abs, guess_salary_medium_abs, guess_salary_sp_abs) ~ treat2, data = hip_analysis_pool)
summary(reg_t2_GSA)
coeftest(reg_t2_GSA, vcov = vcovHC(reg_t2_GSA , type="HC1"))
plot(reg_t2_GSA)

reg_t2a_GSA =  lm(cbind(guess_entry_salary_abs, guess_salary_medium_abs, guess_salary_sp_abs) ~ treat2a, data = hip_analysis_pool)
summary(reg_t2a_GSA)
coeftest(reg_t2a_GSA, vcov = vcovHC(reg_t2a_GSA , type="HC1"))
plot(reg_t2a_GSA)

reg_t2b_GSA =  lm(cbind(guess_entry_salary_abs, guess_salary_medium_abs, guess_salary_sp_abs) ~ treat2b, data = hip_analysis_pool)
summary(reg_t2b_GSA)
coeftest(reg_t2b_GSA, vcov = vcovHC(reg_t2b_GSA , type="HC1"))
plot(reg_t2b_GSR)

reg_t12_GSA =  lm(cbind(guess_entry_salary_abs, guess_salary_medium_abs, guess_salary_sp_abs) ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12_GSA)
coeftest(reg_t12_GSA, vcov = vcovHC(reg_t12_GSA , type="HC1"))
plot(reg_t12_GSA)

reg_t12a_GSA =  lm(cbind(guess_entry_salary_abs, guess_salary_medium_abs, guess_salary_sp_abs) ~ treat1*treat2, data = hip_analysis_pool)
summary(reg_t12a_GSA)
coeftest(reg_t12a_GSA, vcov = vcovHC(reg_t12a_GSA , type="HC1"))
plot(reg_t12a_GSA)

reg_t12b_GSA =  lm(cbind(guess_entry_salary_abs, guess_salary_medium_abs, guess_salary_sp_abs) ~ treat1*treat2b, data = hip_analysis_pool)
summary(reg_t12b_GSA)
coeftest(reg_t12b_GSA, vcov = vcovHC(reg_t12b_GSA , type="HC1"))
plot(reg_t12b_GSA)

reg_all_GSA = lm(cbind(guess_entry_salary_abs, guess_salary_medium_abs, guess_salary_sp_abs) ~ treat1*treat2 + tot_score_CT + card_score + normalize_WM + first_job +high_educ, data = hip_analysis_pool)
summary(reg_all_GSA)
coeftest(reg_all_GSA, vcov = vcovHC(reg_all_GSA , type="HC1"))
plot(reg_all_GSA)


# Heterogeneity Analysis Create Variables  -------------------------------------------------------------------------

normalize_WM_low = data_MFA$normalize_WM[1:round(length(data_MFA$normalize_WM)/2)]
normalize_WM_high = data_MFA$normalize_WM[round((length(data_MFA$normalize_WM)/2)+1):length(data_MFA$normalize_WM)]

tot_score_CT_low = data_MFA$tot_score_CT[1:round(length(data_MFA$tot_score_CT)/2)]
tot_score_CT_high = data_MFA$tot_score_CT[round((length(data_MFA$tot_score_CT)/2)+1):length(data_MFA$tot_score_CT)]

card_score_low = data_MFA$card_score[1:round(length(data_MFA$card_score)/2)]
card_score_high = data_MFA$card_score[round((length(data_MFA$card_score)/2)+1):length(data_MFA$card_score)]

needle_score_low = data_MFA$needle_score[1:round(length(data_MFA$needle_score)/2)]
needle_score_high = data_MFA$needle_score[round((length(data_MFA$needle_score)/2)+1):length(data_MFA$needle_score)]

data_ha_var = data.frame(normalize_WM_low, normalize_WM_high, tot_score_CT_low, tot_score_CT_high, card_score_low, card_score_high, needle_score_low, needle_score_high)

data_ha_var$number <- row.names(data_ha_var)
hip_analysis_pool$number <- row.names(hip_analysis_pool)

data_HA <- merge(data_ha_var, hip_analysis_pool, by = "number", all = TRUE)

# Heterogeneity Analysis Regressions Start  -------------------------------------------------------------------------

hip_analysis_pool$tot_score_CT_dummy = ifelse(hip_analysis_pool$tot_score_CT>5, "1", "0")


# Total Cognitive Test for Low and High (tot_score_CT_lh) Score as Outcome Variable -----------------------------------------------


reg_educ_CT_lh = lm(cbind(tot_score_CT_low, tot_score_CT_high) ~ high_educ, data = data_HA)
summary(reg_educ_CT_lh)
coeftest(reg_educ_CT_lh, vcov = vcovHC(reg_educ_CT_lh , type="HC1"))
plot(reg_educ_CT_lh)

reg_job_CT_lh = lm(cbind(tot_score_CT_low, tot_score_CT_high) ~ first_job, data = data_HA)
summary(reg_job_CT_lh)
coeftest(reg_job_CT_lh, vcov = vcovHC(reg_job_CT_lh , type="HC1"))
plot(reg_job_CT_lh)

reg_t1_CT_lh = lm(cbind(tot_score_CT_low, tot_score_CT_high) ~ treat1, data = data_HA)
summary(reg_t1_CT_lh)
coeftest(reg_t1_CT_lh, vcov = vcovHC(reg_t1_CT_lh , type="HC1"))
plot(reg_t1_CT_lh)

reg_t2_CT_lh =  lm(cbind(tot_score_CT_low, tot_score_CT_high) ~ treat2, data = data_HA)
summary(reg_t2_CT_lh)
coeftest(reg_t2_CT_lh, vcov = vcovHC(reg_t2_CT_lh , type="HC1"))
plot(reg_t2_CT_lh)

reg_t2a_CT_lh =  lm(cbind(tot_score_CT_low, tot_score_CT_high) ~ treat2a, data = data_HA)
summary(reg_t2a_CT_lh)
coeftest(reg_t2a_CT_lh, vcov = vcovHC(reg_t2a_CT_lh , type="HC1"))
plot(reg_t2a_CT_lh)

reg_t2b_CT_lh =  lm(cbind(tot_score_CT_low, tot_score_CT_high) ~ treat2b, data = data_HA)
summary(reg_t2b_CT_lh)
coeftest(reg_t2b_CT_lh, vcov = vcovHC(reg_t2b_CT_lh , type="HC1"))
plot(reg_t2b_CT_lh)

reg_t12_CT_lh =  lm(cbind(tot_score_CT_low, tot_score_CT_high) ~ treat1*treat2, data = data_HA)
summary(reg_t12_CT_lh)
coeftest(reg_t12_CT_lh, vcov = vcovHC(reg_t12_CT_lh , type="HC1"))
plot(reg_t12_CT_lh)

reg_t12a_CT_lh =  lm(cbind(tot_score_CT_low, tot_score_CT_high) ~ treat1*treat2a, data = data_HA)
summary(reg_t12a_CT_lh)
coeftest(reg_t12a_CT_lh, vcov = vcovHC(reg_t12a_CT_lh , type="HC1"))
plot(reg_t12a_CT_lh)

reg_t12b_CT_lh =  lm(cbind(tot_score_CT_low, tot_score_CT_high) ~ treat1*treat2b, data = data_HA)
summary(reg_t12b_CT_lh)
coeftest(reg_t12b_CT_lh, vcov = vcovHC(reg_t12b_CT_lh , type="HC1"))
plot(reg_t12b_CT_lh)

reg_all_CT_lh = lm(cbind(tot_score_CT_low, tot_score_CT_high) ~ treat1*treat2 + normalize_WM + card_score +needle_score + first_job +high_educ, data = data_HA)
summary(reg_all_CT_lh)
coeftest(reg_all_CT_lh, vcov = vcovHC(reg_all_CT_lh , type="HC1"))
plot(reg_all_CT_lh)


# Normalize Working Memory for low and high (normalize_WM_lh) Score as Outcome Variable -----------------------------------------------

reg_educ_WM_lh = lm(cbind(normalize_WM_low, normalize_WM_high) ~ high_educ, data = data_HA)
summary(reg_educ_WM_lh)
coeftest(reg_educ_WM_lh, vcov = vcovHC(reg_educ_WM_lh , type="HC1"))
plot(reg_educ_WM_lh)

reg_job_WM_lh = lm(cbind(normalize_WM_low, normalize_WM_high) ~ first_job, data = data_HA)
summary(reg_job_WM_lh)
coeftest(reg_job_WM_lh, vcov = vcovHC(reg_job_WM_lh , type="HC1"))
plot(reg_job_WM_lh)

reg_t1_WM_lh = lm(cbind(normalize_WM_low, normalize_WM_high) ~ treat1, data = data_HA)
summary(reg_t1_WM_lh)
coeftest(reg_t1_WM_lh, vcov = vcovHC(reg_t1_WM_lh , type="HC1"))
plot(reg_t1_WM_lh)

reg_t2_WM_lh =  lm(cbind(normalize_WM_low, normalize_WM_high) ~ treat2, data = data_HA)
summary(reg_t2_WM_lh)
coeftest(reg_t2_WM_lh, vcov = vcovHC(reg_t2_WM_lh , type="HC1"))
plot(reg_t2_WM_lh)

reg_t2a_WM_lh =  lm(cbind(normalize_WM_low, normalize_WM_high) ~ treat2a, data = data_HA)
summary(reg_t2a_WM_lh)
coeftest(reg_t2a_WM_lh, vcov = vcovHC(reg_t2a_WM_lh , type="HC1"))
plot(reg_t2a_WM_lh)

reg_t2b_WM_lh =  lm(cbind(normalize_WM_low, normalize_WM_high) ~ treat2b, data = data_HA)
summary(reg_t2b_WM_lh)
coeftest(reg_t2b_WM_lh, vcov = vcovHC(reg_t2b_WM_lh , type="HC1"))
plot(reg_t2b_WM_lh)

reg_t12_WM_lh =  lm(cbind(normalize_WM_low, normalize_WM_high) ~ treat1*treat2, data = data_HA)
summary(reg_t12_WM_lh)
coeftest(reg_t12_WM_lh, vcov = vcovHC(reg_t12_WM_lh, type="HC1"))
plot(reg_t12_WM_lh)

reg_t12a_WM_lh =  lm(cbind(normalize_WM_low, normalize_WM_high) ~ treat1*treat2a, data = data_HA)
summary(reg_t12a_WM_lh)
coeftest(reg_t12_WM_lh, vcov = vcovHC(reg_t12a_WM_lh, type="HC1"))
plot(reg_t12a_WM_lh)

reg_t12b_WM_lh =  lm(cbind(normalize_WM_low, normalize_WM_high) ~ treat1*treat2b, data = data_HA)
summary(reg_t12b_WM_lh)
coeftest(reg_t12b_WM_lh, vcov = vcovHC(reg_t12b_WM_lh , type="HC1"))
plot(reg_t12b_WM_lh)

reg_all_WM_lh = lm(cbind(normalize_WM_low, normalize_WM_high) ~ treat1*treat2 + tot_score_CT + card_score +needle_score + first_job +high_educ, data = data_HA)
summary(reg_all_WM_lh)
coeftest(reg_all_WM_lh, vcov = vcovHC(reg_all_WM_lh , type="HC1"))
plot(reg_all_WM_lh)


# Card Score for low and high (card_score_lh) as Outcome Variable -----------------------------------------------


reg_educ_CS_lh = lm(cbind(card_score_low, card_score_high) ~ high_educ, data = data_HA)
summary(reg_educ_CS_lh)
coeftest(reg_educ_CS_lh, vcov = vcovHC(reg_educ_CS_lh , type="HC1"))
plot(reg_educ_CS_lh)

reg_job_CS_lh = lm(cbind(card_score_low, card_score_high)  ~ first_job, data = data_HA)
summary(reg_job_CS_lh)
coeftest(reg_job_CS_lh, vcov = vcovHC(reg_job_CS_lh , type="HC1"))
plot(reg_job_CS_lh)

reg_t1_CS_lh = lm(cbind(card_score_low, card_score_high)  ~ treat1, data = data_HA)
summary(reg_t1_CS_lh)
coeftest(reg_t1_CS_lh, vcov = vcovHC(reg_t1_CS_lh , type="HC1"))
plot(reg_t1_CS_lh)

reg_t2_CS_lh =  lm(cbind(card_score_low, card_score_high) ~ treat2, data = data_HA)
summary(reg_t2_CS_lh)
coeftest(reg_t2_CS_lh, vcov = vcovHC(reg_t2_CS_lh , type="HC1"))
plot(reg_t2_CS_lh)

reg_t2a_CS_lh =  lm(cbind(card_score_low, card_score_high) ~ treat2a, data = data_HA)
summary(reg_t2a_CS_lh)
coeftest(reg_t2a_CS_lh, vcov = vcovHC(reg_t2a_CS_lh , type="HC1"))
plot(reg_t2a_CS_lh)

reg_t2b_CS_lh =  lm(cbind(card_score_low, card_score_high) ~ treat2b, data = data_HA)
summary(reg_t2b_CS_lh)
coeftest(reg_t2b_CS_lh, vcov = vcovHC(reg_t2b_CS_lh , type="HC1"))
plot(reg_t2b_CS_lh)

reg_t12_CS_lh =  lm(cbind(card_score_low, card_score_high) ~ treat1*treat2, data = data_HA)
summary(reg_t12_CS_lh)
coeftest(reg_t12_CS_lh, vcov = vcovHC(reg_t12_CS_lh , type="HC1"))
plot(reg_t12_CS_lh)

reg_t12a_CS_lh =  lm(cbind(card_score_low, card_score_high) ~ treat1*treat2a, data = data_HA)
summary(reg_t12a_CS_lh)
coeftest(reg_t12_CS_lh, vcov = vcovHC(reg_t12a_CS_lh , type="HC1"))
plot(reg_t12a_CS_lh)

reg_t12b_CS_lh =  lm(cbind(card_score_low, card_score_high) ~ treat1*treat2b, data = data_HA)
summary(reg_t12b_CS_lh)
coeftest(reg_t12b_CS_lh, vcov = vcovHC(reg_t12b_CS_lh , type="HC1"))
plot(reg_t12b_CS_lh)

reg_all_CS_lh = lm(cbind(card_score_low, card_score_high) ~ treat1*treat2 + tot_score_CT + normalize_WM + needle_score + first_job +high_educ, data = data_HA)
summary(reg_all_CS_lh)
coeftest(reg_all_CS_lh, vcov = vcovHC(reg_all_CS_lh , type="HC1"))
plot(reg_all_CS_lh)


# Needle Score for low and high (needle_score) as Outcome Variable -----------------------------------------------


reg_educ_NS_lh = lm(cbind(needle_score_low, needle_score_high) ~ high_educ, data = data_HA)
summary(reg_educ_NS_lh)
coeftest(reg_educ_NS_lh, vcov = vcovHC(reg_educ_NS_lh , type="HC1"))
plot(reg_educ_NS_lh)

reg_job_NS_lh = lm(cbind(needle_score_low, needle_score_high) ~ first_job, data = data_HA)
summary(reg_job_NS_lh)
coeftest(reg_job_NS_lh, vcov = vcovHC(reg_job_NS_lh , type="HC1"))
plot(reg_job_NS_lh)

reg_t1_NS_lh = lm(cbind(needle_score_low, needle_score_high) ~ treat1, data = data_HA)
summary(reg_t1_NS_lh)
coeftest(reg_t1_NS_lh, vcov = vcovHC(reg_t1_NS_lh , type="HC1"))
plot(reg_t1_NS_lh)

reg_t2_NS_lh =  lm(cbind(needle_score_low, needle_score_high) ~ treat2, data = data_HA)
summary(reg_t2_NS_lh)
coeftest(reg_t2_NS_lh, vcov = vcovHC(reg_t2_NS_lh , type="HC1"))
plot(reg_t2_NS_lh)

reg_t2a_NS_lh =  lm(cbind(needle_score_low, needle_score_high) ~ treat2a, data = data_HA)
summary(reg_t2a_NS_lh)
coeftest(reg_t2a_NS_lh, vcov = vcovHC(reg_t2a_NS_lh , type="HC1"))
plot(reg_t2a_NS_lh)

reg_t2b_NS_lh =  lm(cbind(needle_score_low, needle_score_high) ~ treat2b, data = data_HA)
summary(reg_t2b_NS_lh)
coeftest(reg_t2b_NS_lh, vcov = vcovHC(reg_t2b_NS_lh , type="HC1"))
plot(reg_t2b_NS_lh)

reg_t12_NS_lh =  lm(cbind(needle_score_low, needle_score_high) ~ treat1*treat2, data = data_HA)
summary(reg_t12_NS_lh)
coeftest(reg_t12_NS_lh, vcov = vcovHC(reg_t12_NS_lh , type="HC1"))
plot(reg_t12_NS_lh)

reg_t12a_NS_lh =  lm(cbind(needle_score_low, needle_score_high) ~ treat1*treat2, data = data_HA)
summary(reg_t12a_NS_lh)
coeftest(reg_t12a_NS_lh, vcov = vcovHC(reg_t12a_NS_lh , type="HC1"))
plot(reg_t12a_NS_lh)

reg_t12b_NS_lh =  lm(cbind(needle_score_low, needle_score_high) ~ treat1*treat2b, data = data_HA)
summary(reg_t12b_NS_lh)
coeftest(reg_t12b_NS_lh, vcov = vcovHC(reg_t12b_NS_lh , type="HC1"))
plot(reg_t12b_NS_lh)

reg_all_NS_lh = lm(cbind(needle_score_low, needle_score_high) ~ treat1*treat2 + tot_score_CT + card_score + normalize_WM + first_job +high_educ, data = data_HA)
summary(reg_all_NS_lh)
coeftest(reg_all_NS_lh, vcov = vcovHC(reg_all_NS_lh , type="HC1"))
plot(reg_all_NS_lh)

























# The most important regression


reg_CT_GSA = lm(cbind(guess_entry_salary_bias, guess_salary_medium_bias, guess_salary_sp_bias) ~ tot_score_CT_dummy*treat12, data = hip_analysis_pool)
summary(reg_CT_GSA)
coeftest(reg_CT_GSA, vcov = vcovHC(reg_CT_GSA , type="HC1"))
plot(reg_educ_GSA)

