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


# put all variables in one regression -----------------------------------------------


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


stargazer(reg_t1_MFA, reg_t2_MFA, reg_t2a_MFA, reg_t2b_MFA, reg_t12_MFA, reg_t12a_MFA, reg_t12b_MFA, type = 'text')

rob_se_MFA <- list(sqrt(diag(vcovHC(reg_t1_MFA, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t2_MFA, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t2a_MFA, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t2b_MFA, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t12_MFA, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t12a_MFA, type = "HC1"))),
                  sqrt(diag(vcovHC(reg_t12b_MFA, type = "HC1"))))

stargazer(reg_t1_MFA, reg_t2_MFA, reg_t2a_MFA, reg_t2b_MFA, reg_t12_MFA, reg_t12a_MFA, reg_t12b_MFA, type = "text", se = rob_se_MFA)
