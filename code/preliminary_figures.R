
# Setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)

source('code/outside_options.R')
source('code/jobs_in_HIP.R')
source('code/jobs_in_HIP_followup.R')


# OA (morning) ------------------------------------------------------------

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_salary), binwidth = 500) +
  ggsave('figures/outside_options/OA1.png')

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_hour), binwidth = 1) +
  ggsave('figures/outside_options/OA2.png')

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_day), binwidth = 1) +
  ggsave('figures/outside_options/OA3.png')

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_extra), binwidth = 1) +
  ggsave('figures/outside_options/OA4.png')

ggplot(data = hip_OA) +
  geom_histogram(aes(x = guess_out_night), binwidth = 1) +
  ggsave('figures/outside_options/OA4b.png')

ggplot(data = hip_OA) +
  geom_bar(aes(x = guess_out_transp)) +
  ggsave('figures/outside_options/OA5.png')

ggplot(data = hip_OA) +
  geom_bar(aes(x = guess_out_lunch)) +
  ggsave('figures/outside_options/OA6.png')

ggplot(data = hip_OA) +
  geom_bar(aes(x = guess_out_attend)) +
  ggsave('figures/outside_options/OA7.png')

ggplot(data = hip_OA_ms) +
  geom_histogram(aes(x = minimal_salary), binwidth = 100) +
  ggsave('figures/outside_options/OA8.png')


# OB (morning) ------------------------------------------------------------

ggplot(data = hip_OB) +
  geom_histogram(aes(x = guess_out_salary_1y), binwidth = 500) +
  ggsave('figures/outside_options/OB2.png')

ggplot(data = hip_OB) +
  geom_bar(aes(x = guess_out_salary_1y_com)) +
  ggsave('figures/outside_options/OB2c.png')

ggplot(data = hip_OB) +
  geom_histogram(aes(x = guess_out_promote), binwidth = 10) +
  ggsave('figures/outside_options/OB3.png')

ggplot(data = hip_OB) +
  geom_histogram(aes(x = guess_out_salary_super), binwidth = 500) +
  ggsave('figures/outside_options/OB4.png')

ggplot(data = hip_OB) +
  geom_bar(aes(x = guess_out_promote_com)) +
  ggsave('figures/outside_options/OB4c.png')


# OC (morning) ------------------------------------------------------------

ggplot(data = hip_OC) +
  geom_bar(aes(x = jobaspect_first)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  ggsave('figures/outside_options/OC1i.png')

ggplot(data = hip_OC) +
  geom_bar(aes(x = jobaspect_second)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  ggsave('figures/outside_options/OC1ii.png')

ggplot(data = hip_OC) +
  geom_bar(aes(x = jobaspect_third)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  ggsave('figures/outside_options/OC1iii.png')

# no figures for `jobaspect_*_other` variables since other responses never provided


# IA (morning) ------------------------------------------------------------

ggplot(data = hip_IA) +
  geom_bar(aes(x = guess_hip_task)) +
  ggsave('figures/jobs_in_hip/IA1.png')

ggplot(data = hip_IA) +
  geom_histogram(aes(x = hip_knowppl), binwidth = 5) +
  ggsave('figures/jobs_in_hip/IA2.png')

ggplot(data = hip_IA) +
  geom_bar(aes(x = hip_knowppl_like)) +
  ggsave('figures/jobs_in_hip/IA3.png')

ggplot(data = hip_IA) +
  geom_histogram(aes(x = guess_hip_turnover), binwidth = 5) +
  ggsave('figures/jobs_in_hip/IA5.png')

ggplot(data = hip_IA) +
  geom_bar(aes(x = guess_hip_turnover_sure)) +
  ggsave('figures/jobs_in_hip/IA5s.png')


# IB (morning) ------------------------------------------------------------

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_hour), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB1.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_hour_sure)) +
  ggsave('figures/jobs_in_HIP/IB1s.png')

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_day), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB2.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_day_sure)) +
  ggsave('figures/jobs_in_HIP/IB2s.png')

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_extra), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB3.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_extra_sure)) +
  ggsave('figures/jobs_in_HIP/IB3s.png')

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_night), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB4.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_night_sure)) +
  ggsave('figures/jobs_in_HIP/IB4s.png')

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_transp), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB5.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_transp_sure)) +
  ggsave('figures/jobs_in_HIP/IB5s.png')

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_lunch), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB6.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_lunch_sure)) +
  ggsave('figures/jobs_in_HIP/IB6s.png')

ggplot(data = hip_IB) +
  geom_histogram(aes(x = guess_hip_attend), binwidth = 1) +
  ggsave('figures/jobs_in_HIP/IB7.png')

ggplot(data = hip_IB) +
  geom_bar(aes(x = guess_hip_attend_sure)) +
  ggsave('figures/jobs_in_HIP/IB7s.png')


# IC (morning) ------------------------------------------------------------

ggplot(data = hip_IC) +
  geom_histogram(aes(x = guess_entry_salary), binwidth = 200) +
  ggsave('figures/jobs_in_HIP/IC1.png')

ggplot(data = hip_IC) +
  geom_bar(aes(x = guess_entry_salary_sure)) +
  ggsave('figures/jobs_in_HIP/IC1s.png')

ggplot(data = hip_IC) +
  geom_histogram(aes(x = guess_entry_salary_6m), binwidth = 500) +
  ggsave('figures/jobs_in_HIP/IC2.png')

ggplot(data = hip_IC) +
  geom_bar(aes(x = guess_entry_salary_6m_sure)) +
  ggsave('figures/jobs_in_HIP/IC2s.png')

ggplot(data = hip_IC) +
  geom_histogram(aes(x = guess_entry_pct), binwidth = 10) +
  ggsave('figures/jobs_in_HIP/IC3.png')

ggplot(data = hip_IC) +
  geom_bar(aes(x = guess_entry_pct_sure)) +
  ggsave('figures/jobs_in_HIP/IC3s.png')

ggplot(data = hip_IC) +
  geom_bar(aes(x = guess_entry_pct_you)) +
  ggsave('figures/jobs_in_HIP/IC4.png')

ggplot(data = hip_IC) +
  geom_histogram(aes(x = guess_you_salary_1m), binwidth = 200) +
  ggsave('figures/jobs_in_HIP/IC5.png')


# ID (morning) ------------------------------------------------------------

ggplot(data = hip_ID) +
  geom_histogram(aes(x = guess_promote_medium), binwidth = 10) +
  ggsave('figures/jobs_in_HIP/ID1.png')

ggplot(data = hip_ID) +
  geom_bar(aes(x = guess_promote_medium_sure)) +
  ggsave('figures/jobs_in_HIP/ID1s.png')

ggplot(data = hip_ID) +
  geom_histogram(aes(x = guess_promote_sp), binwidth = 10) +
  ggsave('figures/jobs_in_HIP/ID2.png')

ggplot(data = hip_ID) +
  geom_bar(aes(x = guess_promote_sp_sure)) +
  ggsave('figures/jobs_in_HIP/ID2s.png')

ggplot(data = hip_ID) +
  geom_histogram(aes(x = guess_salary_medium), binwidth = 500) +
  ggsave('figures/jobs_in_HIP/ID3.png')

ggplot(data = hip_ID) +
  geom_bar(aes(x = guess_salary_medium_sure)) +
  ggsave('figures/jobs_in_HIP/ID3s.png')

ggplot(data = hip_ID) +
  geom_histogram(aes(x = guess_salary_sp), binwidth = 500) +
  ggsave('figures/jobs_in_HIP/ID4.png')

ggplot(data = hip_ID) +
  geom_bar(aes(x = guess_salary_sp_sure)) +
  ggsave('figures/jobs_in_HIP/ID4s.png')

ggplot(data = hip_ID) +
  geom_bar(aes(x = guess_you_promote_medium)) +
  ggsave('figures/jobs_in_HIP/ID5.png')

ggplot(data = hip_ID) +
  geom_bar(aes(x = guess_you_promote_sp)) +
  ggsave('figures/jobs_in_HIP/ID6.png')

ggplot(data = hip_ID) +
  geom_histogram(aes(x = guess_you_salary_6m), binwidth = 500) +
  ggsave('figures/jobs_in_HIP/ID7.png')


# IT (morning) ------------------------------------------------------------

ggplot(data = hip_IT_entry) +
  geom_histogram(aes(x = info_entry_choice), binwidth = 20) +
  ggsave('figures/jobs_in_HIP/IT1.png')

ggplot(data = hip_IT_promote) +
  geom_histogram(aes(x = info_promote_choice), binwidth = 20) +
  ggsave('figures/jobs_in_HIP/IT2.png')


# IB (weekend) ------------------------------------------------------------

w_IB_codes <- dict %>%
  filter(subcategory == 'IB_w') %>%
  pull(code)

for (i in seq(1, 13, 2)) {
  temp <- ggplot(data = hip_w_IB) +
    geom_histogram(aes_string(x = w_IB_names[i]), binwidth = 1)

  ggsave(plot = temp, file = paste0('figures/jobs_in_HIP_followup/w_', w_IB_codes[i], '.png'))
}

for (i in seq(2, 14, 2)) {
  temp <- ggplot(data = hip_w_IB) +
    geom_bar(aes_string(x = w_IB_names[i]))

  ggsave(plot = temp, file = paste0('figures/jobs_in_HIP_followup/w_', w_IB_codes[i], '.png'))
}

# IC (weekend) ------------------------------------------------------------

ggplot(data = hip_w_IC) +
  geom_histogram(aes(x = w_guess_entry_salary), binwidth = 100) +
  ggsave('figures/jobs_in_HIP_followup/w_IC1.png')

ggplot(data = hip_w_IC) +
  geom_bar(aes(x = w_guess_entry_salary_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_IC1s.png')

ggplot(data = hip_w_IC) +
  geom_histogram(aes(x = w_guess_entry_salary_6m), binwidth = 500) +
  ggsave('figures/jobs_in_HIP_followup/w_IC2.png')

ggplot(data = hip_w_IC) +
  geom_bar(aes(x = w_guess_entry_salary_6m_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_IC2s.png')

ggplot(data = hip_w_IC) +
  geom_histogram(aes(x = w_guess_entry_pct), binwidth = 10) +
  ggsave('figures/jobs_in_HIP_followup/w_IC3.png')

ggplot(data = hip_w_IC) +
  geom_bar(aes(x = w_guess_entry_pct_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_IC3s.png')

ggplot(data = hip_w_IC) +
  geom_bar(aes(x = w_guess_entry_pct_you)) +
  ggsave('figures/jobs_in_HIP_followup/w_IC4.png')

ggplot(data = hip_w_IC) +
  geom_histogram(aes(x = w_guess_you_salary_1m), binwidth = 200) +
  ggsave('figures/jobs_in_HIP_followup/w_IC5.png')


# ID (weekend) ------------------------------------------------------------

ggplot(data = hip_w_ID) +
  geom_histogram(aes(x = w_guess_promote_medium), binwidth = 10) +
  ggsave('figures/jobs_in_HIP_followup/w_ID1.png')

ggplot(data = hip_w_ID) +
  geom_bar(aes(x = w_guess_promote_medium_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_ID1s.png')

ggplot(data = hip_w_ID) +
  geom_histogram(aes(x = w_guess_promote_sp), binwidth = 10) +
  ggsave('figures/jobs_in_HIP_followup/w_ID2.png')

ggplot(data = hip_w_ID) +
  geom_bar(aes(x = w_guess_promote_sp_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_ID2s.png')

ggplot(data = hip_w_ID) +
  geom_histogram(aes(x = w_guess_salary_medium), binwidth = 500) +
  ggsave('figures/jobs_in_HIP_followup/w_ID3.png')

ggplot(data = hip_w_ID) +
  geom_bar(aes(x = w_guess_salary_medium_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_ID3s.png')

ggplot(data = hip_w_ID) +
  geom_histogram(aes(x = w_guess_salary_sp), binwidth = 500) +
  ggsave('figures/jobs_in_HIP_followup/w_ID4.png')

ggplot(data = hip_w_ID) +
  geom_bar(aes(x = w_guess_salary_sp_sure)) +
  ggsave('figures/jobs_in_HIP_followup/w_ID4s.png')

ggplot(data = hip_w_ID) +
  geom_bar(aes(x = w_guess_you_promote_medium)) +
  ggsave('figures/jobs_in_HIP_followup/w_ID5.png')

ggplot(data = hip_w_ID) +
  geom_bar(aes(x = w_guess_you_promote_sp)) +
  ggsave('figures/jobs_in_HIP_followup/w_ID6.png')

ggplot(data = hip_w_ID) +
  geom_histogram(aes(x = w_guess_you_salary_6m), binwidth = 500) +
  ggsave('figures/jobs_in_HIP_followup/w_ID7.png')


# IA (weekend) ------------------------------------------------------------

w_IA_codes <- dict %>%
  filter(subcategory == 'IA_w') %>%
  pull(code)

for (i in 1:15) {
  temp <- ggplot(data = hip_w_IA) +
    geom_bar(aes_string(x = w_IA_names[i]))

  ggsave(plot = temp, file = paste0('figures/jobs_in_HIP_followup/w_', w_IA_codes[i], '.png'))
}


# DM_m (morning) ------------------------------------------------------------

ggplot(data = data_DMm) +
  geom_histogram(aes(x = age), binwidth=1) +
  xlim(15,30) + 
  ggsave('DM1.png')

ggplot(data = data_DMm) +
  geom_histogram(aes(x = new_age), binwidth=1) +
  xlim(15,30) + 
  ggsave('DM1newage.png')

ggplot(data = data_DMm) +
  geom_histogram(aes(x = year_birth), binwidth=1) +
  ggsave('DM1a.png')

ggplot(data = data_DMm) +
  geom_histogram(aes(x = month_birth), binwidth=1) + 
  ggsave('DM1b.png')

ggplot(data = data_DMm) +
  geom_bar(aes(x = married), binwidth=1) +
  ggsave('DM2.png')

ggplot(data = data_DMm) +
  geom_bar(aes(x = educ)) +
  ggsave('DM3.png')

ggplot(data = data_DMm) +
  geom_bar(aes(x = school_yesno)) +
  ggsave('DM3a.png')

ggplot(data = data_DMm) +
  geom_bar(aes(x = school_diploma)) +
  ggsave('DM3c.png')

ggplot(data = data_DMm) +
  geom_histogram(aes(x = school_fee), binwidth=1000) +
  ggsave('DM3d.png')


# JA (morning) ------------------------------------------------------------

ggplot(data = data_JA) +
  geom_bar(aes(x = reason_salary)) +
  ggsave('JA1a.png')

ggplot(data = data_JA) +
  geom_bar(aes(x = reason_interest)) +
  ggsave('JA1b.png')

ggplot(data = data_JA) +
  geom_bar(aes(x = reason_tiring)) +
  ggsave('JA1c.png')

ggplot(data = data_JA) +
  geom_bar(aes(x = reason_learn)) +
  ggsave('JA1d.png')

ggplot(data = data_JA) +
  geom_bar(aes(x = reason_style)) +
  ggsave('JA1e.png')

ggplot(data = data_JA) +
  geom_bar(aes(x = reason_quickjob)) +
  ggsave('JA1f.png')

ggplot(data = data_JA) +
  geom_bar(aes(x = reason_family)) +
  ggsave('JA1g.png')

ggplot(data = data_JA) +
  geom_bar(aes(x = reason_school)) +
  ggsave('JA1h.png')

ggplot(data = data_JA) +
  geom_histogram(aes(x = plan_year), binwidth = 1) +
  ggsave('JA2a.png')

ggplot(data = data_JA) +
  geom_histogram(aes(x = plan_month), binwidth = 5) +
  ggsave('JA2b.png')

ggplot(data = data_JA) +
  geom_histogram(aes(x = plan_school_year), binwidth = 1) +
  ggsave('JA3a.png')

ggplot(data = data_JA) +
  geom_histogram(aes(x = plan_school_month), binwidth = 2) +
  ggsave('JA3b.png')

ggplot(data = data_JA) +
  geom_bar(aes(x = plan_marry)) +
  ggsave('JA4.png')

ggplot(data = data_JA) +
  geom_bar(aes(x = plan_startown)) +
  ggsave('JA6.png')

ggplot(data = data_JA) +
  geom_bar(aes(x = plan_migrate)) +
  ggsave('JA7.png')

ggplot(data = data_JA) +
  geom_bar(aes(x = plan_migrate_home)) +
  ggsave('JA8.png')

ggplot(data = data_JA) +
  geom_histogram(aes(x = income_target), binwidth = 100) +
  ggsave('WF12.png')

ggplot(data = data_JA) +
  geom_histogram(aes(x = expend_target), binwidth = 100) +
  ggsave('WF13.png')


# TR (morning) ------------------------------------------------------------

ggplot(data = data_TR) +
  geom_bar(aes(x = trust_anoctr)) +
  ggsave('TR1.png')

ggplot(data = data_TR) +
  geom_bar(aes(x = trust_samelang)) +
  ggsave('TR2.png')

ggplot(data = data_TR) +
  geom_bar(aes(x = trust_difflang)) +
  ggsave('TR3.png')

ggplot(data = data_TR) +
  geom_bar(aes(x = trust_govt)) +
  ggsave('TR4.png')

ggplot(data = data_TR) +
  geom_bar(aes(x = trust_ethres)) +
  ggsave('TR5.png')

ggplot(data = data_TR) +
  geom_bar(aes(x = trust_ancres)) +
  ggsave('TR6.png')


# WF (evening) ------------------------------------------------------------

ggplot(data = data_WF) +
  geom_bar(aes(x = family_told_job)) +
  ggsave('WF1fa.png')

ggplot(data = data_WF) +
  geom_bar(aes(x = friend_told_job)) +
  ggsave('WF1fr.png')

ggplot(data = data_WF) +
  geom_bar(aes(x = search_job_other)) +
  ggsave('WF1o.png')

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


# JH (evening) ------------------------------------------------------------

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


# AT (evening) ------------------------------------------------------------

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


# WM (evening) ------------------------------------------------------------

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


# A11-A36 (Grading - Behavioral Test (BT)) 


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


# B1-B12 (Grading - Cognitive Test (CT)) ------------------------------------------------------------

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


# B1-B12 (Grading - Dexterity (DX)) ------------------------------------------------------------

ggplot(data = data_DX) +
  geom_bar(aes(x = card_qual)) +
  ggsave('DX1.png')

ggplot(data = data_DX) +
  geom_bar(aes(x = needle_qual)) +
  ggsave('DX2.png')
