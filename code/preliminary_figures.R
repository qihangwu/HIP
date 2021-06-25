
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


