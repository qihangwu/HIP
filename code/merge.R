
# Setup -------------------------------------------------------------------

source('code/outside_options.R')
source('code/jobs_in_HIP.R')
source('code/jobs_in_HIP_followup.R')
source('code/DM_JA_TR_M.R')
source('code/DM_WF_JH_E.R')
source('code/Type_AT_WM_M.R')
source('code/Grading_BT_CT_DX.R')


# Merge -------------------------------------------------------------------

# so long as our cleaned data frames are in the original order
# we can merge very simply using `cbind`
# however, if we include `wid` in each cleaned data frame
# we can make the merge process more robust

hip_analysis <- hip %>%
  select(c('wid', 'today_day', 'firm', 'treat1', 'treat2a', 'treat2b')) %>%
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

# write.csv(hip_analysis, 'data/analysis.csv')


