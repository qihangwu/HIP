
# Setup -------------------------------------------------------------------

source('code/outside_options.R')
source('code/jobs_in_HIP.R')
source('code/jobs_in_HIP_followup.R')
source('code/DM_JA_TR_M.R')
source('code/DM_WF_JH_E.R')
source('code/Type_AT_WM_M.R')
source('code/Grading_BT_CT_DX.R')
source('code/MFA.R')


# Merge -------------------------------------------------------------------

# so long as our cleaned data frames are in the original order
# we can merge very simply using `cbind`
# however, if we include `wid` in each cleaned data frame
# we can make the merge process more robust

hip_analysis <- hip %>%
  select(c('wid', 'treat1', 'treat2a', 'treat2b')) %>%
  cbind(hip_IC) %>%   # select only the salary variables?
  cbind(hip_ID) %>%
  cbind(hip_w_IC) %>%
  cbind(hip_w_ID) %>%
  cbind(data_DMm) %>%
  cbind(data_JH) %>%
  cbind(data_TS_WM) %>%
  cbind(data_CT) %>%
  cbind(data_DX) %>%
  cbind(data_MFA)


# Pooled ------------------------------------------------------------------

## No interaction ---------------------------------------------------------

hip_analysis_pool <- hip_analysis %>%
  mutate(treat2 = ifelse(treat2a == 1 | treat2b == 1, 1, 0),
         .after = treat2b)

# hip_analysis_pool %>% group_by(treat1, treat2) %>% count()
# there are 123 indivs. who got treat1 and treat2, do we exclude them?

## Interaction ------------------------------------------------------------

hip_analysis_pool_int <- hip_analysis %>%
  mutate(treat2 = ifelse(treat2a == 1 | treat2b == 1, 1, 0),
         .after = treat2b)

# identical to hip_analysis_pool


# Separated ---------------------------------------------------------------

hip_analysis_sep <- hip_analysis



