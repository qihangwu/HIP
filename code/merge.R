
# Setup -------------------------------------------------------------------

source('code/outside_options.R')
source('code/jobs_in_HIP.R')
source('code/jobs_in_HIP_followup.R')


# Merge -------------------------------------------------------------------

# so long as our cleaned data frames are in the original order
# we can merge very simply using `cbind`
# however, if we include `wid` in each cleaned data frame
# we can make the merge process more robust

hip_analysis <- hip %>%
  select(c('wid', 'treat1', 'treat2a', 'treat2b')) %>%
  cbind(hip_IC) %>%   # select only the salary variables?
  cbind(hip_ID)
