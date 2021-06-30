
# Setup -------------------------------------------------------------------

library(stargazer)

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
  cbind(hip_IC[c(1:4, 6:9, 14:17)]) %>%
  cbind(hip_w_IC[c(1:4, 6:9, 14:17)]) %>%
  cbind(hip_ID[c(5:8, 10:13, 17:20)]) %>%
  cbind(hip_w_ID[c(5:8, 10:13, 17:20)]) %>%
  cbind(data_DMm) %>%
  cbind(data_JH) %>%
  cbind(data_TS_WM) %>%
  cbind(data_CT) %>%
  cbind(data_DX) #%>%
#  cbind(data_MFA)


# Pooled ------------------------------------------------------------------

hip_analysis_pool <- hip_analysis %>%
  mutate(treat2 = ifelse(treat2a == 1 | treat2b == 1, 1, 0),
         .after = treat2b)

## No interaction ---------------------------------------------------------



## Interaction ------------------------------------------------------------

gg_pooled <- function(variable, benchmark) {

  plot <- ggplot(data = hip_analysis_pool) +
    geom_density(aes(x = !!sym(variable)),
                 size = 0.8) +
    geom_density(aes(x = !!sym(paste0('w_', variable))),
                 size = 0.8,
                 color = 'cornflowerblue') +
    geom_vline(aes(xintercept = benchmark),
               color = 'red') +
    facet_grid(treat2 ~ treat1, labeller = 'label_both') +
    labs(title = variable,
         subtitle = 'Black is morning, blue is weekend',
         x = '') +
    theme_minimal()

  return(plot)
}

ggplot() +
  geom_boxplot(data = hip_analysis_pool %>% filter(treat1 == 0), aes(y = guess_entry_salary)) +
  geom_boxplot(data = hip_analysis_pool %>% filter(treat1 == 1), aes(y = guess_entry_salary))




hip_analysis_pool <- hip_analysis_pool %>%
  mutate(guess_entry_salary_diff = w_guess_entry_salary - guess_entry_salary,
         .after = guess_entry_salary_bias) %>%
  mutate(guess_salary_medium_diff = w_guess_salary_medium - guess_salary_medium,
         .after = guess_salary_medium_bias)

reg1 <- lm(guess_entry_salary_diff ~ treat1 + treat2, data = hip_analysis_pool)

reg2 <- lm(guess_salary_medium_diff ~ treat1 + treat2, data = hip_analysis_pool)

stargazer(reg1, type = 'text')

stargazer(reg2, type = 'text')

# Separated ---------------------------------------------------------------

hip_analysis_sep <- hip_analysis



