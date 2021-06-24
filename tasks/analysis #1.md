# Analysis #1

1. For the moment, let's focus on three main sets of cleaned variables:

   a) Perceptions of salary (IC and ID), computed bias - Max

   b) Types (high_educ, firstjob, cognitive test, working memory, dexterity, MFA)  - Ezana

   c) Treatment status: treat1, treat2a, treat2b. 

   Make sure wid is the only identifier of all data frames. If you have time, it would be great to put all the cleaning together and generate a new file for future use. 

   

2. Treatment status: This is one of the most important variables. There are a few ways we can group different treatments. 

   a) Pooled, no interaction: First, pool treat2a and treat2b into one group ("treat2"). Then we focus on 3 groups: control, treat1, treat2. (Notice there's an overlap between treat1 and treat2).

   b) Pooled, interaction: We focus on 4 groups: control, treat1, treat2, treat12. ("treat12" meaning the workers get both treatment 1 and treatment 2)

   c) Separated, no interaction: We do not pool treat2a and treat2b together. We then focus on 4 groups: control, treat1, treat2a, treat2b. (Notice there's an overlap between treat1 and treat2a, also between treat1 and treat2b)

   d)  Separated, interaction: We focus on 6 groups: control, treat1, treat2a, treat12a, treat2b, treat12b. 

   

3. Viz #1: Do different treatments affect bias update?

   For each treatment group, compare bias before and after workers start working. 

   a) Try different ways to group different treatments as described above.

   b) Try different bias definitions.

   c) The graph scheme ideally can be applied to all these graphs.

   

4. Viz #2: Are different treatments balanced on types?

   Compare types across different groups and see if there's any major difference. 

   a) Try different ways to group different treatments as described above.

   b) You can already try to run a regression and see how significant the differences are.

   

5. Viz #3: Do different types affect bias update?

   For each type, compare bias before and after workers start working.

   a) Try different ways to group different treatments as described above.

   b) For each definition of type, some of them are categorical variables (high_educ, firstjob). For continuous variables, you can define "high type" (above the median or average) and "low type" (below the median or average). 