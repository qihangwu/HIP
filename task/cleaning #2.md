# Cleaning #2

## Salary guesses (Max)

1. Winsorize at 99 percentile: For all salary variables, if a value is higher than the 99 percentile, replace the value with p(99)

2. Clean the "1 percentile": Some variables might have absurdly low values (for example, in some salary question, they might put in "10"). You can put missing values there at the moment.

3. Generate bias variables: For each salary variable, generate three bias indicators.

   (a) Raw bias: simply calculate the diff between the guesses and the benchmark level. (Entry-level in the first month - 750; entry-level in 6 months - 1202; middle-level in 6 months - 1707; high-level in 6 months - 2857)

   (b) Absolute bias: the absolute value of raw bias divided by benchmark

   (c) Bias sign: let's generate three categories - 

   ​	(c.1) "Positively biased": raw bias is positive, and the guess is above at least 20% of benchmark (that is, for entry-level first month, we call it "positively biased" if the worker guesses more than 900)

   ​	(c.2) "Negatively biased": raw bias is negative, and the guess is below at least 20% of benchmark (that is, for entry-level first month, we call it "negatively biased" if the worker guesses less than 600)

   ​	(c.3) "Non-biased": the guess is within 20% of the benchmark (that is, for entry-level first month, we call it "non-biased" if the worker guesses between 600 and 900)

   ​	We will for sure test different thresholds (20%, 15%, 10%, etc). You can also generate different bias signs using different thresholds. Ideally, the thresholds would be determined after we look at (b) absolute bias.

## Types of workers (Ezana)

1. Let's generate a few more variables from the data - 

   (a) higheduc: Whether workers have attended more than 10th grade education

   (b) firstjob (history_yesno): Whether this is your first job (we already got this variable cleaned - just to list it here)

   (c) memory: The total score in working memory test. I think we can first generate total number of correct answers from each series; each worker was tested only one series. Then we can take the minimal number from the three series and generate a new variable "memory". Then normalize this variable.

   (d) cognitive: The total score of cognitive tests. We can simply count how many correct answers each worker has. Then normalize this variable.

   (e) dexterity: Let me know if you think this measure makes sense to you - 

   ​	(e.1) Card test (cardscore). We calculate how many cards each worker can deal with in a second. If the quality is G, then we divide 25 by the total amount of time the worker uses. If the quality is O, then we divide 22.5 by time. If the quality if B, we divide 20 by time. Normalize this variable at last.

   ​	(e.2) Needle test (needlescore). We calculate how many needles each worker can deal with in a second. If the quality is G, then we divide 2.5 by the total amount of time. If the quality is O, then we divide 1.5 by time. If the quality if B, we divide 0.5 by time. Normalize this variable at last.

2. I'm not sure if you can do this easily in R, so maybe you can google first how to do principal component analysis (PCA) in R. Basically, we have 6 measures of workers' types: higheduc, firstjob, memory, cognitive, cardscore, needlescore. PCA can generate a normalized score to capture "common" factors amongst these 6 variables. This can be a good holistic measure of worker's type. 
