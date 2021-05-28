# notes to self

### research questions

remember that our objective is to answer the 2 research questions:

1. Does an information treatment of promotion incentives help workers update beliefs of
jobs in the industrial park?
2. Do misperceptions, especially of long-run promotion incentives, affect the turnover
decisions for high-skill and low-skill workers?


### variables

keep in mind question of which variables are relevant!

3 categories:

1. background information for workers

2. information of jobs

OA: simple data dictionary

| code | name             | type    | meaning                                                                   | question                                                                                                                                |
|------|------------------|---------|---------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------|
| OA1  | guess_out_salary | integer | monthly salary in birr                                                    | For an average worker, what would be the monthly salary in the first month?                                                             |
| OA2  | guess_out_hour   | integer | hours per day                                                             | How many hours per day do workers need to work on average?                                                                              |
| OA3  | guess_out_day    | integer | days per week                                                             | How many days per week do workers need to work on average?                                                                              |
| OA4  | guess_out_extra  | integer | overtime hours per week                                                   | How many overtime hours per week do workers need to work on average?                                                                    |
| OA4b | guess_out_night  | integer | night shifts per month                                                    | How many night shifts per month do workers need to work on average?                                                                     |
| OA5  | guess_out_transp | factor  | 0 = likely, 1 = somewhat likely, 2 = somewhat unlikely, 3 = very unlikely | Would employer provide free transportation outside of industrial park?                                                                  |
| OA6  | guess_out_lunch  | factor  | 0 = likely, 1 = somewhat likely, 2 = somewhat unlikely, 3 = very unlikely | Would employer provide free breakfast or lunch outside of Industrial park?                                                              |
| OA7  | guess_out_attend | factor  | 0 = likely, 1 = somewhat likely, 2 = somewhat unlikely, 3 = very unlikely | Would employer provide additional income if you don't miss any day of work outside of industrial park?                                  |
| OA8a | minimal_salary_a | logical | 0 = no, 1 = yes                                                           | If you are offered a new job outside of the industrial park, and the monthly salary of this job is 600 birr, would you accept this job? |
| OA8b | minimal_salary_b | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 700 birr?                                                                                                 |
| OA8c | minimal_salary_c | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 800 birr?                                                                                                 |
| OA8d | minimal_salary_d | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 900 birr?                                                                                                 |
| OA8e | minimal_salary_e | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 1000 birr?                                                                                                |
| OA8f | minimal_salary_f | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 1100 birr?                                                                                                |
| OA8g | minimal_salary_g | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 1200 birr?                                                                                                |
| OA8h | minimal_salary_h | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 1300 birr?                                                                                                |
| OA8i | minimal_salary_i | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 1400 birr?                                                                                                |
| OA8j | minimal_salary_j | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 1500 birr?                                                                                                |
| OA8k | minimal_salary_k | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 1600 birr?                                                                                                |
| OA8l | minimal_salary_l | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 1700 birr?                                                                                                |
| OA8m | minimal_salary_m | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 1800 birr?                                                                                                |
| OA8n | minimal_salary_n | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 1900 birr?                                                                                                |
| OA8o | minimal_salary_o | logical | 0 = no, 1 = yes                                                           | What if the monthly salary is 2000 birr?                                                                                                |

3. additional questions


### miscellaneous notes

- how best to store data for multiple-select questions?
