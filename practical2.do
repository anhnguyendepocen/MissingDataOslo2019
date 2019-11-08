cd "/Users/Jonathan/OneDrive - University of Bath/Courses/MissingDataOslo2019/Datasets/"

use nhanesMort.dta, clear

summ

proportion ALQ150

gen rALQ=1*(ALQ150!=.)
logistic rALQ age i.ethnicity i.gender

logistic ALQ150 age i.ethnicity i.gender

mi set wide
mi register imputed ALQ150

mi impute logit ALQ150 age ethnicity gender, add(10) rseed(79324)
label define yesno 1 yes 0 no
label value ALQ150 yesno

mi impute logit ALQ150 age ethnicity gender, add(10) rseed(79324)
mi estimate: proportion ALQ150

*part 2
summ tMonths if dead==0

gen dead10 = 1*((dead==1) & (tMonths<120))

logistic dead10 i.gender age i.ethnicity sbp waist_circum weight total_chol hdl i.ALQ150

gen cc = 1*(e(sample)==1)
logistic cc dead10

logistic cc dead10 i.gender age i.ethnicity

logistic rALQ dead10 i.gender age i.ethnicity sbp waist_circum weight total_chol hdl
