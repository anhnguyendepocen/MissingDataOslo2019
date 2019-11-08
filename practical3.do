cd "/Users/Jonathan/OneDrive - University of Bath/Courses/MissingDataOslo2019/Datasets/"

use nhanesMort.dta, clear

gen dead10 = 1*((dead==1) & (tMonths<120))
logistic dead10 i.gender age i.ethnicity sbp waist_circum weight total_chol hdl i.ALQ150
est store CCA

misstable patterns

mi set flong
mi register imputed sbp waist_circum weight total_chol hdl ALQ150

mi impute chained (regress) sbp waist_circum weight total_chol hdl (logit) ALQ150 = dead10 gender age i.ethnicity, add(10) rseed(61177)

mi estimate, post: logistic dead10 i.gender age i.ethnicity sbp waist_circum weight total_chol hdl i.ALQ150
est store MI
est table CCA MI,se

*check convergence
save prac3imps, replace
mi impute chained (regress) sbp waist_circum weight total_chol hdl (logit) ALQ150 = dead10 gender age i.ethnicity, burnin(100) rseed(61177) chainonly savetrace(impstats, replace)

use impstats, clear
scatter ALQ150_mean iter

use prac3imps, clear
midiagplots hdl

mi estimate, mcerror: logistic dead10 i.gender age i.ethnicity sbp waist_circum weight total_chol hdl i.ALQ150

mi extract 0, clear
mi set flong
mi register imputed sbp waist_circum weight total_chol hdl ALQ150
mi impute chained (regress) sbp waist_circum weight total_chol hdl (logit) ALQ150 = gender age i.ethnicity, add(10) rseed(61177)
mi estimate, post: logistic dead10 i.gender age i.ethnicity sbp waist_circum weight total_chol hdl i.ALQ150
est store MI2
est table CCA MI MI2
