use "nhanesMort.dta", clear

reg sbp i.gender age i.ethnicity waist_circum weight total_chol hdl i.ALQ150 c.waist_circum#i.ALQ150
est store CCA

mi set flong
mi register imputed sbp waist_circum weight total_chol hdl ALQ150
mi impute chained (reg) sbp waist_circum weight total_chol hdl (logit) ALQ150, add(10) rseed(21466)

mi estimate, post: reg sbp i.gender age i.ethnicity waist_circum weight /*
*/ total_chol hdl i.ALQ150 c.waist_circum#i.ALQ150
est store MIimpTransform
est table CCA MIimpTransform

mi extract 0, clear
mi set flong
mi register imputed sbp waist_circum weight total_chol hdl ALQ150
mi impute chained (reg, include((waist_circum*ALQ150))) sbp /*
*/ (reg) waist_circum weight total_chol hdl (logit) ALQ150, add(10) rseed(21466)
	
mi estimate, post: reg sbp i.gender age i.ethnicity waist_circum weight /*
*/ total_chol hdl i.ALQ150 c.waist_circum#i.ALQ150
est store MIpassive

est table MIimpTransform MIpassive

ssc install smcfcs, replace

mi extract 0, clear
mi set flong
set seed 34412
gen waistALQ = waist_circum*ALQ150
smcfcs reg sbp i.gender age i.ethnicity waist_circum weight total_chol hdl /*
*/  ALQ150 waistALQ, /*
*/ reg(sbp waist_circum weight total_chol hdl) logit(ALQ150) /*
*/ passive(waistALQ = waist_circum*ALQ150) m(10)

mi estimate, post: reg sbp i.gender age i.ethnicity waist_circum weight /*
*/ total_chol hdl i.ALQ150 c.waist_circum#i.ALQ150
est store smcfcs
est table CCA MIimpTransform MIpassive smcfcs

use "nhanesMort.dta", clear
stset tMonths, failure(dead)
stcox i.gender age i.ethnicity sbp waist_circum weight total_chol hdl i.ALQ150
est store CCA

set seed 7233
smcfcs stcox gender age i.ethnicity sbp waist_circum weight total_chol hdl ALQ150, /*
*/ reg(sbp waist_circum weight total_chol hdl) logit(ALQ150)

mi estimate, post: stcox i.gender age i.ethnicity sbp waist_circum weight /*
*/ total_chol hdl i.ALQ150
est store smcfcs
est table CCA smcfcs, se