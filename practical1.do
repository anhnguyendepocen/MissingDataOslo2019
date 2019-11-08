
use practical1_1.dta, clear

summ

scatter sbpmiss age

gen r = 1*(sbpmiss!=.)

logistic r age

logistic r age sbpmiss

logistic r age sbpcomplete
