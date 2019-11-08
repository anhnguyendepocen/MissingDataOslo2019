#setwd("C:/Users/jwb67/OneDrive - University of Bath/Courses/Copenhagen2019")
setwd("/Users/Jonathan/OneDrive - University of Bath/Courses/MissingDataOslo2019")

#scenario 1
set.seed(1234)
n <- 1000
age <- 20+60*runif(n)
sbpcomplete <- 130+(age-40)+rnorm(n,sd=20)
sbpmiss <- sbpcomplete
sbpmiss[runif(n)<0.5] <- NA

data1 <- data.frame(age, sbpcomplete, sbpmiss)

dim(data1)
summary(data1$sbpmiss)

r <- 1-is.na(data1$sbpmiss)

#investigate missingness using observed data
summary(glm(r ~ age, family=binomial, data1))

#consistent with MCAR, but doesn't prove it

summary(glm(r ~ age+sbpcomplete, family=binomial, data1))

summary(lm(sbpcomplete~age, data1))
summary(lm(sbpmiss~age, data1))

#scenario 2 - MAR
expit <- function(x) {
  exp(x)/(1+exp(x))
}

n <- 1000
age <- 20+60*runif(n)
sbpcomplete <- 130+(age-40)+rnorm(n,sd=20)
sbpmiss <- sbpcomplete
sbpmiss[runif(n)<expit((age-mean(age))/sd(age)-(sbpcomplete-130)/120)] <- NA

data2 <- data.frame(age, sbpcomplete, sbpmiss)

dim(data2)
summary(data2$sbpmiss)

r <- 1-is.na(data2$sbpmiss)

#investigate missingness using observed data
summary(glm(r ~ age, family=binomial, data2))

#reject MCAR, but could be MAR or MNAR
#now cheat

summary(glm(r ~ age+sbpcomplete, family=binomial, data2))

summary(glm(r ~ sbpcomplete, family=binomial, data2))
summary(lm(sbpcomplete~age, data=data2))

#scenario 3 - MNAR

n <- 1000
age <- 20+60*runif(n)
sbpcomplete <- 130+(age-40)+rnorm(n,sd=20)
sbpmiss <- sbpcomplete
sbpmiss[runif(n)<expit((sbpcomplete-130)/120)] <- NA

data3 <- data.frame(age, sbpcomplete, sbpmiss)

dim(data3)
summary(data3$sbpmiss)

r <- 1-is.na(data$sbpmiss3)

#investigate missingness using observed data
summary(glm(r ~ age, family=binomial, data3))

#reject MCAR, but could be MAR or MNAR
#now cheat

summary(glm(r ~ age+sbpcomplete, family=binomial, data3))


#scenario 4 - MNAR which appears MCAR

n <- 1000
age <- 20+60*runif(n)
sbpcomplete <- 130+rnorm(n,sd=40)
sbpmiss <- sbpcomplete
sbpmiss[runif(n)<expit((sbpcomplete-130)/120)] <- NA

data4 <- data.frame(age, sbpcomplete, sbpmiss)

dim(data4)
summary(data4$sbpmiss)

r <- 1-is.na(data4$sbpmiss)

#investigate missingness using observed data
summary(glm(r ~ age, family=binomial, data4))

#appears consistent with MCAR, but if we cheat
summary(glm(r ~ age+sbpcomplete, family=binomial, data4))
#we see it is actually MNAR

save(data1,data2,data3,data4,file="practical1.Rdata")

#save Stata version
library(foreign)
write.dta(data1, file="practical1_1.dta")
write.dta(data2, file="practical1_2.dta")
write.dta(data3, file="practical1_3.dta")
write.dta(data4, file="practical1_4.dta")