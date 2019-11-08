#this program creates an NHANES dataset that is used in course practicals

install.packages("nhanesA")

library(nhanesA)
#we will use data from the 1999 survey

#DEMOGRAPHICS
nhanesTables('DEMO', 1999)
demo <- nhanesTranslate('DEMO', c("SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1"), data=nhanes('DEMO'))
demoSub <- subset(demo, select=c("SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1"))
colnames(demoSub) <- c("SEQN", "gender", "age", "ethnicity")
levels(demoSub$ethnicity)[levels(demoSub$ethnicity)=="Other Race - Including Multi-Rac"] <- "Other Race"

#EXAMINATION
nhanesTables('EXAM', 1999)
nhanesTableVars('EXAM', 'BPX')
bloodPressure <- nhanes('BPX')
bloodPressureSub <- subset(bloodPressure, select=c("SEQN", "BPXSAR"))
colnames(bloodPressureSub) <- c("SEQN", "sbp")

nhanesTableVars('EXAM', 'BMX')
bodyMeasures <- nhanes('BMX')
bodyMeasuresSub <- subset(bodyMeasures, select=c("SEQN", "BMXWAIST", "BMXWT"))
colnames(bodyMeasuresSub) <- c("SEQN", "waist_circum", "weight")

#LAB
nhanesTables('LAB', 1999)
chol <- nhanes('Lab13')
cholSub <- subset(chol, select=c("SEQN", "LBXTC", "LBDHDL"))
colnames(cholSub) <- c("SEQN", "total_chol", "hdl")

#ALCOHOL
alc <- nhanesTranslate('ALQ', c("ALQ150"), data=nhanes('ALQ'))
alcSub <- subset(alc, select=c("SEQN", "ALQ150"))
alcSub$ALQ150[alcSub$ALQ150=="Refused"] <- NA
alcSub$ALQ150[alcSub$ALQ150=="Don't know"] <- NA
alcSub$ALQ150 <- droplevels(alcSub$ALQ150)
alcSub$ALQ150 <- factor(alcSub$ALQ150,levels(alcSub$ALQ150)[c(2,1)])
#alcSub$ALQ150 <- as.numeric(alcSub$ALQ150)-1

#merge together
baseline <- demoSub
baseline <- merge(baseline, bloodPressureSub, by="SEQN")
baseline <- merge(baseline, bodyMeasuresSub, by="SEQN")
baseline <- merge(baseline, cholSub, by="SEQN")
baseline <- merge(baseline, alcSub, by="SEQN")

summary(baseline)

#now we load in linked mortality data
#the following code is extracted from: ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/R_ReadInProgramAllSurveys.R

library(readr)
library(dplyr)
srvyin <- paste("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_1999_2000_MORT_2015_PUBLIC.dat")   # full .DAT name here
srvyout <- "mortality" # shorthand dataset name here

# read in the fixed-width format ASCII file
dsn <- read_fwf(file=srvyin,
                col_types = "ciiiiiiiddii",
                fwf_cols(publicid = c(1,14),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         dodqtr = c(22,22),
                         dodyear = c(23,26),
                         wgt_new = c(27,34),
                         sa_wgt_new = c(35,42),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = "."
)

# create the ID (SEQN) for the NHANES surveys
dsn$seqn <- substr(dsn$publicid,1,5)
# NOTE:   SEQN is the unique ID for NHANES.

#Drop NHIS variables
dsn <- select(dsn, -publicid)
dsn <- select(dsn, -dodqtr)
dsn <- select(dsn, -dodyear)
dsn <- select(dsn, -wgt_new)
dsn <- select(dsn, -sa_wgt_new)

# Variable frequencies

#ELIGSTAT: Eligibility Status for Mortality Follow-up
table(dsn$eligstat)
#1 = "Eligible"
#2 = "Under age 18, not available for public release"
#3 = "Ineligible"

#MORTSTAT: Final Mortality Status
table(dsn$mortstat, useNA="ifany")
# 0 = Assumed alive
# 1 = Assumed deceased
# <NA> = Ineligible or under age 18

#UCOD_LEADING: Underlying Cause of Death: Recode
table(dsn$ucod_leading, useNA="ifany")
# 1 = Diseases of heart (I00-I09, I11, I13, I20-I51)
# 2 = Malignant neoplasms (C00-C97)
# 3 = Chronic lower respiratory diseases (J40-J47)
# 4 = Accidents (unintentional injuries) (V01-X59, Y85-Y86)
# 5 = Cerebrovascular diseases (I60-I69)
# 6 = Alzheimer's disease (G30)
# 7 = Diabetes mellitus (E10-E14)
# 8 = Influenza and pneumonia (J09-J18)
# 9 = Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)
# 10 = All other causes (residual)
# <NA> = Ineligible, under age 18, assumed alive, or no cause of death data

#DIABETES: Diabetes Flag from Multiple Cause of Death (MCOD)
table(dsn$diabetes, useNA="ifany")
# 0 = No - Condition not listed as a multiple cause of death
# 1 = Yes - Condition listed as a multiple cause of death
# <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available

#HYPERTEN: Hypertension Flag from Multiple Cause of Death (MCOD)
table(dsn$hyperten, useNA="ifany")
# 0 = No - Condition not listed as a multiple cause of death
# 1 = Yes - Condition listed as a multiple cause of death
# <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available

# Structure and contents of data
str(dsn)
# Re-name the dataset, DSN, to the short survey name then remove other R objects
assign(paste0(srvyout), dsn)
rm(dsn, srvyin, srvyout)

#subset the mortality dataset created
#information on these variables is at: https://www.cdc.gov/nchs/data/datalinkage/public-use-2015-linked-mortality-file-description.pdf
mortalitySub <- subset(mortality, select=c("seqn", "mortstat", "permth_int"))
colnames(mortalitySub) <- c("SEQN", "dead", "tMonths")
mortalitySub <- data.frame(mortalitySub)
mortalitySub$SEQN <- as.integer(mortalitySub$SEQN)

#merge with baseline information
nhanesMort <- merge(baseline, mortalitySub, by="SEQN")
#drop seqn
nhanesMort <- select(nhanesMort, -SEQN)
#drop those missing dead
nhanesMort <- nhanesMort[is.na(nhanesMort$dead)==FALSE,]
#save dataset
save(nhanesMort, file="nhanesMort.RData")

library(foreign)
#subtract one off ALQ150
nhanesMort$ALQ150 <- as.numeric(nhanesMort$ALQ150)-1
write.dta(nhanesMort, file="nhanesMort.dta")