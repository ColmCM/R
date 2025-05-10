hospital_dat <- readRDS("~/SharedFiles/ST201/hospital_dat.rds")

#H1 <- hospital_dat[1:100,] #H1 data
#H2 <- hospital_dat[101:200,] #H2 data
##These are broad tables that tell us the amount of success,failures and Sum.
#hospital_tab <- table(hospital_dat) #hospital_tab #addmargins(hospital_tab)
#hospital_prob_tab <- prop.table(hospital_tab) #hospital_prob_tab #addmargins(hospital_prob_tab)

#2-way table (tab_overall)
tab_overall <- xtabs(~ hospital+outcome, data = hospital_dat)
tab_overall

# 3-way table (tab_dept)
tab_dept <- xtabs(~ hospital + outcome + severity, data = hospital_dat)
tab_dept
addmargins(tab_dept) #This is the same as Lines 12-14

#This is a Chi-squared test for the 2-way table
my_chisq_test <- chisq.test(tab_overall)
my_chisq_test
#Note that the results given in the Chi-squared test is the same if it was done in a 3-way table.


##Odds ratio
library(vcd)

#loddsratio() (overall)  #Necessary to get odds ratio
lodds_overall <- loddsratio(~hospital+outcome,data = hospital_dat)
lodds_overall # [1] 0.7537718

#Odds ratio #Odds ratio for 2-wau relationship.
exp(lodds_overall$coefficients)# [1]2.125

## log odds (dept) #This is the odds ratio for the 3-way relationship
lodds_dept <- loddsratio(~hospital + outcome + severity, data = hospital_dat)
lodds_dept

## odds ratio for H1
exp(lodds_dept$coefficients[1])
## odds ratio for H2
exp(lodds_dept$coefficients[2])

exp(lodds_dept$coefficients[1]) - exp(lodds_dept$coefficients[2])

