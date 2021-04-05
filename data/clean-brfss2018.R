
# read SAS xport file =====================================

brfss2018 = haven::read_xpt('LLCP2018.XPT')




# subset missouri respondents specified variables =========

myvars = c('SEX1', '_RACEGR3', '_AGEG5YR', '_AGE_G', 'MSCODE', '_BMI5', 
           '_BMI5CAT', 'SMOKE100', 'DIABETE3', 'ADDEPEV2', 'GENHLTH', 
           'PHYSHLTH', 'MENTHLTH', 'HLTHPLN1', 'MEDCOST', 'EXERANY2', 
           'SLEPTIM1', '_RFDRHV6', 'FLUSHOT6', 'TRNSGNDR')

col_ind = which(names(brfss2018) %in% myvars)

brfss2018mo = subset(brfss2018, subset=(`_STATE`==29), select=col_ind)




# clean variables =========================================

# respondent sex (at birth)

tmp = factor(brfss2018mo$SEX1, levels=1:2, 
             labels=c('Male', 'Female'))
brfss2018mo$sex = tmp


# race

tmp = factor(brfss2018mo$`_RACEGR3`, levels=c(1:5,9), 
             labels=c('White', 'Black','Other','MultiRacial',
                      'Hispanic',NA))
brfss2018mo$race = tmp


# age (14-level category)

tmp = factor(brfss2018mo$`_AGEG5YR`, levels=1:14,
            labels=c('18-24','25-29','30-34','35-39','40-44','45-49',
                     '50-54','55-59','60-64','65-69','70-74','75-79',
                     '80+',NA))
brfss2018mo$age14 = tmp


# age (6-level category)

tmp = factor(brfss2018mo$`_AGE_G`, levels=1:6, 
             labels=c('18-24','25-34','35-44','45-54','55-64','65+'))
brfss2018mo$age6 = tmp
             

# metro status

tmp = factor(brfss2018mo$MSCODE, levels=c(1:3,5), 
             labels=c('Urban','Urban','Suburban','Rural'))
brfss2018mo$metro = tmp


# BMI numeric

tmp = ifelse(brfss2018mo$`_BMI5` %in% c(1,9999), NA, brfss2018mo$`_BMI5`)
brfss2018mo$bmi = tmp


# BMI categorical

tmp = factor(brfss2018mo$`_BMI5CAT`, levels=c(1:5), 
             labels=c('Underweight','Normal','Overweight',
                      'Obese',NA),
             ordered=TRUE)
brfss2018mo$bmicat = tmp


# smoked at least 100 cigarettes in life

tmp = factor(brfss2018mo$SMOKE100, levels=1:2, 
             labels=c('Yes', 'No'))
brfss2018mo$smoke100 = tmp


# ever told have diabetes

tmp = factor(brfss2018mo$DIABETE3, levels=1:4, 
             labels=c('Yes', 'No', 'No', 'No'))
brfss2018mo$diabetes = tmp


# every told have depressive disorder

tmp = factor(brfss2018mo$ADDEPEV2, levels=1:2, 
             labels=c('Yes', 'No'))
brfss2018mo$depression = tmp


# general health

tmp = factor(brfss2018mo$GENHLTH, levels=1:5, 
             labels=c('Excellent', 'Very good', 'Good', 'Fair', 'Poor'), 
             ordered=TRUE)
brfss2018mo$genhealth = tmp


# physical health not good

hlth_lookup = c(1:30, 0, NA, NA)
names(hlth_lookup) = c(1:30, 88, 77, 99)
tmp = hlth_lookup[as.character(brfss2018mo$PHYSHLTH)]
brfss2018mo$physhealth = tmp


# mental health not good

tmp = hlth_lookup[as.character(brfss2018mo$MENTHLTH)]
brfss2018mo$menthealth = tmp


# healthcare coverage

tmp = factor(brfss2018mo$HLTHPLN1, levels=1:2, 
             labels=c('Yes', 'No'))
brfss2018mo$coverage = tmp


# could not see a doctor because of cost (last 12 months)

tmp = factor(brfss2018mo$MEDCOST, levels=1:2, 
             labels=c('Yes', 'No'))
brfss2018mo$medcost = tmp


# any physical activities or exercises

tmp = factor(brfss2018mo$EXERANY2, levels=1:2, 
             labels=c('Yes', 'No'))
brfss2018mo$anyexercise = tmp


# average sleep in 24-hour period

tmp = ifelse(brfss2018mo$SLEPTIM1 %in% c(77,99), NA, brfss2018mo$SLEPTIM1)
brfss2018mo$sleeptime = tmp


# heavy drinker

tmp = factor(brfss2018mo$`_RFDRHV6`, levels=c(1:2,9), 
             labels=c('Yes', 'No',NA))
brfss2018mo$heavydrink = tmp


# flu shot or vaccine (last 12 months)

tmp = factor(brfss2018mo$FLUSHOT6, levels=c(1:2,9),
             labels=c('Yes','No',NA))
brfss2018mo$flushot = tmp


# consider self transgender

tmp = factor(brfss2018mo$TRNSGNDR, levels=c(1:4,7,9, 10),
             labels=c('Yes','Yes','Yes','No',
                      NA,NA,NA))
brfss2018mo$transgender = tmp


# select cleaned variables ================================

mydat = as.data.frame(brfss2018mo[, 21:40])



# save ====================================================

save(file='brfss2018mo.RData', mydat)
