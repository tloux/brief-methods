
# read SAS xport file =====================================

brfss2019 = haven::read_xpt('LLCP2019.XPT')




# subset missouri respondents specified variables =========

myvars = c('_SEX', '_RACEGR3', '_AGEG5YR', '_AGE_G', 'MSCODE', '_BMI5', 
           '_BMI5CAT', 'SMOKE100', 'DIABETE4', 'ADDEPEV3', 'GENHLTH', 
           'PHYSHLTH', 'MENTHLTH', 'HLTHPLN1', 'MEDCOST', 'EXERANY2', 
           '_RFDRHV7', 'FLUSHOT7', 'TRNSGNDR')

col_ind = which(names(brfss2019) %in% myvars)

brfss2019mo = subset(brfss2019, subset=(`_STATE`==29), select=col_ind)

set.seed(2019)
sample_ind = sample(1:nrow(brfss2019mo), size=2000, replace=FALSE)
brfss2019mo = brfss2019mo[sample_ind, ]




# clean variables =========================================

# respondent sex (at birth)

tmp = factor(brfss2019mo$`_SEX`, levels=1:2, 
             labels=c('Male', 'Female'))
brfss2019mo$sex = tmp


# race

tmp = factor(brfss2019mo$`_RACEGR3`, levels=c(1:5,9), 
             labels=c('White', 'Black','Other','MultiRacial',
                      'Hispanic',NA))
brfss2019mo$race = tmp


# age (14-level category)

tmp = factor(brfss2019mo$`_AGEG5YR`, levels=1:14,
            labels=c('18-24','25-29','30-34','35-39','40-44','45-49',
                     '50-54','55-59','60-64','65-69','70-74','75-79',
                     '80+',NA))
brfss2019mo$age14 = tmp


# age (6-level category)

tmp = factor(brfss2019mo$`_AGE_G`, levels=1:6, 
             labels=c('18-24','25-34','35-44','45-54','55-64','65+'))
brfss2019mo$age6 = tmp
             

# metro status

tmp = factor(brfss2019mo$MSCODE, levels=c(1:3,5), 
             labels=c('Urban','Urban','Suburban','Rural'))
brfss2019mo$metro = tmp


# BMI numeric

tmp = ifelse(brfss2019mo$`_BMI5` %in% c(1,9999), NA, brfss2019mo$`_BMI5`)
brfss2019mo$bmi = tmp / 100


# BMI categorical

tmp = factor(brfss2019mo$`_BMI5CAT`, levels=c(1:5), 
             labels=c('Underweight','Normal','Overweight',
                      'Obese',NA),
             ordered=TRUE)
brfss2019mo$bmicat = tmp


# smoked at least 100 cigarettes in life

tmp = factor(brfss2019mo$SMOKE100, levels=1:2, 
             labels=c('Yes', 'No'))
brfss2019mo$smoke100 = tmp


# ever told have diabetes

tmp = factor(brfss2019mo$DIABETE4, levels=1:4, 
             labels=c('Yes', 'No', 'No', 'No'))
brfss2019mo$diabetes = tmp


# every told have depressive disorder

tmp = factor(brfss2019mo$ADDEPEV3, levels=1:2, 
             labels=c('Yes', 'No'))
brfss2019mo$depression = tmp


# general health

tmp = factor(brfss2019mo$GENHLTH, levels=1:5, 
             labels=c('Excellent', 'Very good', 'Good', 'Fair', 'Poor'), 
             ordered=TRUE)
brfss2019mo$genhealth = tmp


# physical health not good

hlth_lookup = c(1:30, 0, NA, NA)
names(hlth_lookup) = c(1:30, 88, 77, 99)
tmp = hlth_lookup[as.character(brfss2019mo$PHYSHLTH)]
brfss2019mo$physhealth = tmp


# mental health not good

tmp = hlth_lookup[as.character(brfss2019mo$MENTHLTH)]
brfss2019mo$menthealth = tmp


# healthcare coverage

tmp = factor(brfss2019mo$HLTHPLN1, levels=1:2, 
             labels=c('Yes', 'No'))
brfss2019mo$coverage = tmp


# could not see a doctor because of cost (last 12 months)

tmp = factor(brfss2019mo$MEDCOST, levels=1:2, 
             labels=c('Yes', 'No'))
brfss2019mo$medcost = tmp


# any physical activities or exercises

tmp = factor(brfss2019mo$EXERANY2, levels=1:2, 
             labels=c('Yes', 'No'))
brfss2019mo$anyexercise = tmp


# heavy drinker

tmp = factor(brfss2019mo$`_RFDRHV7`, levels=c(1:2,9), 
             labels=c('No', 'Yes', NA))
brfss2019mo$heavydrink = tmp


# flu shot or vaccine (last 12 months)

tmp = factor(brfss2019mo$FLUSHOT7, levels=c(1:2,9),
             labels=c('Yes','No',NA))
brfss2019mo$flushot = tmp


# consider self transgender

tmp = factor(brfss2019mo$TRNSGNDR, levels=c(1:4,7,9, 10),
             labels=c('Yes','Yes','Yes','No',
                      NA,NA,NA))
brfss2019mo$transgender = tmp


# select cleaned variables ================================

mydat = as.data.frame(brfss2019mo[, 20:38])



# save ====================================================

save(file='brfss2019mo.RData', mydat)
