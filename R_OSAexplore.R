schmale <- read.csv("C:/Users/pallen/Desktop/Schmale.csv")
schmale$remgroup[schmale$o_ahi > 1 ] <- "OAHI"
schmale$remgroup[schmale$o_ahi > 1 & schmale$rem_nrem_ratio>=2 & schmale$nrem_oahi <= 1 ] <- "REM"
schmale$remgroup[schmale$o_ahi > 1 & schmale$rem_nrem_ratio<=0.5 & schmale$rem_oahi <= 1 ] <- "NREM"
schmale$remgroup[is.na(schmale$remgroup) ] <- "Normal AHI"

schmale$bmi[schmale$bmi == 0] <- NA

#figure 1

table(schmale$remgroup)
mean(schmale$age)
mean(schmale$age[schmale$remgroup == "Normal AHI"])
mean(schmale$age[schmale$remgroup == "REM"])
mean(schmale$age[schmale$remgroup == "NREM"])
mean(schmale$age[schmale$remgroup == "OAHI"])

sd(schmale$age)
sd(schmale$age[schmale$remgroup == "Normal AHI"])
sd(schmale$age[schmale$remgroup == "REM"])
sd(schmale$age[schmale$remgroup == "NREM"])
sd(schmale$age[schmale$remgroup == "OAHI"])


sum(schmale$gender == 2) #male
sum(schmale$gender[schmale$remgroup == "Normal AHI"] == 2) #male
sum(schmale$gender[schmale$remgroup == "REM"] == 2) #male
sum(schmale$gender[schmale$remgroup == "NREM"] == 2) #male
sum(schmale$gender[schmale$remgroup == "OAHI"] == 2) #male

sum(schmale$gender == 1) #female
sum(schmale$gender[schmale$remgroup == "Normal AHI"] == 1) #
sum(schmale$gender[schmale$remgroup == "REM"] == 1) #
sum(schmale$gender[schmale$remgroup == "NREM"] == 1) #
sum(schmale$gender[schmale$remgroup == "OAHI"] == 1) #

mean(schmale$bmi)
mean(schmale$bmi[schmale$remgroup == "Normal AHI"])
mean(schmale$bmi[schmale$remgroup == "REM"])
mean(schmale$bmi[schmale$remgroup == "NREM"])
mean(schmale$bmi[schmale$remgroup == "OAHI"])

sd(schmale$bmi)
sd(schmale$bmi[schmale$remgroup == "Normal AHI"])
sd(schmale$bmi[schmale$remgroup == "REM"])
sd(schmale$bmi[schmale$remgroup == "NREM"])
sd(schmale$bmi[schmale$remgroup == "OAHI"])

sum(schmale$Asthma == "yes")/976*100
sum(schmale$Asthma[schmale$remgroup == "Normal AHI"] == "yes")/49*100
sum(schmale$Asthma[schmale$remgroup == "REM"] == "yes")/35*100
sum(schmale$Asthma[schmale$remgroup == "NREM"] == "yes")/82*100
sum(schmale$Asthma[schmale$remgroup == "OAHI"] == "yes")/810*100

sum(schmale$Reflux == "yes")/976*100
sum(schmale$Reflux[schmale$remgroup == "Normal AHI"] == "yes")/49*100
sum(schmale$Reflux[schmale$remgroup == "REM"] == "yes")/35*100
sum(schmale$Reflux[schmale$remgroup == "NREM"] == "yes")/82*100
sum(schmale$Reflux[schmale$remgroup == "OAHI"] == "yes")/810*100

binom.test(60,82, p = 0.588)

# figure 2
mean(schmale$ahi, na.rm = TRUE)
mean(schmale$ahi[schmale$remgroup == "Normal AHI"], na.rm = TRUE)
mean(schmale$ahi[schmale$remgroup == "REM"], na.rm = TRUE)
mean(schmale$ahi[schmale$remgroup == "NREM"], na.rm = TRUE)
mean(schmale$ahi[schmale$remgroup == "OAHI"], na.rm = TRUE)

sd(schmale$ahi, na.rm = TRUE)
sd(schmale$ahi[schmale$remgroup == "Normal AHI"], na.rm = TRUE)
sd(schmale$ahi[schmale$remgroup == "REM"], na.rm = TRUE)
sd(schmale$ahi[schmale$remgroup == "NREM"], na.rm = TRUE)
sd(schmale$ahi[schmale$remgroup == "OAHI"], na.rm = TRUE)

mean(schmale$o_ahi, na.rm = TRUE)
mean(schmale$o_ahi[schmale$remgroup == "Normal AHI"], na.rm = TRUE)
mean(schmale$o_ahi[schmale$remgroup == "REM"], na.rm = TRUE)
mean(schmale$o_ahi[schmale$remgroup == "NREM"], na.rm = TRUE)
mean(schmale$o_ahi[schmale$remgroup == "OAHI"], na.rm = TRUE)

sd(schmale$o_ahi, na.rm = TRUE)
sd(schmale$o_ahi[schmale$remgroup == "Normal AHI"], na.rm = TRUE)
sd(schmale$o_ahi[schmale$remgroup == "REM"], na.rm = TRUE)
sd(schmale$o_ahi[schmale$remgroup == "NREM"], na.rm = TRUE)
sd(schmale$o_ahi[schmale$remgroup == "OAHI"], na.rm = TRUE)

mean(schmale$rem_oahi, na.rm = TRUE)
mean(schmale$rem_oahi[schmale$remgroup == "Normal AHI"], na.rm = TRUE)
mean(schmale$rem_oahi[schmale$remgroup == "REM"], na.rm = TRUE)
mean(schmale$rem_oahi[schmale$remgroup == "NREM"], na.rm = TRUE)
mean(schmale$rem_oahi[schmale$remgroup == "OAHI"], na.rm = TRUE)

sd(schmale$rem_oahi, na.rm = TRUE)
sd(schmale$rem_oahi[schmale$remgroup == "Normal AHI"], na.rm = TRUE)
sd(schmale$rem_oahi[schmale$remgroup == "REM"], na.rm = TRUE)
sd(schmale$rem_oahi[schmale$remgroup == "NREM"], na.rm = TRUE)
sd(schmale$rem_oahi[schmale$remgroup == "OAHI"], na.rm = TRUE)

mean(schmale$nrem_oahi, na.rm = TRUE)
mean(schmale$nrem_oahi[schmale$remgroup == "Normal AHI"], na.rm = TRUE)
mean(schmale$nrem_oahi[schmale$remgroup == "REM"], na.rm = TRUE)
mean(schmale$nrem_oahi[schmale$remgroup == "NREM"], na.rm = TRUE)
mean(schmale$nrem_oahi[schmale$remgroup == "OAHI"], na.rm = TRUE)

sd(schmale$nrem_oahi, na.rm = TRUE)
sd(schmale$nrem_oahi[schmale$remgroup == "Normal AHI"], na.rm = TRUE)
sd(schmale$nrem_oahi[schmale$remgroup == "REM"], na.rm = TRUE)
sd(schmale$nrem_oahi[schmale$remgroup == "NREM"], na.rm = TRUE)
sd(schmale$nrem_oahi[schmale$remgroup == "OAHI"], na.rm = TRUE)

mean(schmale$o2_nadir, na.rm = TRUE)
mean(schmale$o2_nadir[schmale$remgroup == "Normal AHI"], na.rm = TRUE)
mean(schmale$o2_nadir[schmale$remgroup == "REM"], na.rm = TRUE)
mean(schmale$o2_nadir[schmale$remgroup == "NREM"], na.rm = TRUE)
mean(schmale$o2_nadir[schmale$remgroup == "OAHI"], na.rm = TRUE)

sd(schmale$o2_nadir, na.rm = TRUE)
sd(schmale$o2_nadir[schmale$remgroup == "Normal AHI"], na.rm = TRUE)
sd(schmale$o2_nadir[schmale$remgroup == "REM"], na.rm = TRUE)
sd(schmale$o2_nadir[schmale$remgroup == "NREM"], na.rm = TRUE)
sd(schmale$o2_nadir[schmale$remgroup == "OAHI"], na.rm = TRUE)

mean(schmale$o2_desat_index, na.rm = TRUE)
mean(schmale$o2_desat_index[schmale$remgroup == "Normal AHI"], na.rm = TRUE)
mean(schmale$o2_desat_index[schmale$remgroup == "REM"], na.rm = TRUE)
mean(schmale$o2_desat_index[schmale$remgroup == "NREM"], na.rm = TRUE)
mean(schmale$o2_desat_index[schmale$remgroup == "OAHI"], na.rm = TRUE)

sd(schmale$o2_desat_index, na.rm = TRUE)
sd(schmale$o2_desat_index[schmale$remgroup == "Normal AHI"], na.rm = TRUE)
sd(schmale$o2_desat_index[schmale$remgroup == "REM"], na.rm = TRUE)
sd(schmale$o2_desat_index[schmale$remgroup == "NREM"], na.rm = TRUE)
sd(schmale$o2_desat_index[schmale$remgroup == "OAHI"], na.rm = TRUE)

##
table(schmale$osa_dx)
table(schmale$osa_dx, schmale$remgroup )

##
mild <- schmale[schmale$osa_dx == "Mild",]

table(mild$remgroup)

mean(mild$age)
mean(mild$age[mild$remgroup == "Normal AHI"])
mean(mild$age[mild$remgroup == "REM"])
mean(mild$age[mild$remgroup == "NREM"])
mean(mild$age[mild$remgroup == "OAHI"])

sd(mild$age)
sd(mild$age[mild$remgroup == "Normal AHI"])
sd(mild$age[mild$remgroup == "REM"])
sd(mild$age[mild$remgroup == "NREM"])
sd(mild$age[mild$remgroup == "OAHI"])


sum(mild$gender == 2) #male
sum(mild$gender[mild$remgroup == "Normal AHI"] == 2) #male
sum(mild$gender[mild$remgroup == "REM"] == 2) #male
sum(mild$gender[mild$remgroup == "NREM"] == 2) #male
sum(mild$gender[mild$remgroup == "OAHI"] == 2) #male

sum(mild$gender == 1) #female
sum(mild$gender[mild$remgroup == "Normal AHI"] == 1) #
sum(mild$gender[mild$remgroup == "REM"] == 1) #
sum(mild$gender[mild$remgroup == "NREM"] == 1) #
sum(mild$gender[mild$remgroup == "OAHI"] == 1) #

mean(mild$bmi, na.rm = TRUE)
mean(mild$bmi[mild$remgroup == "Normal AHI"])
mean(mild$bmi[mild$remgroup == "REM"])
mean(mild$bmi[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$bmi[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$bmi, na.rm = TRUE)
sd(mild$bmi[mild$remgroup == "Normal AHI"])
sd(mild$bmi[mild$remgroup == "REM"])
sd(mild$bmi[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$bmi[mild$remgroup == "OAHI"], na.rm = TRUE)

sum(mild$Asthma == "yes")/425*100
sum(mild$Asthma[mild$remgroup == "Normal AHI"] == "yes")/33*100
sum(mild$Asthma[mild$remgroup == "REM"] == "yes")/35*100
sum(mild$Asthma[mild$remgroup == "NREM"] == "yes")/55*100
sum(mild$Asthma[mild$remgroup == "OAHI"] == "yes")/302*100

sum(mild$Reflux == "yes")/425*100
sum(mild$Reflux[mild$remgroup == "Normal AHI"] == "yes")/33*100
sum(mild$Reflux[mild$remgroup == "REM"] == "yes")/35*100
sum(mild$Reflux[mild$remgroup == "NREM"] == "yes")/55*100
sum(mild$Reflux[mild$remgroup == "OAHI"] == "yes")/302*100

###

mean(mild$ahi, na.rm = TRUE)
mean(mild$ahi[mild$remgroup == "Normal AHI"], na.rm = TRUE)
mean(mild$ahi[mild$remgroup == "REM"], na.rm = TRUE)
mean(mild$ahi[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$ahi[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$ahi, na.rm = TRUE)
sd(mild$ahi[mild$remgroup == "Normal AHI"], na.rm = TRUE)
sd(mild$ahi[mild$remgroup == "REM"], na.rm = TRUE)
sd(mild$ahi[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$ahi[mild$remgroup == "OAHI"], na.rm = TRUE)

mean(mild$o_ahi, na.rm = TRUE)
mean(mild$o_ahi[mild$remgroup == "Normal AHI"], na.rm = TRUE)
mean(mild$o_ahi[mild$remgroup == "REM"], na.rm = TRUE)
mean(mild$o_ahi[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$o_ahi[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$o_ahi, na.rm = TRUE)
sd(mild$o_ahi[mild$remgroup == "Normal AHI"], na.rm = TRUE)
sd(mild$o_ahi[mild$remgroup == "REM"], na.rm = TRUE)
sd(mild$o_ahi[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$o_ahi[mild$remgroup == "OAHI"], na.rm = TRUE)

mean(mild$rem_oahi, na.rm = TRUE)
mean(mild$rem_oahi[mild$remgroup == "Normal AHI"], na.rm = TRUE)
mean(mild$rem_oahi[mild$remgroup == "REM"], na.rm = TRUE)
mean(mild$rem_oahi[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$rem_oahi[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$rem_oahi, na.rm = TRUE)
sd(mild$rem_oahi[mild$remgroup == "Normal AHI"], na.rm = TRUE)
sd(mild$rem_oahi[mild$remgroup == "REM"], na.rm = TRUE)
sd(mild$rem_oahi[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$rem_oahi[mild$remgroup == "OAHI"], na.rm = TRUE)

mean(mild$nrem_oahi, na.rm = TRUE)
mean(mild$nrem_oahi[mild$remgroup == "Normal AHI"], na.rm = TRUE)
mean(mild$nrem_oahi[mild$remgroup == "REM"], na.rm = TRUE)
mean(mild$nrem_oahi[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$nrem_oahi[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$nrem_oahi, na.rm = TRUE)
sd(mild$nrem_oahi[mild$remgroup == "Normal AHI"], na.rm = TRUE)
sd(mild$nrem_oahi[mild$remgroup == "REM"], na.rm = TRUE)
sd(mild$nrem_oahi[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$nrem_oahi[mild$remgroup == "OAHI"], na.rm = TRUE)

mean(mild$o2_nadir, na.rm = TRUE)
mean(mild$o2_nadir[mild$remgroup == "Normal AHI"], na.rm = TRUE)
mean(mild$o2_nadir[mild$remgroup == "REM"], na.rm = TRUE)
mean(mild$o2_nadir[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$o2_nadir[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$o2_nadir, na.rm = TRUE)
sd(mild$o2_nadir[mild$remgroup == "Normal AHI"], na.rm = TRUE)
sd(mild$o2_nadir[mild$remgroup == "REM"], na.rm = TRUE)
sd(mild$o2_nadir[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$o2_nadir[mild$remgroup == "OAHI"], na.rm = TRUE)

mean(mild$o2_desat_index, na.rm = TRUE)
mean(mild$o2_desat_index[mild$remgroup == "Normal AHI"], na.rm = TRUE)
mean(mild$o2_desat_index[mild$remgroup == "REM"], na.rm = TRUE)
mean(mild$o2_desat_index[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$o2_desat_index[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$o2_desat_index, na.rm = TRUE)
sd(mild$o2_desat_index[mild$remgroup == "Normal AHI"], na.rm = TRUE)
sd(mild$o2_desat_index[mild$remgroup == "REM"], na.rm = TRUE)
sd(mild$o2_desat_index[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$o2_desat_index[mild$remgroup == "OAHI"], na.rm = TRUE)

## Strict O AHI definition
table(schmale$osa_dx)
table(schmale$osa_dx, schmale$remgroup )

table(schmale$alt_osa_dx)
table(schmale$alt_osa_dx, schmale$remgroup )

##
mild <- schmale[schmale$alt_osa_dx == "Mild",]

table(mild$remgroup)

mean(mild$age)
mean(mild$age[mild$remgroup == "REM"])
mean(mild$age[mild$remgroup == "NREM"])
mean(mild$age[mild$remgroup == "OAHI"])

sd(mild$age)
sd(mild$age[mild$remgroup == "REM"])
sd(mild$age[mild$remgroup == "NREM"])
sd(mild$age[mild$remgroup == "OAHI"])


sum(mild$gender == 2) #male
sum(mild$gender[mild$remgroup == "REM"] == 2) #male
sum(mild$gender[mild$remgroup == "NREM"] == 2) #male
sum(mild$gender[mild$remgroup == "OAHI"] == 2) #male

sum(mild$gender == 1) #female
sum(mild$gender[mild$remgroup == "REM"] == 1) #
sum(mild$gender[mild$remgroup == "NREM"] == 1) #
sum(mild$gender[mild$remgroup == "OAHI"] == 1) #

mean(mild$bmi, na.rm = TRUE)
mean(mild$bmi[mild$remgroup == "REM"])
mean(mild$bmi[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$bmi[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$bmi, na.rm = TRUE)
sd(mild$bmi[mild$remgroup == "REM"])
sd(mild$bmi[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$bmi[mild$remgroup == "OAHI"], na.rm = TRUE)

sum(mild$Asthma == "yes")/488*100
sum(mild$Asthma[mild$remgroup == "REM"] == "yes")/35*100
sum(mild$Asthma[mild$remgroup == "NREM"] == "yes")/64*100
sum(mild$Asthma[mild$remgroup == "OAHI"] == "yes")/389*100

sum(mild$Reflux == "yes")/488*100
sum(mild$Reflux[mild$remgroup == "REM"] == "yes")/35*100
sum(mild$Reflux[mild$remgroup == "NREM"] == "yes")/64*100
sum(mild$Reflux[mild$remgroup == "OAHI"] == "yes")/389*100

###

mean(mild$ahi, na.rm = TRUE)

mean(mild$ahi[mild$remgroup == "REM"], na.rm = TRUE)
mean(mild$ahi[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$ahi[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$ahi, na.rm = TRUE)

sd(mild$ahi[mild$remgroup == "REM"], na.rm = TRUE)
sd(mild$ahi[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$ahi[mild$remgroup == "OAHI"], na.rm = TRUE)

mean(mild$o_ahi, na.rm = TRUE)

mean(mild$o_ahi[mild$remgroup == "REM"], na.rm = TRUE)
mean(mild$o_ahi[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$o_ahi[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$o_ahi, na.rm = TRUE)

sd(mild$o_ahi[mild$remgroup == "REM"], na.rm = TRUE)
sd(mild$o_ahi[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$o_ahi[mild$remgroup == "OAHI"], na.rm = TRUE)

mean(mild$rem_oahi, na.rm = TRUE)

mean(mild$rem_oahi[mild$remgroup == "REM"], na.rm = TRUE)
mean(mild$rem_oahi[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$rem_oahi[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$rem_oahi, na.rm = TRUE)

sd(mild$rem_oahi[mild$remgroup == "REM"], na.rm = TRUE)
sd(mild$rem_oahi[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$rem_oahi[mild$remgroup == "OAHI"], na.rm = TRUE)

mean(mild$nrem_oahi, na.rm = TRUE)

mean(mild$nrem_oahi[mild$remgroup == "REM"], na.rm = TRUE)
mean(mild$nrem_oahi[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$nrem_oahi[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$nrem_oahi, na.rm = TRUE)

sd(mild$nrem_oahi[mild$remgroup == "REM"], na.rm = TRUE)
sd(mild$nrem_oahi[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$nrem_oahi[mild$remgroup == "OAHI"], na.rm = TRUE)

mean(mild$o2_nadir, na.rm = TRUE)

mean(mild$o2_nadir[mild$remgroup == "REM"], na.rm = TRUE)
mean(mild$o2_nadir[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$o2_nadir[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$o2_nadir, na.rm = TRUE)

sd(mild$o2_nadir[mild$remgroup == "REM"], na.rm = TRUE)
sd(mild$o2_nadir[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$o2_nadir[mild$remgroup == "OAHI"], na.rm = TRUE)

mean(mild$o2_desat_index, na.rm = TRUE)

mean(mild$o2_desat_index[mild$remgroup == "REM"], na.rm = TRUE)
mean(mild$o2_desat_index[mild$remgroup == "NREM"], na.rm = TRUE)
mean(mild$o2_desat_index[mild$remgroup == "OAHI"], na.rm = TRUE)

sd(mild$o2_desat_index, na.rm = TRUE)

sd(mild$o2_desat_index[mild$remgroup == "REM"], na.rm = TRUE)
sd(mild$o2_desat_index[mild$remgroup == "NREM"], na.rm = TRUE)
sd(mild$o2_desat_index[mild$remgroup == "OAHI"], na.rm = TRUE)
