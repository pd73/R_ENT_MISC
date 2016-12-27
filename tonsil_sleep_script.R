TonsilData <- read.csv("Z:/Research Coordinator/_Electronic Regulatory Binders/63255 Pediatric Tonsillectomy OSA outcomes - Karelsky/SuccessOfAdenotonsil_DATA_2016-11-14_1630.csv")
TonsilData <- TonsilData[TonsilData$date_of_ta_surgery_complete == 2,]

TonsilData$sleep_1 <- NA
TonsilData$sleep_2 <- NA
TonsilData$sleep_3 <- NA
TonsilData$sleep_4 <- NA

TonsilData$AHI_1 <- NA
TonsilData$AHI_2 <- NA
TonsilData$AHI_3 <- NA
TonsilData$AHI_4 <- NA

TonsilData$presurgdate <- NA
TonsilData$postsurgdate <- NA
TonsilData$presurgAHI <- NA
TonsilData$postsurgAHI <- NA

# TonsilData$presleep_days <- NA
# TonsilData$postsleep_date <- NA
# TonsilData$postsleep_days <- NA
# TonsilData$presleep_AHI <- NA
# TonsilData$postsleep_AHI <- NA

visitdays <- read.csv("Z:/Research Coordinator/_Electronic Regulatory Binders/63255 Pediatric Tonsillectomy OSA outcomes - Karelsky/PedSleepClinicPatients_DOS_identifiers_SEP2016.csv")
visitdays <- visitdays[!is.na(visitdays$MRN),]

for (i in 1:length(TonsilData$mrn)) {
  thisMRN <- TonsilData$mrn[i]
  if (length(visitdays$firstService[visitdays$MRN == thisMRN]) == 0) {next}
  # dates
  TonsilData$sleep_1[i] <- as.character(visitdays$firstService[visitdays$MRN == thisMRN])
  TonsilData$sleep_2[i] <- as.character(visitdays$secondService[visitdays$MRN == thisMRN])
  TonsilData$sleep_3[i] <- as.character(visitdays$thirdService[visitdays$MRN == thisMRN])
  TonsilData$sleep_4[i] <- as.character(visitdays$fourthService[visitdays$MRN == thisMRN])
  #AHI
  TonsilData$AHI_1[i] <- visitdays$AHIfirst[visitdays$MRN == thisMRN]
  TonsilData$AHI_2[i] <- visitdays$AHIsecond[visitdays$MRN == thisMRN]
  TonsilData$AHI_3[i] <- visitdays$AHIthird[visitdays$MRN == thisMRN]
  TonsilData$AHI_4[i] <- visitdays$AHIfourth[visitdays$MRN == thisMRN]
  
  thisSurgDate <- as.Date(TonsilData$date_of_t_a_surgery[i], "%m/%d/%Y")
  SleepDate <- as.Date(TonsilData$sleep_1[i], "%m/%d/%Y")
  
  #if surg date is before first sleep study then next subject
  if ((thisSurgDate-SleepDate)< 0 )  {next()}
  # otherwise let try the first study is the presurg study
  TonsilData$presurgdate[i] <- TonsilData$sleep_1[i]
  TonsilData$presurgAHI[i] <- TonsilData$AHI_1[i]
  
  #lets check the second date
  SleepDate <- as.Date(TonsilData$sleep_2[i], "%m/%d/%Y")
  
  
  if ((thisSurgDate-SleepDate)< 0 )  {
    # if the second date is negative then great, it's the post surg date and we are done
    TonsilData$postsurgdate[i] <- TonsilData$sleep_2[i]
    TonsilData$postsurgAHI[i] <- TonsilData$AHI_2[i]
    next()
  }
  # if we got to here then second date could be presurg
  TonsilData$presurgdate[i] <- TonsilData$sleep_2[i]
  TonsilData$presurgAHI[i] <- TonsilData$AHI_2[i]
  #lets check the third date
  SleepDate <- as.Date(TonsilData$sleep_3[i], "%m/%d/%Y")
  # check that there actually is a 3rd date
  if (is.na(SleepDate)) {
    TonsilData$presurgdate[i] <- NA
    TonsilData$presurgAHI[i] <- NA
    TonsilData$postsurgdate[i] <- NA
    TonsilData$postsurgAHI[i] <- NA
    next
  } 
  
  if ((thisSurgDate-SleepDate)< 0 )  {
    # if the third date is negative then great, it's the post surg date and we are done
    TonsilData$postsurgdate[i] <- TonsilData$sleep_3[i]
    TonsilData$postsurgAHI[i] <- TonsilData$AHI_3[i]
    next()
  }
  # if we got to here then third date could be presurg
  TonsilData$presurgdate[i] <- TonsilData$sleep_3[i]
  TonsilData$presurgAHI[i] <- TonsilData$AHI_3[i]
  #lets check the fourth date
  SleepDate <- as.Date(TonsilData$sleep_4[i], "%m/%d/%Y")
  # check that there actually is a 4th date
  if (is.na(SleepDate)) { 
    TonsilData$presurgdate[i] <- NA
    TonsilData$presurgAHI[i] <- NA
    TonsilData$postsurgdate[i] <- NA
    TonsilData$postsurgAHI[i] <- NA
    next
  } 
  
  if ((thisSurgDate-SleepDate)< 0 )  {
    # if the forth date is negative then great, it's the post surg date and we are done
    TonsilData$postsurgdate[i] <- TonsilData$sleep_4[i]
    TonsilData$postsurgAHI[i] <- TonsilData$AHI_4[i]
    next
  }
  #if we got here then problem!
  TonsilData$presurgdate[i] <- NA
  TonsilData$presurgAHI[i] <- NA
  TonsilData$postsurgdate[i] <- NA
  TonsilData$postsurgAHI[i] <- NA
  
}
  
write.csv(TonsilData, "Z:/Research Coordinator/_Electronic Regulatory Binders/63255 Pediatric Tonsillectomy OSA outcomes - Karelsky/TAsurg_w_sleepdates_AHI.csv")
  
#   
#   thisSub <- visitdays[visitdays$MRN == thisMRN,]
#   
#   if (length(thisSub$numServiceDays) == 0) {
#     TonsilData$status[i] <- -88
#     next()
#   }
#   
#   if(thisSub$numServiceDays == 1) {
#     TonsilData$status[i] <- -1
#     next()
#   }
#   firstsleep <- as.Date(thisSub$firstService, "%m/%d/%Y")
#   if (firstsleep > thisSurgDate) {
#     TonsilData$status[i] <- -2
#     next()
#   }
#   
#   TonsilData$status[i] <-  thisSurgDate - firstsleep
#   secsleep <- as.Date(thisSub$secondService, "%m/%d/%Y")
#   
#   if (secsleep > thisSurgDate) {
#     TonsilData$status[i] <- 1
#     TonsilData$presleep_date[i] <- as.character(firstsleep)
#     TonsilData$presleep_days[i] <- thisSurgDate - firstsleep
#     TonsilData$postsleep_date[i] <- as.character(secsleep)
#     TonsilData$postsleep_days[i] <- secsleep - thisSurgDate
#     TonsilData$presleep_AHI[i] <- thisSub$AHIfirst
#     TonsilData$postsleep_AHI[i] <- thisSub$AHIsecond
#     next()
#   }
#   
# }
  
# i = 1
# visitdays$MRN[i]
# thisSub <- TonsilData[TonsilData$mrn == thisMRN,]
# thisSurgDate <- as.Date(thisSub$date_of_t_a_surgery, "%m/%d/%Y")
# firstsleep <- as.Date(visitdays$firstService[i], "%m/%d/%Y")
# secondsleep <- as.Date(visitdays$secondService[i], "%m/%d/%Y")
# if (firstsleep - thisSurgDate < 0)