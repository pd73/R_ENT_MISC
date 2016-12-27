TonsilData <- read.csv("Z:/Research Coordinator/_Electronic Regulatory Binders/63255 Pediatric Tonsillectomy OSA outcomes - Karelsky/TAsurg_w_sleepdates_AHI.csv")

allstudies <- OutputData[OutputData$draft == -1,]
allstudies <- allstudies[!is.na(allstudies$DOBlist),]

TonsilSleepData <- TonsilData[!is.na(TonsilData$presurgdate), c(2,3,4,5,6,7,19,20)]

TonsilSleepData$age <- (as.Date(TonsilSleepData$date_of_t_a_surgery,"%m/%d/%Y") - as.Date(TonsilSleepData$date_of_birth,"%m/%d/%Y"))/365
TonsilSleepData$gender <- NA
TonsilSleepData$days_b4_surg <-as.Date(TonsilSleepData$date_of_t_a_surgery,"%m/%d/%Y") - as.Date(TonsilSleepData$presurgdate,"%m/%d/%Y")
TonsilSleepData$days_aft_surg <-as.Date(TonsilSleepData$postsurgdate,"%m/%d/%Y") - as.Date(TonsilSleepData$date_of_t_a_surgery,"%m/%d/%Y")

for (i in 1:length(TonsilSleepData$mrn)) {
  thisDOB <- as.Date(TonsilSleepData$date_of_birth[i],"%m/%d/%Y")
  thisdate <- as.Date(TonsilSleepData$presurgdate[i], "%m/%d/%Y")
  TonsilSleepData$gender[i] <- as.character(allstudies$GENDERlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1])
  
  TonsilSleepData$preBMI[i] <- allstudies$BMIlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1]
  TonsilSleepData$preAHI[i] <- allstudies$AHIlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1]
  TonsilSleepData$preOAHI[i] <- allstudies$OAHIlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1]
  TonsilSleepData$preNOAHI[i] <- allstudies$NOAHIlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1]
  TonsilSleepData$preROAHI[i] <- allstudies$ROAHIlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1]
  TonsilSleepData$preODI[i] <- allstudies$ODIlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1]
  TonsilSleepData$preO2nadir[i] <- allstudies$O2nadirlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1]
  
  thisdate <- as.Date(TonsilSleepData$postsurgdate[i], "%m/%d/%Y")
  
  TonsilSleepData$postBMI[i] <- allstudies$BMIlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1]
  TonsilSleepData$postAHI[i] <- allstudies$AHIlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1]
  TonsilSleepData$postOAHI[i] <- allstudies$OAHIlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1]
  TonsilSleepData$postNOAHI[i] <- allstudies$NOAHIlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1]
  TonsilSleepData$postROAHI[i] <- allstudies$ROAHIlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1]
  TonsilSleepData$postODI[i] <- allstudies$ODIlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1] 
  TonsilSleepData$postO2nadir[i] <- allstudies$O2nadirlist[allstudies$DOBlist == thisDOB & allstudies$DORlist == thisdate][1]
}

write.csv(TonsilSleepData, "Z:/Research Coordinator/_Electronic Regulatory Binders/63255 Pediatric Tonsillectomy OSA outcomes - Karelsky/TAsurgdata_27DEC2016.csv")
  
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