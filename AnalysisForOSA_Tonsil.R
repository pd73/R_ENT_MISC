#Script for extracting sleep study notes from text export
sleep_notes <- read.csv("Z:/Research Coordinator/_Electronic Regulatory Binders/61735 REM vs nREM Benoit/Allen_05092016.csv", comment.char="#", stringsAsFactors=FALSE)
sleep_notes$MRN <- as.factor(sleep_notes$MRN)
sleep_notes$NOTE_CSN_ID <- as.factor(sleep_notes$NOTE_CSN_ID)
sleep_notes$X <- NULL
sleep_notes$MRN.1 <- NULL
sleep_notes$ICD9_LIST<- NULL
!is.na(sleep_notes$NOTE_CSN_ID)
sleep_notes <- sleep_notes[!is.na(sleep_notes$NOTE_CSN_ID),]


MRNlist <- unique(sleep_notes$MRN)
CSNlist <- unique(sleep_notes$NOTE_CSN_ID)

AGElist <- rep(NA, length(CSNlist)) 
GENDERlist <- rep(NA, length(CSNlist))
Heightlist <- rep(NA, length(CSNlist))  
Weightlist <- rep(NA, length(CSNlist))

Notelist <- rep(NA, length(CSNlist))

draft <-  rep(NA, length(CSNlist))
IDlist <- rep(NA, length(CSNlist))

DOBlist <-  as.Date(rep(NA, length(CSNlist)))
check_Heightlist <- rep(NA, length(CSNlist))  
DORlist <-  as.Date(rep(NA, length(CSNlist)))
check_Weightlist <- rep(NA, length(CSNlist))
check_MRNlist <- rep(NA, length(CSNlist))
BMIlist <-  rep(NA, length(CSNlist))

sleep_efficiency <-  rep(NA, length(CSNlist))
rem_latency <-  rep(NA, length(CSNlist))
awakenings_index <-  rep(NA, length(CSNlist))
arousal_index <-  rep(NA, length(CSNlist))

n1_stage_dur  <-  rep(NA, length(CSNlist))
n1_stage_perc <-  rep(NA, length(CSNlist))
n2_stage_dur <-  rep(NA, length(CSNlist))
n2_stage_perc <-  rep(NA, length(CSNlist))
n3_stage_dur <-  rep(NA, length(CSNlist))
n3_stage_perc <-  rep(NA, length(CSNlist))

REM_dur <-  rep(NA, length(CSNlist))
rem_perc <-  rep(NA, length(CSNlist))
nrem_dur <-  rep(NA, length(CSNlist))
nrem_perc <-  rep(NA, length(CSNlist))

AHIlist <-  rep(NA, length(CSNlist))
OAHIlist <-  rep(NA, length(CSNlist))
NOAHIlist <-  rep(NA, length(CSNlist))
ROAHIlist <-  rep(NA, length(CSNlist))
ODIlist <-  rep(NA, length(CSNlist))

O2nadirlist <- rep(NA, length(CSNlist))
OSADxlist <- rep(NA, length(CSNlist))

lsat_rem <- rep(NA, length(CSNlist))
last_nrem <- rep(NA, length(CSNlist))
lsat_total <- rep(NA, length(CSNlist))

dur_below_90 <- rep(NA, length(CSNlist))
perc_below_90 <- rep(NA, length(CSNlist))

### below here is scratch

for (i in 1:length(CSNlist)) {
  AGElist[i] <- sleep_notes$AGE[sleep_notes$NOTE_CSN_ID == CSNlist[i]][1]
  GENDERlist[i] <- sleep_notes$GENDER[sleep_notes$NOTE_CSN_ID == CSNlist[i]][1]
  
  temp <- as.character(sleep_notes$HEIGHT[sleep_notes$NOTE_CSN_ID == CSNlist[i]][1])
  Heightlist[i] <- round(2.54*(as.numeric(substr(temp,1,1))*12 + as.numeric(substr(temp,3,6))))
  
  Weightlist[i] <- signif(28.35*(sleep_notes$WEIGHT[sleep_notes$NOTE_CSN_ID == CSNlist[i]][1])/1000, digits = 3)
  
  Notelist[i] <- paste(sleep_notes$NOTE_TEXT[sleep_notes$NOTE_CSN_ID == CSNlist[i]], collapse = "; ")
  
  draft[i] <- regexpr("DRAFT", Notelist[i])
  
  loc <- regexpr("Date of Birth:", Notelist[i])
  DOBlist[i] <- as.Date(substr(Notelist[i], loc[1]+15, loc[1]+24), "%m/%d/%Y")
  
  loc <- regexpr("Recording Date:", Notelist[i])
  DORlist[i] <- as.Date(substr(Notelist[i], loc[1]+16, loc[1]+25), "%m/%d/%Y")
  
  loc <- regexpr("Patient ID:", Notelist[i])
  IDlist[i] <- as.numeric(substr(Notelist[i], loc[1]+11, loc[1]+19))
  
  loc <- regexpr("BMI", Notelist[i])
  BMIlist[i] <- as.numeric(substr(Notelist[i], loc[1]+4, loc[1]+8))
  
  loc <- regexpr("Sleep Efficiency", Notelist[i])
  sleep_efficiency[i] <- as.numeric(substr(Notelist[i], loc[1]+35, loc[1]+39))
  
  
  
  loc <- regexpr("Apnea-hypopnea index ", Notelist[i])
  AHIlist[i] <- as.numeric(substr(Notelist[i], loc[1]+27, loc[1]+32))
  
  loc <- regexpr("Obstructive Apnea-hypopnea index ", Notelist[i])
  OAHIlist[i] <- as.numeric(substr(Notelist[i], loc[1]+39, loc[1]+44))
  
  loc <- regexpr("During NREM", Notelist[i])
  NOAHIlist[i] <- as.numeric(substr(Notelist[i], loc[1]+12, loc[1]+17))
  
  loc <- regexpr("During REM", Notelist[i])
  ROAHIlist[i] <- as.numeric(substr(Notelist[i], loc[1]+11, loc[1]+16))
  
  loc <- regexpr("Oxygen Desaturation Index", Notelist[i])
  ODIlist[i] <- as.numeric(substr(Notelist[i], loc[1]+25, loc[1]+29))
  
  loc <- regexpr("Lowest saturation", Notelist[i])  
  O2nadirlist[i] <-min(as.numeric(unlist(strsplit(substr(Notelist[i], loc[1]+18, loc[1]+38),'%'))), na.rm = TRUE)
  # NB this will fail if there are no % signs after the values, which I saw for record 30 - replace manually
  
  #OSA Dx
  #Patients who undergo polysomnography will be classified as 
  #normal (AHI < 1 and O2 nadir >90%), 
  #mild OSA (AHI 1-5 and O2 nadir >90%), 
  #moderate OSA (AHI 5-10 and/or O2 nadir <90%), or 
  #severe OSA (AHI >10 and/or O2 nadir <80%). 
  ifelse(O2nadirlist[i]<90, OSADxlist[i] <- "Moderate",OSADxlist[i] <-"Undefined")
  
  ifelse(AHIlist[i] < 1, OSADxlist[i] <- "Normal", 
         ifelse(AHIlist[i] < 5, OSADxlist[i] <- "Mild",
                ifelse(AHIlist[i] < 10, OSADxlist[i] <- "Moderate", 
                       ifelse(AHIlist[i] > 10,
                              OSADxlist[i] <- "Severe", OSADxlist[i] <-"Undefined")))) 
  ifelse(O2nadirlist[i]<80, OSADxlist[i] <- "Severe", "")
  
}

#Polysomnographic data 
#  AHI, obstructive AHI, AHI (REM), AHI (non-REM), 
#  oxygen nadir, %time <90% O2 saturation, 
#  %time end-tidal CO2>50, peak end-tidal CO2, 
#  %time transcutaneous CO2>50, peak transcutaneous CO2

#table(OSADxlist[LMlist == TRUE & refluxlist == TRUE & AGElist <2])
#table(OSADxlist[LMlist == TRUE & refluxlist == FALSE & AGElist <2])
#table(OSADxlist[LMlist == FALSE & refluxlist == TRUE & AGElist <2])
#table(OSADxlist[LMlist == FALSE & refluxlist == FALSE & AGElist <2])


OutputData <- data.frame(draft, IDlist, DOBlist, DORlist, AGElist,GENDERlist, Heightlist, Weightlist, BMIlist, OSADxlist, O2nadirlist, AHIlist, OAHIlist,NOAHIlist,ROAHIlist,ODIlist)
write.table(OutputData, file="PedSleepClinicPatients_27DEC2016.csv",sep=",",row.names=F)

#OutputData <- data.frame(MRNlist[LMlist == TRUE], DOBlist[LMlist == TRUE], DORlist[LMlist == TRUE],AGElist[LMlist == TRUE],GENDERlist[LMlist == TRUE], refluxlist[LMlist == TRUE], OSADxlist[LMlist == TRUE], O2nadirlist[LMlist == TRUE], AHIlist[LMlist == TRUE], DXlist[LMlist == TRUE])
#write.table(OutputData, file="LM_SleepClinicPatients.csv",sep=",",row.names=F)
#careful, contains PHI

# make a new loop through the list of MRNs andpull out the unique dates of service
servicedates <- data.frame(MRN = integer(),
                           date_birth = as.Date(character()),
                           numServiceDays = integer(),
                           firstService = as.Date(character()),
                           ageatfirstservice = numeric(),
                           AHIfirst = numeric(),
                           secondService = as.Date(character()),
                           AHIsecond = numeric(),
                           thirdService = as.Date(character()),
                           AHIthird = numeric(),
                           fourthService = as.Date(character()),
                           AHIfourth = numeric(),
                           stringsAsFactors=FALSE
)

IDinNotes = unique(IDlist)

for (i in 1: length(IDinNotes)) { 
  
  temp <- as.Date(unique(na.omit(DORlist[IDlist == IDinNotes[i]])))
  temp2 <- na.omit(AHIlist[IDlist == IDinNotes[i]])
  temp3 <- na.omit(AGElist[IDlist == IDinNotes[i]])
  temp4 <- na.omit(BMIlist[IDlist == IDinNotes[i]])
  
  newrow <- data.frame(MRN = as.integer(as.character(IDinNotes[i])), 
                       date_birth = as.Date(na.omit(DOBlist[IDlist == IDinNotes[i]])[1]) ,
                       numServiceDays = as.integer(length(temp)),
                       firstService = NA,
                       ageatfirstservice = NA,
                       AHIfirst = NA,
                       BMIfirst = NA,
                       secondService = NA,
                       AHIsecond = NA,
                       BMIsecond = NA,
                       thirdService = NA,
                       AHIthird = NA,
                       BMIthird = NA,
                       fourthService = NA,
                       AHIfourth = NA,
                       BMIfourth = NA,
                       stringsAsFactors=FALSE
  ) 
  
  
  servicedates <- rbind(servicedates, newrow) 
  
  if (servicedates$numServiceDays[i] > 0){
    servicedates$firstService[i] = strftime(temp[1], format = "%Y-%m-%d")
    servicedates$ageatfirstservice[i] = temp3[1] 
    servicedates$AHIfirst[i] = temp2[1]
    servicedates$BMIfirst[i] = temp4[1]
  }
  if (servicedates$numServiceDays[i] > 1){
    servicedates$secondService[i] = strftime(temp[2], format = "%Y-%m-%d")
    servicedates$AHIsecond[i] = temp2[3]
    servicedates$BMIsecond[i] = temp4[3]
  }
  if (servicedates$numServiceDays[i] > 2){
    servicedates$thirdService[i] = strftime(temp[3], format = "%Y-%m-%d")
    servicedates$AHIthird[i] = temp2[5]
    servicedates$BMIthird[i] = temp4[5]
  }
  if (servicedates$numServiceDays[i] > 3){
    servicedates$fourthService[i] = strftime(temp[4], format = "%Y-%m-%d")
    servicedates$AHIfourth[i] = temp2[7]
    servicedates$BMIfourth[i] = temp4[7]
  }
  
  
} 
write.table(servicedates, file="PedSleepClinicPatients_DOS_identifiers_DEC2016.csv",sep=",",row.names=F)
# Manually remove 8 MRN-DOB mismatches
sleep_notes$MRN <- as.factor(sleep_notes$MRN)