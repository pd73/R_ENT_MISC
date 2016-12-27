# script to analyse tonsil study diary data
# Open excel file and read in the diary data
# calculate the inter-dose period on a day by day basis
require(XLConnect)
require(dplyr)

file <- "Z:/Research Coordinator/_Electronic Regulatory Binders/46411 Post-tonsilectomy Pain - Benoit/MPD_4_ResearchDay.xls"
wb <- loadWorkbook(file)

allSheets<- getSheets(wb)
numdiaries <- length(allSheets)-1
lastdiaryday <- vector('numeric',numdiaries)

numdosesperday <- matrix(0, nrow = numdiaries, ncol = 15)
avepainperday <- matrix(NA, nrow = numdiaries, ncol = 15)
averageperiodperday <- matrix(NA, nrow = numdiaries, ncol = 15)

for (j in 2:length(allSheets)-1) {
  
diary_data<- readWorksheet(wb, sheet=allSheets[j], startRow =3, endCol = 4,
                           colTypes = c(XLC$DATA_TYPE.NUMERIC,
                                        XLC$DATA_TYPE.NUMERIC,
                                        XLC$DATA_TYPE.NUMERIC,
                                        XLC$DATA_TYPE.NUMERIC),
                           forceConversion = TRUE)

subject <- readWorksheet(wb, sheet=allSheets[j], startRow =1, startCol = 1, endRow = 1, endCol = 1, header = F)[[1]]

lastdiaryday[j] <- max(diary_data$Post.operative.day)

diary_data$med_day_time <- diary_data$Post.operative.day+diary_data$Time.of.medication
diary_data$med_period <- c(0,diff(diary_data$med_day_time))

if (any(diary_data$med_period < 0)) {
  show(subject)
  show(diary_data$med_period) # 4/25/16 these were all cleaned-up
}

if (any(diary_data$Name.of.medication == 3)) {
  show(subject)
  temp <- diary_data[diary_data$Name.of.medication == 3,]
  temp <- temp[!is.na(temp$Name.of.medication),]
  show(temp) 
}

#morph <- diary_data[diary_data$Name.of.medication == 1,]

for (k in 1:lastdiaryday[j]) {
  temp <- diary_data$med_period[diary_data$Post.operative.day == k]
  temp <- temp[temp>0]
 
  numdosesperday[j,k] <- length(temp)
  avepainperday[j,k] <- mean(diary_data$Pain.score..0.10.[diary_data$Post.operative.day == k], na.rm = TRUE)
  averageperiodperday[j,k] <- mean(temp)*24
  
# beware negative values which indicate a data entry problem - these were manually cleaned up
  
  }
}

outfile1 <- "Z:/Research Coordinator/_Electronic Regulatory Binders/46411 Post-tonsilectomy Pain - Benoit/diaries_doses.csv"
outfile3 <- "Z:/Research Coordinator/_Electronic Regulatory Binders/46411 Post-tonsilectomy Pain - Benoit/diaries_averageperiod.csv"
outfile2 <- "Z:/Research Coordinator/_Electronic Regulatory Binders/46411 Post-tonsilectomy Pain - Benoit/diaries_averagepain.csv"


write.csv(numdosesperday, outfile1)
write.csv(averageperiodperday, outfile3)
write.csv(avepainperday, outfile2)


