# readMatFlimRoi1D is used for read all the mat files in a folder, each of which
#   includes the mean and std for lifetime, intensity ratio and intensity; Then return
#   a dataframe includes the 1D vectors of lifetime, intensity ratio and intensity over
#   different files; 

#   folderPath: Prefer absolute path

#   Note: add "No_" to the head of the file name to determine the sequence
#   Example: "1_20170621 SolvEffect BP_3h ROI"

#   NaN will become 0
#   Can be processed by other xxx1D functions

#   Copyright, 2017-, Cai Li, UC Davis

readMatFlimRoi1D = function(folderPath) {
  
  library(R.matlab)
  
  fileNamesInFolder = list.files(path = folderPath, pattern = "\\.mat$")
  
  N_FILES = length(fileNamesInFolder)
  N_CHANNELS = 4
  N_STATISTICS = 2
  
  colNames = c('meanINT1', 'stdINT1', 'meanINT2', 'stdINT2',
               'meanINT3', 'stdINT3', 'meanINT4', 'stdINT4',
               'meanLT1', 'stdLT1', 'meanLT2', 'stdLT2',
               'meanLT3', 'stdLT3', 'meanLT4', 'stdLT4',
               'meanINTR1', 'stdINTR1', 'meanINTR2', 'stdINTR2',
               'meanINTR3', 'stdINTR3', 'meanINTR4', 'stdINTR4')
  outputData = as.data.frame(
    matrix(nrow = N_FILES, ncol = length(colNames),
           dimnames = list(NULL, colNames)))
  
  for (iFile in 1:N_FILES) {
    fileName = paste(folderPath, fileNamesInFolder[iFile], sep = '/')
    fileData = readMat(fileName)
    for (jVariable in 1:(length(fileData)/N_STATISTICS)) {
      for (kChannel in 1:N_CHANNELS){
        for (lStatistics in 1:N_STATISTICS) {
          if (!is.na(fileData[[lStatistics +
                              N_STATISTICS * (jVariable - 1)]][kChannel])) {
            outputData[[lStatistics + 
                         N_STATISTICS * (kChannel - 1) +
                         N_CHANNELS * N_STATISTICS * (jVariable - 1)]][iFile] = 
              fileData[[lStatistics +
                         N_STATISTICS * (jVariable - 1)]][kChannel]
          } # if the value is not NaN
          else {
            outputData[[lStatistics + 
                          N_STATISTICS * (kChannel - 1) +
                          N_CHANNELS * N_STATISTICS * (jVariable - 1)]][iFile] = 0
          }
        } # loop the statistics, like mean, std
      } # loop the channels, like CH1, CH2
    } # loop the variables, like INT, LT, INTR
  } # loop the files in the folder
  
  return(outputData)
  
}

# Example
 tes = readMatFlimRoi1D('/Users/caili/Google Drive/Lab/Data/20170621 SolvEffect BP ROI Data')