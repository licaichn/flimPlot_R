# barplotFlim1D is used for ploting the barplot of lifetime over different timepoints

#    The function is written using ggplot, need to install ggplot2 package
#    The function does not compare differnet groups at each time point

#    inputData:  the output from readMatFlimRoi1D, the inputData should include the mean and 
#    and std of the Flim variables like INT, LT, INTR; the odd index column is mean; the even
#    index column is std
#    timepoints:  a vector of time points used as x labels
#    No_plots:  the No. of the plots going to be ploted
#    NameOfSlectedField:  the plot name going to be ploted, like('meanINTR1')
#    folderName:  the name of the folder to save the data if wanted
#    imageSize:  the size of the image to be saved, a 2-elements vector, c(width, height)

#    Copyright:2017-, Cai Li, UC Davis

barplotFlim1D <- function(inputData, timepoints, yAxesLim, nameOfXlabel, nameOfSlectedField, folderName,
                          imageSize) {
  
  library(ggplot2)
  
  N_STATISTICS = 2
  
  # Determine which field to be ploted
  
  if (missing(nameOfSlectedField)) {
    No_plots = seq(1, length(inputData), 2)
  }
  else {
    No_plots = c(grep(nameOfSlectedField, colnames(inputData)))
  }
  
  # make timepoints an ordered factor
  # avoid auto order of timepoints
  timepoints = factor(timepoints, levels = timepoints)
  
  # Plot
  
  for (iField in No_plots) {
    p = ggplot(data = inputData, aes(x = timepoints,
                                     y  = inputData[[iField]])) + 
     geom_bar(stat = "identity") + 
        
     # add errorbar
     geom_errorbar(aes(ymin = inputData[[iField]] - inputData[[iField + 1]],
                       ymax = inputData[[iField]] + inputData[[iField + 1]]),
                   width = 0.2, 
                   position = position_dodge(0.9)) +
      
     # add Y limit
     coord_cartesian(ylim = yAxesLim) +
        
     # add labels
     ylab(colnames(inputData)[iField]) +
     xlab(nameOfXlabel) +
        
     # white background, add the axis lines
     theme_classic() +
        
     # font size
     theme(text = element_text(size = 20))
    
    # Save the data or Not
    if(missing(imageSize)){
      widthImage = 5
      heightImage = 5
    }
    else {
      widthImage = imageSize[1]
      heightImage = imageSize[2]
    }
    
    if(!missing(folderName)){
      p = p + ggsave(paste0(folderName, '/',
                           colnames(inputData)[iField], '.png'), 
                     width = widthImage,
                     height = heightImage)
    }
  }
  
  
  # show the last image if missing nameOfSelectedField
  # show the figure if having nameOfSelectedField
  return(p)
}

# Example  
# coltest = c("a", "b", "c", "d", "e");
# barplotFlim1D(tes, coltest, 'time', folderName = '/Users/caili/Desktop/test')
