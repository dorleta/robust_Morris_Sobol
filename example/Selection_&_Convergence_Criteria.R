#--------------------------------------------------------------------------------------
#  Implementation of the Selection and Convergence Criterion defined in :
#    
#        "Robust combination of the Morris and Sobol methods 
#             in complex multidimensional models"
#
#  Submitted to Environmental Modelling & Software. 
#
#  What it is needed to run this script:
#
#
#
#
# Dorleta Garcia, Inmaculada Arostegui and Raul Prellezo
# 2019/08/01
#--------------------------------------------------------------------------------------

# load(file = './output/Morris_For_Selection_Criteria.RData')
# AEE <- subset(res, stat == 'mean' & indicator %in% c('catch', 'rec', 'ssb', 'f', 'tac'))
# names(AEE)[c(4,6)] <- c('stock', 'AEE')
# AEE <- cbind(AEE, outVar = paste(AEE$indicator, AEE$stock, sep = "_"))
# 
# AEE <- AEE[, c(3,1,7,2)]
# names(AEE)[c(4)] <- c('AEE')
# save(AEE, file = 'C:/Users/dgarcia/Dropbox/PhD_Dorleta/Papers/3_GSA_MethodOnly/revision_EMS/Algorithm/AEE.RData')


# load ggplot2 library to build the barplots with the absolute elementary effects per output variable.
library(ggplot2)


# Set the working directory
wd <- "C:/Users/dgarcia/Dropbox/PhD_Dorleta/Papers/3_GSA_MethodOnly/revision_EMS/Algorithm/"
setwd(wd)

# Read the functions to with the selection and convergence criteria 
source('./Garcia_2019_Selection_&_Convergence_Criteria_Functions.R')
         
         
#-----------------------------------------------------------------------------------
## K_EE: The objective number of input factors to be selected.
#-----------------------------------------------------------------------------------
K_EE <- 50 


#-------------------------------------------------------------------------------------
## 1. VISUAL SELECTION
##      
##      * The results of morris methods are provided in a data frame in 4 columns: 
##                      c('name', 'param', 'outVar', 'AEE') 
##      * First we create a set of plots to summarize the results of the application 
##        of the Morris method.
##        We create one plot per output variable (outVar) with the AEE of the 50 
##        input factors with the highest AEE. The plots are stored in the working 
##        directory with the name 'AEE.pdf'. 
##      * Based on these plots, perform a visual selection that results in the 
##        selection of K_EE factors.
##
#------------------------------------------------------------------------------------

load('AEE.RData')

pdf('AEE.pdf')
for(id in unique(AEE$outVar)){
  x1 <- subset(AEE, outVar == id)
  x1 <- cbind(x1[order(x1$AEE, decreasing = TRUE),], ord = 135:1)[1:50,]
  
  p <- ggplot(x1,aes(x = factor(ord), y = AEE)) + geom_col() + coord_flip() +
    scale_x_discrete(breaks = x1$ord, labels = x1$name) +
    ggtitle(id)
  print(p)
}
dev.off()

# The number of factors selected in each output variable.
Fvis <- c(ssb_HKE = 5,   ssb_HOM = 6,   ssb_MEG = 5,   ssb_LDB = 5,   ssb_MON = 5,   
          rec_HKE = 3,   rec_HOM = 7,   rec_MEG = 6,   rec_LDB = 5,   rec_MON = 5,   
          f_HKE = 2,     f_HOM = 8,     f_MEG = 4,     f_LDB = 5,     f_MON = 7,     
          catch_HKE = 6, catch_HOM = 4, catch_MEG = 2, catch_LDB = 7, catch_MON = 3, 
          tac_HKE = 7,   tac_HOM = 7,   tac_MEG = 5,   tac_LDB = 5,   tac_MON = 5)  

# Identify the input factor selected with Fvis.
inpFactVis <- unique(unlist(lapply(names(Fvis), function(id) 
                        as.character(subset(AEE, outVar == id)[1:Fvis[id],'name']))))



#-------------------------------------------------------------------------------------
## 2. SELECTION CRITERION
##      * Apply the  "selection_criterion" function.
#----------------------------------------------------------------------------------------------------
FM <- selection_criterion(AEE, K_EE, Fvis)


#-------------------------------------------------------------------------------------
## 3. Convergence
##      * Apply the  "selection_criterion" function.
#----------------------------------------------------------------------------------------------------
files <- c("morris_25_convergence.RData", "morris_50_convergence.RData", "morris_100_convergence.RData",
           "morris_150_convergence.RData", "morris_200_convergence.RData", "morris_250_convergence.RData",
           "morris_300_convergence.RData")

kkk <- 1
pp <- c(25, 50, 100, 150, 200, 250, 300)
for(f in files){

  load(paste('C:/use/OneDrive - AZTI/Tesia/Sensitivity Analysis/output/', f, sep = ""))
  save(bootmorris, file = paste('AEE_Boot_', pp[kkk], '.RData'))
  kkk <- kkk+1

}


bootmorris