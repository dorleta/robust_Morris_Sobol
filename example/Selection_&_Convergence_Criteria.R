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

# load ggplot2 library to build the barplots with the absolute elementary effects per output variable.
library(ggplot2)
library(gridExtra)


# Set the working directory
wd <- "C:/use/GitHub/robust_Morris_Sobol/"
setwd(wd)

# Read the functions to with the selection and convergence criteria 
source('./code/Selection_&_Convergence_Criteria_Functions.R')
         
         
#-----------------------------------------------------------------------------------
## Settings:
##  - K_EE: The objective number of input factors to be selected.
#-----------------------------------------------------------------------------------
K_EE  <- 20 
Nboot <- 500
alpha <- 0.95


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

load('./example/AEE.RData')

plots <- list()
for(id in unique(AEE$outVar)){
  x1 <- subset(AEE, outVar == id)
  x1 <- cbind(x1[order(x1$AEE, decreasing = TRUE),], ord = 135:1)[1:50,]
  
  plots[[id]] <- ggplot(x1,aes(x = factor(ord), y = AEE)) + geom_col() + coord_flip() +
    scale_x_discrete(breaks = x1$ord, labels = x1$name) +
    ggtitle(id)

}


grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)


# The number of factors selected in each output variable.
Fvis <- c(ssb_HKE = 5,   ssb_HOM = 6,  f_HKE = 10,     f_HOM = 8, catch_HKE = 9, catch_HOM = 7)  

# Identify the input factor selected with Fvis.
inpFactVis <- unique(unlist(lapply(names(Fvis), function(id) 
                        as.character(subset(AEE, outVar == id)[1:Fvis[id],'name']))))



#-------------------------------------------------------------------------------------
## 2. SELECTION CRITERION
##      * Apply the  "selection_criterion" function.
##      * In this step we calibrate the method using the visual selection and obtain the weights
##        that we will use in the bootstrap to calculate the weighted criterion.
##
#----------------------------------------------------------------------------------------------------
FM <- selection_criterion(AEE, K_EE, Fvis)


#-------------------------------------------------------------------------------------
## 3. Convergence
##      * Apply the  "selection_criterion" function.
#----------------------------------------------------------------------------------------------------

load('./example/AEE_Boot_25.RData')

F25_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F25 <- names(F25_all[which(F25_all>=alpha*Nboot)])
length(F25)


load('./example/AEE_Boot_50.RData')

F50_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F50 <- names(F50_all[which(F50_all>=alpha*Nboot)])
length(F50)


load('./example/AEE_Boot_100.RData')

F100_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F100 <- names(F100_all[which(F100_all>=alpha*Nboot)])
length(F100)


load('./example/AEE_Boot_150.RData')

F150_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F150 <- names(F150_all[which(F150_all>=alpha*Nboot)])
length(F150)


load('./example/AEE_Boot_200.RData')

F200_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F200 <- names(F200_all[which(F200_all>=alpha*Nboot)])
length(F200)


load('./example/AEE_Boot_250.RData')

F250_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F250 <- names(F250_all[which(F250_all>=alpha*Nboot)])
length(F250)



load('./example/AEE_Boot_300.RData')
F300_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F300 <- names(F300_all[which(F300_all>=alpha*Nboot)])
length(F300)




