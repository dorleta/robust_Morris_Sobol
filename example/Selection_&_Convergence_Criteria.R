#--------------------------------------------------------------------------------------
# 
#   |    Implementation of the Selection and Convergence Criterion defined in :   |
#   |                                                                             |
#   |            "Robust combination of the Morris and Sobol methods              |
#   |                    in complex multidimensional models"                      | 
#   |                                                                             |
#   |            Submitted to Environmental Modelling & Software,                 | 
#   |                                                                             |
#   |       by Dorleta Garcia, Inmaculada Arostegui and Raul Prellezo             |
#
#  
#  ** What it is needed to run this script:
#
#   
#       * The results of the morris method applied to a simulation model with 
#         multidimensional output. The results need to provided in a data frame 
#         with 3 columns: 
#                      
#                           | 'inpFact' | 'outVar' | 'AEE' | 
#
#          Each row contains the absolute elementary effect (AEE) of the input 
#          factor in column 'inpFact' for ouput variable in 'outVar' column.
#
#       * The results of a boostrap of the morris methods  provided in a data frame 
#         with 4 columns: 
#
#                          | 'inpFact' | 'outVar' | 'AEE' | 'bootit'|
#         
#         
#
#  ** Steps followed in the script: 
#
#      * The same steps are applied to the output of the morris method with different 
#        number of trajectories, starting from the application with less number of  
#        trajectories (25 trajectories in this case). The steps are replicated 
#        increasing the number of trajectories in the morris method until the  
#        number of factors selected in more that alpha*Nboot iterations do not 
#        increase with the number of trajectories.
#        The morris method is not applied in this script so in reality in must 
#        be combined with the functions in 'sensitivity' package,  personal code  
#        to calculate the elementary effects or any form of implementation of the 
#        morris method.
#
#       
#        1. For each output variable generate the barplot of the absolute elementary effects of
#           each input factor.
#        2. Using the barplots select the number of input factors per output variable that
#           result in the selection of K_EE input factors.
#        3. Calibrate the selection criteria using the visual selection performed in step 2 and 
#           the base application of the morris method.
#        4. Apply the selection criteria to the boostrap application of the Morris method and 
#           identify the number of factors that have been selected in more than alpha*Nboot 
#           of the iterations.
#            
# 
# 2019/08/01
#---------------------------------------------------------------------------------------------------------



# load ggplot2 library to build the barplots with the absolute elementary effects per output variable.
library(ggplot2)
library(gridExtra)


# Set the working directory.
wd <- "C:/use/GitHub/robust_Morris_Sobol/"
setwd(wd)

# Read the functions  needed to implement the criteria stored in the 'code' folder of the GitHub repository. 
source('./code/Selection_&_Convergence_Criteria_Functions.R')
         
         
#----------------------------------------------------------------------------------------------------------
#  0. GENERAL SETTINGS :
##
##  - K_EE: The objective number of input factors to be selected.
##  - Nboot: Number of iterations in the boostrap.
##  - alpha: Proportion of iterations the factors need to be selected to keep them.
#
#-----------------------------------------------------------------------------------
K_EE  <- 15 
Nboot <- 500
alpha <- 0.95



######-------------------------------------------------------------------------------
###    25 TRAJECTORIES
#####--------------------------------------------------------------------------------

load('./example/AEE_25.RData')

# 1. Generate the barplots with the absolute elementary effects.
#    The 25 input factors with the highest AEE for each output variable.

plots <- list()
for(id in unique(AEE$outVar)){
  x1 <- subset(AEE, outVar == id)
  x1 <- cbind(x1[order(x1$AEE, decreasing = TRUE),], ord = 135:1)[1:25,]
  
  plots[[id]] <- ggplot(x1,aes(x = factor(ord), y = AEE)) + geom_col() + coord_flip() +
    scale_x_discrete(breaks = x1$ord, labels = x1$name) +
    ggtitle(id)
  
}

grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)


# 2. Visual selection of the input factors. 
Nvis <- c(ssb_HKE = 3,   ssb_HOM = 2,  rec_HKE = 2,     rec_HOM = 4, catch_HKE = 4, catch_HOM = 3)  

# Identify the input factor selected in the visual selection.
Fvis <- unique(unlist(lapply(names(Nvis), function(id) 
  as.character(subset(AEE, outVar == id)[1:Nvis[id],'name']))))


## 3. Calibration of the selection criterion
FM <- selection_criterion(AEE, K_EE, Nvis)

## 4. Application of the selection criteria to the boostrap of the Morris method.
load('./example/AEE_Boot_25.RData')

F25_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F25 <- names(F25_all[which(F25_all>=alpha*Nboot)])
length(F25)




######-------------------------------------------------------------------------------
###    50 TRAJECTORIES
######-------------------------------------------------------------------------------

load('./example/AEE_50.RData')

# 1. Generate the barplots with the absolute elementary effects.
#    The 25 input factors with the highest AEE for each output variable.

plots <- list()
for(id in unique(AEE$outVar)){
  x1 <- subset(AEE, outVar == id)
  x1 <- cbind(x1[order(x1$AEE, decreasing = TRUE),], ord = 135:1)[1:25,]
  
  plots[[id]] <- ggplot(x1,aes(x = factor(ord), y = AEE)) + geom_col() + coord_flip() +
    scale_x_discrete(breaks = x1$ord, labels = x1$name) +
    ggtitle(id)
  
}

grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)


# 2. Visual selection of the input factors. 
Nvis <- c(ssb_HKE = 5,   ssb_HOM = 4,  rec_HKE = 6,     rec_HOM = 6, catch_HKE = 5, catch_HOM = 3)  

# Identify the input factor selected in the visual selection.
Fvis <- unique(unlist(lapply(names(Nvis), function(id) 
  as.character(subset(AEE, outVar == id)[1:Nvis[id],'name']))))


## 3. Calibration of the selection criterion
FM <- selection_criterion(AEE, K_EE, Nvis)

## 4. Application of the selection criteria to the boostrap of the Morris method.
load('./example/AEE_Boot_50.RData')

F50_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F50 <- names(F50_all[which(F50_all>=alpha*Nboot)])
length(F50)



######-------------------------------------------------------------------------------
### 100 TRAJECTORIES
######-------------------------------------------------------------------------------

load('./example/AEE_100.RData')


# 1. Generate the barplots with the absolute elementary effects.
#    The 25 input factors with the highest AEE for each output variable.

plots <- list()
for(id in unique(AEE$outVar)){
  x1 <- subset(AEE, outVar == id)
  x1 <- cbind(x1[order(x1$AEE, decreasing = TRUE),], ord = 135:1)[1:25,]
  
  plots[[id]] <- ggplot(x1,aes(x = factor(ord), y = AEE)) + geom_col() + coord_flip() +
    scale_x_discrete(breaks = x1$ord, labels = x1$name) +
    ggtitle(id)
  
}


grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)


# 2. Visual selection of the input factors. 
Nvis <- c(ssb_HKE = 1,   ssb_HOM = 4,  rec_HKE = 3,     rec_HOM = 4, catch_HKE = 2, catch_HOM = 2)  

# Identify the input factor selected in the visual selection.
Fvis <- unique(unlist(lapply(names(Nvis), function(id) 
  as.character(subset(AEE, outVar == id)[1:Nvis[id],'name']))))


## 3. Calibration of the selection criterion
FM <- selection_criterion(AEE, K_EE, Nvis)

## 4. Application of the selection criteria to the boostrap of the Morris method.
load('./example/AEE_Boot_100.RData')

F100_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F100 <- names(F100_all[which(F100_all>=alpha*Nboot)])
length(F100)



######-------------------------------------------------------------------------------
###       150 TRAJECTORIES
######-------------------------------------------------------------------------------

load('./example/AEE_150.RData')

# 1. Generate the barplots with the absolute elementary effects.
#    The 25 input factors with the highest AEE for each output variable.

plots <- list()
for(id in unique(AEE$outVar)){
  x1 <- subset(AEE, outVar == id)
  x1 <- cbind(x1[order(x1$AEE, decreasing = TRUE),], ord = 135:1)[1:25,]
  
  plots[[id]] <- ggplot(x1,aes(x = factor(ord), y = AEE)) + geom_col() + coord_flip() +
    scale_x_discrete(breaks = x1$ord, labels = x1$name) +
    ggtitle(id)
  
}


grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)


# 2. Visual selection of the input factors. 
Nvis <- c(ssb_HKE = 5,   ssb_HOM = 4,  rec_HKE = 6,     rec_HOM = 4, catch_HKE = 4, catch_HOM = 5)  

# Identify the input factor selected in the visual selection.
Fvis <- unique(unlist(lapply(names(Nvis), function(id) 
  as.character(subset(AEE, outVar == id)[1:Nvis[id],'name']))))


## 3. Calibration of the selection criterion
FM <- selection_criterion(AEE, K_EE, Nvis)

## 4. Application of the selection criteria to the boostrap of the Morris method.
load('./example/AEE_Boot_150.RData')

F150_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F150 <- names(F150_all[which(F150_all>=alpha*Nboot)])
length(F150)




