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
# The output of the morris method for several paths
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
K_EE  <- 15 
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

######-----------------------------------------
###    25 PATHS
#####---------------------------------------------


# 1. Visual selection of the input factors
load('./example/AEE_25.RData')

# Plot the 25 input factors with the highest AEE for each output variable.
plots <- list()
for(id in unique(AEE$outVar)){
  x1 <- subset(AEE, outVar == id)
  x1 <- cbind(x1[order(x1$AEE, decreasing = TRUE),], ord = 135:1)[1:25,]
  
  plots[[id]] <- ggplot(x1,aes(x = factor(ord), y = AEE)) + geom_col() + coord_flip() +
    scale_x_discrete(breaks = x1$ord, labels = x1$name) +
    ggtitle(id)
  
}


grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)


# The number of factors selected for each output variable based on the visual inspection of the barplots.
Nvis <- c(ssb_HKE = 3,   ssb_HOM = 2,  rec_HKE = 2,     rec_HOM = 4, catch_HKE = 4, catch_HOM = 3)  

# Identify the input factor selected in the visual selection.
Fvis <- unique(unlist(lapply(names(Nvis), function(id) 
  as.character(subset(AEE, outVar == id)[1:Nvis[id],'name']))))


## 2. Calibration of the selection criterion
FM <- selection_criterion(AEE, K_EE, Nvis)

## 3. Application of the selection criteria to the boostrap of the Morris method.
load('./example/AEE_Boot_25.RData')

F25_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F25 <- names(F25_all[which(F25_all>=alpha*Nboot)])
length(F25)




######-----------------------------------------
###    50 PATHS
#####---------------------------------------------

# 1. Visual selection of the input factors
load('./example/AEE_50.RData')

# Plot the 25 input factors with the highest AEE for each output variable.
plots <- list()
for(id in unique(AEE$outVar)){
  x1 <- subset(AEE, outVar == id)
  x1 <- cbind(x1[order(x1$AEE, decreasing = TRUE),], ord = 135:1)[1:25,]
  
  plots[[id]] <- ggplot(x1,aes(x = factor(ord), y = AEE)) + geom_col() + coord_flip() +
    scale_x_discrete(breaks = x1$ord, labels = x1$name) +
    ggtitle(id)
  
}


grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)


# The number of factors selected for each output variable based on the visual inspection of the barplots.
Nvis <- c(ssb_HKE = 5,   ssb_HOM = 4,  rec_HKE = 6,     rec_HOM = 6, catch_HKE = 5, catch_HOM = 3)  

# Identify the input factor selected in the visual selection.
Fvis <- unique(unlist(lapply(names(Nvis), function(id) 
  as.character(subset(AEE, outVar == id)[1:Nvis[id],'name']))))


## 2. Calibration of the selection criterion
FM <- selection_criterion(AEE, K_EE, Nvis)

## 3. Application of the selection criteria to the boostrap of the Morris method.
load('./example/AEE_Boot_50.RData')

F50_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F50 <- names(F50_all[which(F50_all>=alpha*Nboot)])
length(F50)





######-----------------------------------------
### 100 PATHS
#####---------------------------------------------

## 1. Visual selection of the input factors
load('./example/AEE_100.RData')

# Plot the 25 input factors with the highest AEE for each output variable.
plots <- list()
for(id in unique(AEE$outVar)){
  x1 <- subset(AEE, outVar == id)
  x1 <- cbind(x1[order(x1$AEE, decreasing = TRUE),], ord = 135:1)[1:25,]
  
  plots[[id]] <- ggplot(x1,aes(x = factor(ord), y = AEE)) + geom_col() + coord_flip() +
    scale_x_discrete(breaks = x1$ord, labels = x1$name) +
    ggtitle(id)
  
}


grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)


# The number of factors selected for each output variable based on the visual inspection of the barplots.
Nvis <- c(ssb_HKE = 1,   ssb_HOM = 4,  rec_HKE = 3,     rec_HOM = 4, catch_HKE = 2, catch_HOM = 2)  

# Identify the input factor selected in the visual selection.
Fvis <- unique(unlist(lapply(names(Nvis), function(id) 
  as.character(subset(AEE, outVar == id)[1:Nvis[id],'name']))))


## 2. Calibration of the selection criterion
FM <- selection_criterion(AEE, K_EE, Nvis)

## 3. Application of the selection criteria to the boostrap of the Morris method.
load('./example/AEE_Boot_100.RData')

F100_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F100 <- names(F100_all[which(F100_all>=alpha*Nboot)])
length(F100)




######--------------------------------------------
###       150 PATHS
#####---------------------------------------------


## 1. Visual selection of the input factors.
load('./example/AEE_150.RData')

# Plot the 25 input factors with the highest AEE for each output variable.
plots <- list()
for(id in unique(AEE$outVar)){
  x1 <- subset(AEE, outVar == id)
  x1 <- cbind(x1[order(x1$AEE, decreasing = TRUE),], ord = 135:1)[1:25,]
  
  plots[[id]] <- ggplot(x1,aes(x = factor(ord), y = AEE)) + geom_col() + coord_flip() +
    scale_x_discrete(breaks = x1$ord, labels = x1$name) +
    ggtitle(id)
  
}


grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)


# The number of factors selected for each output variable based on the visual inspection of the barplots.
Nvis <- c(ssb_HKE = 5,   ssb_HOM = 4,  rec_HKE = 6,     rec_HOM = 4, catch_HKE = 4, catch_HOM = 5)  

# Identify the input factor selected in the visual selection.
Fvis <- unique(unlist(lapply(names(Nvis), function(id) 
  as.character(subset(AEE, outVar == id)[1:Nvis[id],'name']))))


## 2. Calibration of the selection criterion
FM <- selection_criterion(AEE, K_EE, Nvis)

## 3. Application of the selection criteria to the boostrap of the Morris method.
load('./example/AEE_Boot_150.RData')

F150_all <- selection_criterion_boot(AEEboot, K_EE, FM$weights)

F150 <- names(F150_all[which(F150_all>=alpha*Nboot)])
length(F150)




