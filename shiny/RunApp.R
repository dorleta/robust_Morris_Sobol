#----------------------------------------------------------------------------------------------
#
# Shiny app to show the results  of the Morris and Sobol methods in 
# 
# "Robust combination of the Morris and Sobol methods 
#             in complex multidimensional models"
#
#  Submitted to Environmental Modelling & Software. 
#
# The password is: flbeiaGSA
#
# by Dorleta Garcia, Inmaculada Arostegui and Raul Prellezo
# 
# 2018/09/010
#---------------------------------------------------------------------------------------------

rm(list = ls())

library(shiny)
library(shinyBS)
library(dplyr)
library(magrittr)


# Change the working directory 
setwd("~/GitHub/robust_Morris_Sobol/shiny")

load('./shinyDat.RData')

runApp()
