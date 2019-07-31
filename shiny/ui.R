#----------------------------------------------------------------------------------------------
# Dorleta Garcia
# Shiny app to show PhD results
#---------------------------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(magrittr)
library(shinyBS)

load('./gsi.RData')
load('./morris_results.RData')
morris <- res
load('./res_morris_groups.RData')

gsi$index <- as.character(gsi$index)
gsi$index <- ifelse(gsi$index == 'diff', 'Total-First', ifelse(gsi$index == 'first-order', 'First_Order', gsi$index))
gsi$index <- as.factor(gsi$index)
gsi$index <- relevel(gsi$index, 'Total-First')

options(shiny.sanitize.errors = TRUE)


# Page ui.R  
ui = fluidPage(#theme = "lumen.css",
              title = "GSA-FLBEIA",
#  titlePanel(includeHTML("data/title.txt"), windowTitle = includeHTML("data/shorttitle.txt")),
#  windowTitle="Mi aplicacion",navbarPage("GSA-FLBEIA", header = 'Global Sensitivity Analysis of a FLBEIA Implementation',
    
    
                # .................... #
                # Tab "Login "         #
                # .................... #  
                # Window of the password
                bsModal("modalPass", "Enter your password", "showResults",
                        passwordInput("idpass","Enter your password",""),
                        bsButton("idbutton", "Go"),
                        uiOutput("resultadoPass")),
                
                navbarPage(    

#-------------------------------------------------------------------
# INTRODUCTION 
#-------------------------------------------------------------------
   navbarMenu("GSA of a Fisheries Management model"),
#                 
# #-------------------------------------------------------------------
# # FLBEIA 
# #-------------------------------------------------------------------
# navbarMenu("FLBEIA"),
# 
# #-------------------------------------------------------------------
# # CONDITIONING 
# #-------------------------------------------------------------------
#   navbarMenu("Conditioning",
#                 tabPanel("Stocks"),
#                 tabPanel("Fleets"),
#                 tabPanel("Observation Errors")),
#                 
                
#-------------------------------------------------------------------
# MORRIS
#-------------------------------------------------------------------             
                
    navbarMenu("Morris Elementary Effects method",

  #---- Stocks ----------------------------------------------------------------------------------------
     tabPanel("Stocks",
         sidebarPanel(
             # Facet in rows
             selectInput('facet_row1_morris', 'Facet in Rows',c('indicator', 'unit'),'unit'),
             # Facet in columns
             selectInput('facet_col1_morris', 'Facet in Columns',c('indicator', 'unit'),'indicator'),
  
             conditionalPanel(condition = "input.facet_col1_morris == 'indicator' || input.facet_row1_morris  == 'indicator'",
                  selectInput('indicator1a_morris', 'Result Indicators', c('catch','f','rec','ssb', 'tac'), c('catch','f','rec','ssb', 'tac'), multiple = TRUE)),
  
             conditionalPanel(condition =  "input.facet_col1_morris != 'indicator' && input.facet_row1_morris  != 'indicator'",
                  selectInput('indicator1b_morris', 'Result Indicators', c('catch','f','rec','ssb', 'tac'), 'ssb', multiple = FALSE)),
  
             conditionalPanel(condition = "input.facet_col1_morris == 'unit' || input.facet_row1_morris == 'unit'",
                  selectInput('unit1a_morris', 'Stock', c('HKE','MEG','MON','LDB','HOM'), c('HKE','MEG','MON','LDB','HOM'),multiple = TRUE)),
  
             conditionalPanel(condition = "input.facet_col1_morris != 'unit' && input.facet_row1_morris != 'unit'",
                 selectInput('unit1b_morris', 'Stocks', c('HKE','MEG','MON','LDB','HOM'), 'HKE', multiple = FALSE)),
  
             numericInput('facNumb1_morris', "Number of factors", 50, min = 1, max = 145, step = 1, width = NULL),
  
             checkboxInput("pmax1", h5("Factors with high AEE value criterion"), FALSE),
  
             checkboxInput("numb1", h5("Fixed-Number of factors criterion"), FALSE),
  
             checkboxInput("jump1", h5("Factors distinguished from the others criterion"), FALSE),
  
             checkboxInput("wgt1",  h5("Selection criterion (weighted mean)"), FALSE),
         
             checkboxInput("eye1",  h5("Visual criterion"), FALSE)),
  
     mainPanel(plotOutput('plot1_morris'))),


#---- Fleets ----------------------------------------------------------------------------------------
    tabPanel("Fleets",
         sidebarPanel(
           # Facet in rows
           selectInput('facet_row2_morris', 'Facet in Rows',c('indicator', 'unit'),'unit'),
           # Facet in columns
           selectInput('facet_col2_morris', 'Facet in Columns',c('indicator', 'unit'),'indicator'),
           
           conditionalPanel(condition = "input.facet_col2_morris == 'indicator' || input.facet_row2_morris  == 'indicator'",
                            selectInput('indicator2a_morris', 'Result Indicators', c('effort', 'profits', 'gva', 'nVessels'), c('effort', 'profits', 'gva', 'nVessels'), multiple = TRUE)),
           
           conditionalPanel(condition =  "input.facet_col2_morris != 'indicator' && input.facet_row2_morris  != 'indicator'",
                            selectInput('indicator2b_morris', 'Result Indicators', c('effort', 'profits', 'gva', 'nVessels'), 'effort', multiple = FALSE)),
           
           conditionalPanel(condition = "input.facet_col2_morris == 'unit' || input.facet_row2_morris == 'unit'",
                            selectInput('unit2a_morris', 'Stock', c('DTS_SP', 'DFN_SP', 'HOK_SP'), c('DTS_SP', 'DFN_SP', 'HOK_SP'),multiple = TRUE)),
           
           conditionalPanel(condition = "input.facet_col2_morris != 'unit' && input.facet_row2_morris != 'unit'",
                            selectInput('unit2b_morris', 'Stocks', c('DTS_SP', 'DFN_SP', 'HOK_SP'), 'DTS_SP', multiple = FALSE)),
           
           numericInput('facNumb2_morris', "Number of factors", 50, min = 1, max = 145, step = 1, width = NULL),
           
           checkboxInput("pmax2", h5("Factors with high AEE value criterion"), FALSE),
           
           checkboxInput("numb2", h5("Fixed-Number of factors criterion"), FALSE),
           
           checkboxInput("jump2", h5("Factors distinguished from the others criterion"), FALSE),
           
           checkboxInput("wgt2",  h5("Selection criterion (weighted mean)"), FALSE),
           
           checkboxInput("eye2",  h5("Visual criterion"), FALSE)),
         
         mainPanel(plotOutput('plot2_morris')))),
                
                
#-------------------------------------------------------------------
# VARIANCE DECOMPOSITION 
#-------------------------------------------------------------------
                
navbarMenu("Sobol Variance Decomposition method",
    
    #---- Stocks ----------------------------------------------------------------------------------------  
           
    tabPanel("Stocks", sidebarPanel(
                                      
        selectInput('facet_row1', 'Facet in Rows',c('indicator', 'unit', 'year', '.'),'indicator'),
        
        selectInput('facet_col1', 'Facet in Columns',c('indicator', 'unit', 'year', '.'),'unit'),
        
        conditionalPanel(condition = "input.facet_col1 == 'indicator' || input.facet_row1 == 'indicator'",
                         selectInput('indicator1a', 'Result Indicators',c( 'Catch', 'F', 'Recruitment', 'SSB', 'TAC'), c( 'Catch', 'F', 'Recruitment', 'SSB', 'TAC'), multiple = TRUE)),
        
        conditionalPanel(condition =  "input.facet_col1 != 'indicator' && input.facet_row1 != 'indicator'",
                         selectInput('indicator1b', 'Result Indicators', c( 'Catch', 'F', 'Recruitment', 'SSB', 'TAC'), 'SSB', multiple = FALSE)),
        
        conditionalPanel(condition = "input.facet_col1 == 'unit' || input.facet_row1 == 'unit'",
                         selectInput('unit1a', 'Stock', c('HKE','MEG','MON','LDB','HOM'), c('HKE','MEG','MON','LDB','HOM'),multiple = TRUE)),
        
        conditionalPanel(condition = "input.facet_col1 != 'unit' && input.facet_row1 != 'unit'",
                         selectInput('unit1b', 'Stocks', c('HKE','MEG','MON','LDB','HOM'), 'HKE', multiple = FALSE)),
                                      
        conditionalPanel(condition = "input.facet_col1 == 'year' || input.facet_row1 == 'year' ",
            selectInput('year1a', 'Year', unique(gsi$year), unique(gsi$year), multiple = TRUE)),
        
        conditionalPanel(condition =  "input.facet_col1 != 'year' && input.facet_row1 != 'year'",
            selectInput('year1b', 'Year', unique(gsi$year), c('2020'), multiple = FALSE)),                 
                                      
      #  selectInput('G11', 'Set of factors', unique(gsi$G1),  unique(gsi$G1),multiple = TRUE),
        
        numericInput('facNumb1', "Number of factors", 56, min = 1, max = 56, step = 1, width = NULL),
        
        checkboxInput("stacked1", h5("Stacked"), FALSE),
        
        checkboxInput("confInt1", h5("Confidence Intervals"), FALSE)),
    
    mainPanel(plotOutput('plot1'))),
    
    
    #---- Fleets ----------------------------------------------------------------------------------------  
    
    tabPanel("Fleets", sidebarPanel(
    
        selectInput('facet_row2', 'Facet in Rows',c('indicator', 'unit', 'year', '.'),'indicator'),
        
        selectInput('facet_col2', 'Facet in Columns',c('indicator', 'unit', 'year', '.'),'unit'),
                                      
        conditionalPanel(condition = "input.facet_col2 == 'year' || input.facet_row2 == 'year' ",
                         selectInput('year2a', 'Year', unique(gsi$year), unique(gsi$year), multiple = TRUE)),
        conditionalPanel(condition =  "input.facet_col2 != 'year' && input.facet_row2 != 'year'",
                         selectInput('year2b', 'Year', unique(gsi$year), c('2020'), multiple = FALSE)),                
                                      
        conditionalPanel(condition = "input.facet_col2 == 'indicator' || input.facet_row2 == 'indicator'",
            selectInput('indicator2a', 'Result Indicators', c('Effort', 'GVA', 'nVessels', 'Profits'), c( 'Effort', 'GVA', 'nVessels', 'Profits'), multiple = TRUE)),
        
        conditionalPanel(condition =  "input.facet_col2 != 'indicator' && input.facet_row2 != 'indicator'",
            selectInput('indicator2b', 'Result Indicators', c( 'Effort', 'GVA', 'nVessels', 'Profits'), 'Profits', multiple = FALSE)),
                                      
        conditionalPanel(condition = "input.facet_col2 == 'unit' || input.facet_row2 == 'unit'",
            selectInput('unit2a', 'Fleets', c('DFN_SP', 'DTS_SP', 'HOK_SP'), c('DFN_SP', 'DTS_SP', 'HOK_SP'),multiple = TRUE)),
        
        conditionalPanel(condition = "input.facet_col2 != 'unit' && input.facet_row2 != 'unit'",
            selectInput('unit2b', 'Fleets', c('DFN_SP', 'DTS_SP', 'HOK_SP'), 'DFN_SP', multiple = FALSE)),
                                      
      #  selectInput('G12', 'Set of factors', unique(gsi$G1),unique(gsi$G1),multiple = TRUE),
        
        numericInput('facNumb2', "Number of factors", 56, min = 1, max = 56, step = 1, width = NULL),
        
        checkboxInput("stacked2", h5("Stacked"), FALSE),
        
        checkboxInput("confInt2", h5("Confidence Intervals"), FALSE)),
    
    mainPanel(plotOutput('plot2'))))
))