
# #----------------------------------------------------------------------------------------------
# # Dorleta Garcia
# # Shiny app to show PhD results
# #---------------------------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(magrittr)
library(shinyBS)
library(ggplot2)

load('./gsi.RData')
load('./morris_results.RData')
morris <- res
load('./res_morris_groups.RData')
options(shiny.sanitize.errors = FALSE)


gsi$index <- as.character(gsi$index)
gsi$index <- ifelse(gsi$index == 'diff', 'Total-First', ifelse(gsi$index == 'first-order', 'First_Order', gsi$index))
gsi$index <- as.factor(gsi$index)
gsi$index <- relevel(gsi$index, 'Total-First')
pass <- 'flbeiaGSA'

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
cols <- gg_color_hue(2)

function(input, output, session) {
  

  toggleModal(session,modalId = "modalPass",toggle = "open") # Ventana contrase~na
  
  rv <- reactiveValues(intentos=3, correcto=FALSE) # 3 intentos
  
  observe({
    if (rv$intentos==0 | rv$correcto) {
      # si se queda sin intentos o ha acertado el password el boton de verificacion se inhabilita
      updateButton(session,"idbutton",disabled=TRUE)
    }
  })
  observeEvent(rv$intentos,{ # al cambiar el numero de intentos el boton cambia de color
    if (rv$intentos==3)
      updateButton(session,"idbutton",style="primary")
    if (rv$intentos==2)
      updateButton(session,"idbutton",style="warning")
    if (rv$intentos==1)
      updateButton(session,"idbutton",style="danger")
  })  
  output$resultadoPass <- renderUI({  # resultado password
    if (input$idbutton==0) return(invisible(NULL))
    isolate({
      if (input$idpass!="" & input$idpass!=pass){ # intento fallido
        rv$intentos <<- rv$intentos-1               # le resto un intento
        return(HTML(paste("<br><br>Clave incorrecta. ",rv$intentos,"intentos restantes<br>")))
      }
      if (input$idpass=="") 
        return(HTML("<br><br>Introduce la clave<br>")) # no ha introducido el password
      if (input$idpass==pass){ # password correcto
        rv$intentos <<- 3    
        rv$correcto <<- TRUE
        return("Correct")
      }
    })  
  }) # Resultado password
  observe({
    if(rv$correcto==TRUE)
      toggleModal(session,modalId = "modalPass",toggle = "close")
  })
  

  
  #---- fltData ----------------------------------------------------------------------------
  fltData <- reactive({
    data <- subset(gsi, unit %in% c('DTS_SP', 'HOK_SP', 'DFN_SP'))
    
       if(input$facet_col2 == 'year' || input$facet_row2 == 'year') yrsub <- input$year2a
    else yrsub <- input$year2b
    
    if(input$facet_col2 == 'indicator' || input$facet_row2 == 'indicator') indsub <- input$indicator2a
    else indsub <- input$indicator2b
    
    if(input$facet_col2 == 'unit' || input$facet_row2 == 'unit') unitsub <- input$unit2a
    else unitsub <- input$unit2b    
    
    dat0 <- subset(data, index == 'total')
    dat1 <- subset(data, index == 'First_Order')
    dat2 <- subset(data, index == 'Total-First')
    
    dat0$.r <- NA
    
    k <- 1

    for(rr in unique(dat0$year)){
      for(cc in unique(dat0$unit)){
        for(id in unique(dat0$indicator)){
          
          if(rr == 2014 & id == 'TAC') next
          
          ord. <- order(dat0[dat0[,'year'] == rr & dat0[,'unit'] == cc & dat0[,'indicator'] == id, 'median'])
          dat0[dat0[,'year'] == rr & dat0[,'unit'] == cc & dat0[,'indicator'] == id, ][ord.,'.r'] <- (56*k-55):(56*k)
          k <- k+1
        }}}
    
    dat0$.r <- factor(dat0$.r)
    dat1$.r <- dat0$.r
    dat2$.r <- dat0$.r
    
    data <- rbind(dat0,dat1,dat2)
    
    data <- subset(data, data$.r %in% c(sapply(1:(length(unique(data$unit))*length(unique(data$indicator))*length(unique(data$year))),
                                               function(x) (x*56-(input$facNumb2-1)):(x*56))))
    
    data <- subset(data, year %in% yrsub & indicator %in% indsub & unit %in% unitsub)

    
    
    data
    })

  
  
#---- stkData ----------------------------------------------------------------------------
  stkData <- reactive({
    data <- subset(gsi, unit %in% c('HKE','MEG','MON','LDB','HOM'))
    
    if(input$facet_col1 == 'year' || input$facet_row1 == 'year') yrsub <- input$year1a
    else yrsub <- input$year1b
    
    if(input$facet_col1 == 'indicator' || input$facet_row1 == 'indicator') indsub <- input$indicator1a
    else indsub <- input$indicator1b
    
    if(input$facet_col1 == 'unit' || input$facet_row1 == 'unit') unitsub <- input$unit1a
    else unitsub <- input$unit1b    

    dat0 <- subset(data, index == 'total')
    dat1 <- subset(data, index == 'First_Order')
    dat2 <- subset(data, index == 'Total-First')
    
    dat0$.r <- NA
    
    k <- 1
    for(rr in unique(dat0$year)){
      for(cc in unique(dat0$unit)){
        for(id in unique(dat0$indicator)){
        
          if(rr %in% 2013:2016 & id == 'TAC') next
          
          ord. <- order(dat0[dat0[,'year'] == rr & dat0[,'unit'] == cc & dat0[,'indicator'] == id, 'median'])
          dat0[dat0[,'year'] == rr & dat0[,'unit'] == cc & dat0[,'indicator'] == id, ][ord.,'.r'] <- (56*k-55):(56*k)
         k <- k+1
        }}}
    
    
    dat0$.r <- factor(dat0$.r)
    dat1$.r <- dat0$.r
    dat2$.r <- dat0$.r
    
    data <- rbind(dat0,dat1,dat2)
    
    data <- subset(data, data$.r %in% c(sapply(1:(length(unique(data$unit))*length(unique(data$indicator))*length(unique(data$year))),
                                            function(x) (x*56-(input$facNumb1-1)):(x*56))))
    
    data <- subset(data, year %in% yrsub & indicator %in% indsub & unit %in% unitsub)
    
    
    data
    
    })
  

  
#---- stkDataMorris ----------------------------------------------------------------------------
  stkDataMorris <- reactive({
    data <- subset(morris, unit %in% c('HKE','MEG','MON','LDB','HOM'))
    
    if(input$facet_col1_morris == 'year' || input$facet_row1_morris == 'year') yrsub <- input$year1a_morris
    else yrsub <- input$year1b_morris
    
    if(input$facet_col1_morris == 'indicator' || input$facet_row1_morris == 'indicator') indsub <- input$indicator1a_morris
    else indsub <- input$indicator1b_morris
    
    if(input$facet_col1_morris == 'unit' || input$facet_row1_morris == 'unit') unitsub <- input$unit1a_morris
    else unitsub <- input$unit1b_morris   
    
    dat0 <- data
    
    dat0$.r <- NA
    
    dat0 <- subset(dat0, stat == 'mean')
    
    k <- 1

      for(cc in unique(dat0$unit)){
        for(id in unique(dat0$indicator)){
          
          ord. <- order(dat0[dat0[,'unit'] == cc & dat0[,'indicator'] == id, 'value'])
          dat0[dat0[,'unit'] == cc & dat0[,'indicator'] == id, ][ord.,'.r'] <- (135*k-134):(135*k)
          k <- k+1
        }}
    
    dat0$.r <- factor(dat0$.r)
    
    dat0 <- subset(dat0, indicator %in% indsub & unit %in% unitsub)

    dat0
    
  })
  

  
  
  #---- fltDataMorris ----------------------------------------------------------------------------
  fltDataMorris <- reactive({
    data <- subset(morris, unit %in% c('DTS_SP', 'DFN_SP', 'HOK_SP'))
    
    if(input$facet_col2_morris == 'year' || input$facet_row2_morris == 'year') yrsub <- input$year2a_morris
    else yrsub <- input$year2b_morris
    
    if(input$facet_col2_morris == 'indicator' || input$facet_row2_morris == 'indicator') indsub <- input$indicator2a_morris
    else indsub <- input$indicator2b_morris
    
    if(input$facet_col2_morris == 'unit' || input$facet_row2_morris == 'unit') unitsub <- input$unit2a_morris
    else unitsub <- input$unit2b_morris   
    
    dat0 <- data
    
    dat0$.r <- NA
    
    dat0 <- subset(dat0, stat == 'mean')
    
    k <- 1
    
    for(cc in unique(dat0$unit)){
      for(id in unique(dat0$indicator)){
        
        ord. <- order(dat0[dat0[,'unit'] == cc & dat0[,'indicator'] == id, 'value'])
        dat0[dat0[,'unit'] == cc & dat0[,'indicator'] == id, ][ord.,'.r'] <- (135*k-134):(135*k)
        k <- k+1
      }}
    
    dat0$.r <- factor(dat0$.r)
    
    dat0 <- subset(dat0, indicator %in% indsub & unit %in% unitsub)
    
    dat0
    
  })
  

#---- plot1_Morris ----------------------------------------------------------------------------
  
  Plot1Height_morris <- reactive({
    
    fr <- input$facet_row1_morris
    
    mult <- ifelse(fr == '.',1,length(unique(stkDataMorris()[,fr])))
    
    return(20*length(unique(stkDataMorris()$param))*mult*input$facNumb1_morris/135)})
  
  observe({
    if (rv$correcto) {
  
  output$plot1_morris <- renderPlot({
    
    dat <- stkDataMorris()
    
    facet_row1 <- ifelse(input$facet_row1_morris == '.', NULL, input$facet_row1_morris)
    facet_col1 <- ifelse(input$facet_col1_morris == '.', NULL, input$facet_col1_morris)
    
    facets <- c(facet_row1, facet_col1)
    
    ord <- ifelse(!is.null(facet_row1), facet_row1, facet_col1)
    
    dat <- subset(dat, stat == 'mean')
    
    nunit <- length(unique(dat$unit)) 
    nind  <- length(unique(dat$indicator)) 
    
    lines <- data.frame(line = rep(c('pmax', 'jump', 'numb', 'wgt', 'eye'), nunit*nind), 
                        unit = rep(unique(dat$unit), each = nind*5), 
                        indicator = rep(rep(unique(dat$indicator), each = 5),nunit), 
                        value = NA)
    
    numb  <- 6     # For 68: 7  
    pmax  <- .38  # For 68:.305 
    pjump <- 0.0047 # For 68:0.003   
    
    ##  By eye  ##
    feye <- c(5,5,9,6,6,
              6,7,5,6,7,
              5,6,4,2,7,
              8,6,5,7,5,
              8,5,14,3,5)
    
    p1 <- 0.53
    p2 <- 0.35
    p3 <- 0.11
    
  
    k <- 1
    j <- 1
    for(st in unique(dat$unit)){
      for(id in unique(dat$indicator)){
        
      #  print(st)
      #  print(id)
        res0    <- subset(dat, unit == st & indicator == id)
        
        pm      <- res0$value[1]*pmax
        fjump   <- which(((res0$value[-dim(res0)[1]] - res0$value[-1])/max(res0$value)) < pjump)[1]-1
        fjump <- ifelse(fjump == 0,1,fjump)
        pos     <- which(res0$value > pm)
        fwgt    <- round(p1*numb + p2*length(pos) + p3*fjump)
        
        lines[k,'value']   <- pm# pos[1]
     
     #   print(res0$value[fjump])
        lines[k+1,'value'] <- res0$value[fjump]

        
        lines[k+2,'value'] <- res0$value[numb]
       
        lines[k+3,'value'] <- res0$value[fwgt]
        lines[k+4,'value'] <- res0$value[feye[j]]
        j <- j+1
        k <- k+5
        
      }
    }


    dat <- subset(dat, dat$.r %in% c(sapply(1:25,function(x) (x*135-(input$facNumb1_morris-1)):(x*135))))
    p <- ggplot(dat,aes(x = .r, y = value))  + coord_flip() + geom_col(position =  'dodge') +
         facet_wrap(eval(facets), scales = 'free', drop= TRUE, ncol = length(unique(dat[,facet_col1])))  +
       geom_hline(yintercept = 0.0)+
       scale_x_discrete(breaks = dat$.r, labels = dat$name)#, limits = c(100, 135)) 

    
    if(input$pmax1 == TRUE){ 
      l0 <- subset(lines,line == 'pmax' )
      p <- p + geom_hline(data = l0, aes(yintercept=value), col = 2, lwd = 2) 
    }
    if(input$numb1 == TRUE){ 
      l0 <- subset(lines,line == 'numb' )
      p <- p + geom_hline(data = l0, aes(yintercept=value), col = 3, lwd = 2)  
    }
    if(input$jump1 == TRUE){ 
      l0 <- subset(lines,line == 'jump' )
      p <- p + geom_hline(data = l0, aes(yintercept=value), col = 4, lwd = 2)  
    }
    if(input$wgt1 == TRUE){ 
      l0 <- subset(lines,line == 'wgt' )
      p <- p + geom_hline(data = l0, aes(yintercept=value), col = 5, lwd = 2)  
    }
    if(input$eye1 == TRUE){ 
      l0 <- subset(lines,line == 'eye' )
      p <- p + geom_hline(data = l0, aes(yintercept=value), col = 6, lwd = 2, lty = 2)  
    }
 #   print(p)  
    
    
  }, width = 1000, height = Plot1Height_morris)
}})


  #---- plot2_Morris ----------------------------------------------------------------------------
  
  Plot2Height_morris <- reactive({
    
    fr <- input$facet_row2_morris
    
    mult <- ifelse(fr == '.',1,length(unique(fltDataMorris()[,fr])))
    
    return(20*length(unique(fltDataMorris()$param))*mult*input$facNumb2_morris/135)})
  

  observe({
    if (rv$correcto) {
  output$plot2_morris <- renderPlot({
    
    dat <- fltDataMorris()
    
    facet_row2 <- ifelse(input$facet_row2_morris == '.', NULL, input$facet_row2_morris)
    facet_col2 <- ifelse(input$facet_col2_morris == '.', NULL, input$facet_col2_morris)
    
    facets <- c(facet_row2, facet_col2)
    
    ord <- ifelse(!is.null(facet_row2), facet_row2, facet_col2)
    
    dat <- subset(dat, stat == 'mean')
    
    nunit <- length(unique(dat$unit)) 
    nind  <- length(unique(dat$indicator)) 
    
    lines <- data.frame(line = rep(c('pmax', 'jump', 'numb', 'wgt', 'eye'), nunit*nind), 
                        unit = rep(unique(dat$unit), each = nind*5), 
                        indicator = rep(rep(unique(dat$indicator), each = 5),nunit), 
                        value = NA)

    numb  <- 4     # For 68: 7  
    pmax  <- .67  # For 68:.305 
    pjump <- 0.0088 # For 68:0.003   
    
    ##  By eye  ##
    feye <- c(12,10,11,5,
              8,8,7,5,
              8,8,8,4)
    
    p1 <- 0.53
    p2 <- 0.35
    p3 <- 0.11
    
    k <- 1
    j <- 1
    for(st in unique(dat$unit)){
      for(id in unique(dat$indicator)){
        
        res0    <- subset(dat, unit == st & indicator == id)
        
        pm      <- res0$value[1]*pmax
        fjump   <- which(((res0$value[-dim(res0)[1]] - res0$value[-1])/max(res0$value)) < pjump)[1]-1
        fjump   <- ifelse(fjump == 0,1,fjump)
        pos     <- which(res0$value > pm)
        fwgt    <- round(p1*numb + p2*length(pos) + p3*fjump)

        lines[k,'value']   <- pm# pos[1]
        lines[k+1,'value'] <- res0$value[fjump]
        lines[k+2,'value'] <- res0$value[numb]
        lines[k+3,'value'] <- res0$value[fwgt]
        lines[k+4,'value'] <- res0$value[feye[j]]
        j <- j+1
        k <- k+5
      }
    }
    
    dat <- subset(dat, dat$.r %in% c(sapply(1:25,function(x) (x*135-(input$facNumb2_morris-1)):(x*135))))
    p <- ggplot(dat,aes(x = .r, y = value))  + coord_flip() + geom_col(position =  'dodge') +
      facet_wrap(eval(facets), scales = 'free', drop= TRUE, ncol = length(unique(dat[,facet_col2])))  +
      geom_hline(yintercept = 0.0)+
      scale_x_discrete(breaks = dat$.r, labels = dat$name)#, limits = c(100, 135)) 
   
    
    if(input$pmax2 == TRUE){ 
      l0 <- subset(lines,line == 'pmax' )
      p <- p + geom_hline(data = l0, aes(yintercept=value), col = 2, lwd = 2) 
    }
    if(input$numb2 == TRUE){ 
      l0 <- subset(lines,line == 'numb' )
      p <- p + geom_hline(data = l0, aes(yintercept=value), col = 3, lwd = 2)  
    }
    if(input$jump2 == TRUE){ 
      l0 <- subset(lines,line == 'jump' )
      p <- p + geom_hline(data = l0, aes(yintercept=value), col = 4, lwd = 2)  
    }
    if(input$wgt2 == TRUE){ 
      l0 <- subset(lines,line == 'wgt' )
      p <- p + geom_hline(data = l0, aes(yintercept=value), col = 5, lwd = 2)  
    }
    if(input$eye2 == TRUE){ 
      l0 <- subset(lines,line == 'eye' )
      p <- p + geom_hline(data = l0, aes(yintercept=value), col = 6, lwd = 2, lty = 2)  
    }
   
     print(p)  
    
    
    
  }, width = 1000, height = Plot2Height_morris)
  
    }})
#---- Heigth parameters for GSA plots  ----------------------------------------------------------------------------
  
  Plot1Height <- reactive({
   
                        fr <- input$facet_row1
                     
                        mult <- ifelse(fr == '.',1,length(unique(stkData()[,fr])))*(0.2+input$facNumb1*(0.8/56))
                      
                        return(20*length(unique(stkData()$param))*mult)})
                                        
  Plot2Height <- reactive({
    fr <- input$facet_row2
    mult <- ifelse(fr == '.',1,length(unique(fltData()[,fr])))*(0.3+input$facNumb2*(0.7/56))
  
    return(20*length(unique(fltData()$param))*mult)})
  

#---- plot1 for GSA  ----------------------------------------------------------------------------

  observe({
    if (rv$correcto) {
  output$plot1 <- renderPlot({

    dat <- stkData()
    
    facet_row1 <- ifelse(input$facet_row1 == '.', NULL, input$facet_row1)
    facet_col1 <- ifelse(input$facet_col1 == '.', NULL, input$facet_col1)
    
    facets <- c(facet_row1, facet_col1)
    
    ord <- ifelse(!is.null(facet_row1), facet_row1, facet_col1)
   
    if(input$stacked1 == FALSE){
      dat <- subset(dat, index != 'Total-First')
      p <- ggplot(dat,aes(x = .r, y = median, fill = index), colour = cols[1:2])  + coord_flip() + geom_col(position =  'dodge')
    }
    else{
      dat <- subset(dat, index != 'total')
      p <- ggplot(dat,aes(x = .r, y = median, fill = index), colour = cols[1:2])  + coord_flip() + geom_col() +
        scale_fill_manual("legend", values = c(First_Order = cols[1], "Total-First" = cols[2]))
    }

    p <- p + facet_wrap(eval(facets), scales = 'free_y', drop= TRUE, ncol = length(unique(dat[,facet_col1])))  +
         ylim(NA,1.2) + geom_hline(yintercept = 0.0)+
         geom_hline(yintercept = 0.05,lty = 2, colour = 'darkgrey') +
         scale_x_discrete(breaks = dat$.r, labels = dat$name)

    if(input$confInt1 == TRUE & input$stacked1 == FALSE)
      
    p <- p +  geom_errorbar(aes(ymin=q05, ymax=q95), width=.2, position=position_dodge(.9))
    
    print(p)  
    
  }, width = 1000, height = Plot1Height)
    }})
  
#---- plot2 for GSA ----------------------------------------------------------------------------
  observe({
    if( rv$correcto == TRUE) {
  output$plot2 <- renderPlot({
    
    dat <- fltData()
    
    facet_row2 <- ifelse(input$facet_row2 == '.', NULL, input$facet_row2)
    facet_col2 <- ifelse(input$facet_col2 == '.', NULL, input$facet_col2)
    
    facets <- c(facet_row2, facet_col2)
    
    ord <- ifelse(!is.null(facet_row2), facet_row2, facet_col2)
    
    if(input$stacked2 == FALSE){
      dat <- subset(dat, index != 'Total-First')
      p <- ggplot(dat,aes(x = .r, y = median, fill = index), colour = cols[1:2])  + coord_flip() + geom_col(position =  'dodge')
    }
    else{
      dat <- subset(dat, index != 'total')
      p <- ggplot(dat,aes(x = .r, y = median, fill = index), colour = cols[1:2])  + coord_flip() + geom_col() +
        scale_fill_manual("legend", values = c(First_Order = cols[1], "Total-First" = cols[2]))
    }
    
    tt <- subset(dat, indicator == 'nVessels'  & unit == 'HOK_SP' & name == 'MaxDays1' )
    print(tt)

    
    p <- p + facet_wrap(eval(facets), scales = 'free_y', drop= TRUE, ncol = length(unique(dat[,facet_col2])))  +
      ylim(NA,1.2) + geom_hline(yintercept = 0.0)+
      geom_hline(yintercept = 0.05,lty = 2, col = 'darkgrey') +
      scale_x_discrete(breaks = dat$.r, labels = dat$name)
    
    if(input$confInt2 == TRUE & input$stacked2 == FALSE)
      
      p <- p +  geom_errorbar(aes(ymin=q05, ymax=q95), width=.2, position=position_dodge(.9))
    
    print(p)  
    
    
  }, width = 1000, height = Plot2Height)}}) # close observe
  
}


 