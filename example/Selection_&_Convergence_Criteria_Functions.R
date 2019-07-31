#--------------------------------------------------------------------------------------
#  Functions to implement the Selection and Convergence Criterion defined in :
#    
#        "Robust combination of the Morris and Sobol methods 
#             in complex multidimensional models"
#
#  Submitted to Environmental Modelling & Software. 
#
#  Functions in this script:
#               - selection_criterion ~ arguments: AEE, K_EE, FVis
#
#
#
#
# Dorleta Garcia, Inmaculada Arostegui and Raul Prellezo
# 2019/08/01
#--------------------------------------------------------------------------------------

selection_criterion <- function(AEE, K_EE, FVis){

  # Name of output variables.
  outVar <- unique(AEE$outVar)
  
  # Set of weights to be used in the weigthed criterion.
  wgt_grid <- expand.grid(seq(0,1,0.01),seq(0,1,0.01),seq(0,1,0.01))
  wgt_grid <- wgt_grid[which(rowSums(wgt_grid)==1),]
  rownames(wgt_grid) <-  1:dim(wgt_grid)[1]

  #  A list to store the selected factors for each set of weights in wgt_grid data frame.
  factors_morris_wgt_grid <- vector('list', dim(wgt_grid)[1])
  names(factors_morris_wgt_grid) <- paste(wgt_grid[,1],wgt_grid[,2], wgt_grid[,3], sep = "_")

  # FN, FH and FD: The objects where the selected factors will be stored. 
  FN <- FH <- FD <- NULL

  # A set of values for tauN, tauD and tauH that will be used to apply the selection criterion to 
  # identify those that result in the selection of K_EE number of input factors. 
  vnumb  <- 1:20
  vjump  <- seq(0.0001,0.5, 0.001)  
  vpmax  <- seq(0.01,0.99,0.01)[length(seq(0.01,0.99,0.01)):1] 


  # ** Number criterion: Find the tauN **
  flag     <- FALSE
  nfactors <- numeric(20)
  k <- 1

  while(flag == FALSE){
    FN. <- FN
    FN <- NULL
    tauN <- vnumb[k]
    
    for(id in outVar){
  
      res0 <- subset(AEE, outVar == id)
      res0 <- res0[order(res0$AEE, decreasing = TRUE),]
      FN <- c(FN, as.character(res0[1:tauN,1]))
    }
  
    nfactors[k] <- length(unique(FN))
    if(k > 1) if(nfactors[k] > K_EE & nfactors[k-1] <= K_EE) flag <- TRUE
  
    tauN <- tauN
    nf_numb <- nfactors[k]
  
    k <- k +1
  }

  FN <- unique(FN) 


  # ** Difference criterion: Find tauD **
  k <- 1
  flag <- FALSE
  nfactors <- numeric(length(vjump))
  while(flag == FALSE){
    FD. <- FD 
    FD <- NULL
    tauD <- vjump[k]

    for(id in outVar){
      res0 <- subset(AEE, outVar == id)
      # jump
      fjump <- which(((res0$AEE[-dim(res0)[1]] - res0$AEE[-1])/max(res0$AEE)) < tauD)[1]-1
      
      if(!is.na(fjump))  FD <- c(FD,as.character(res0[1:fjump,1]))  
      else FD <- c(FD,NULL)  
    }
  
  
    nfactors[k] <- length(unique(FD))
    if(k > 1) if(nfactors[k-1] > K_EE & nfactors[k] <= K_EE) flag <- TRUE
    tauD    <- vjump[k]
    nf_jump <- nfactors[k]
    k <- k+1
  }
  FD <- unique(FD)

  
  # ** High value criterion: Find tauH **
  k <- 1
  flag <- FALSE
  nfactors <- numeric(25)

  while(flag == FALSE){
    FH. <- FH
    FH <- NULL
    tauH <- vpmax[k]

    for(id in outVar){
      res0 <- subset(AEE, outVar == id)
      # proportion of maximum value
      pos <- which(res0$AEE > res0$AEE[1]*tauH)
      FH <- c(FH,as.character(res0[pos,1]))  
    }
    
    nfactors[k] <- length(unique(FH))
    if(k > 1) if(nfactors[k] > K_EE & nfactors[k-1] <= K_EE) flag <- TRUE
    tauH    <- vpmax[k-1]
    nf_pmax<- nfactors[k-1]
    k <- k+1
  }
  FH <- unique(FH.)

  factors_morris_eye   <- NULL
  factors_morris_wgt <- NULL

  
  k <- 1

  for(id in outVar){
    res0 <- subset(AEE, outVar == id)
    
    # eye
    factors_morris_eye  <- c(factors_morris_eye, as.character(res0[1:Fvis[id],1]))
    
    pos   <- which(res0$AEE > res0$AEE[1]*tauH)
    fjump <- which(((res0$AEE[-dim(res0)[1]] - res0$AEE[-1])/max(res0$AEE)) < tauD)[1]-1
    
    # weighted mean grid.
    fwgt_grid <- round(wgt_grid[,1]*tauN + wgt_grid[,2]*length(pos) + wgt_grid[,3]*fjump)
    factors_morris_wgt_grid <- sapply(1:5151, function(x) c(factors_morris_wgt_grid[[x]],as.character(res0[1:fwgt_grid[x],1])))
    k <- k+1
  }


  factors_morris_eye  <- sort(unique(factors_morris_eye))
  factors_morris_wgt  <- sort(unique(factors_morris_wgt))

  factors_morris_wgt_grid <- lapply(factors_morris_wgt_grid, function(x) sort(unique(x)))
  sapply(factors_morris_wgt_grid, function(x) length(x))
  wgt_grid <- cbind(wgt_grid, nfac = sapply(factors_morris_wgt_grid, function(x) length(x)),
                  intersec = sapply(factors_morris_wgt_grid, function(x) length(intersect(factors_morris_eye, x))))

  # The maximum overlap with 'factors_morris_eye' with the minimum number of factors.
  #wgt_grid <- wgt_grid[wgt_grid$nfac < 68,]
  tt0 <- wgt_grid[which(wgt_grid$intersec==max(wgt_grid$intersec)) ,]

  wgt_sel <- tt0[ which(tt0$nfac == min(tt0$nfac)), ]

  wgt_sel[which.min(wgt_sel[,1]),]

  sel_wgh <- as.numeric(names(which.min(rowSums((wgt_sel[,1:3]-1/3)^2))))[1]


  factors_morris_wgt <- factors_morris_wgt_grid[[sel_wgh]]
  
  return(list(factors_wght = factors_morris_wgt, factors_vis = factors_morris_eye, weights = wgt_grid[sel_wgh,], tauN = tauN, tauH = tauH, tauD = tauD))
}
