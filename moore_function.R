### takes information from a simulation (population)


moore.summary <- function(LANDSCAPE, EGG.LANDSCAPE, MOORE.RANGE){
  
  A <- LANDSCAPE
  ## egg landscape
  E <- EGG.LANDSCAPE
  
  
  index <- array(1:length(A), dim = dim(A))
  ## big table of neighborhood characteristics, and outputs (eggs)
  range.vec <- 1:MOORE.RANGE
  
  big.table <- array(dim = c(length(c(A)), 8, length(range.vec)))
  big.table[,1,] <- c(A)
  colnames(big.table) <- c('patch.type', 
                           'row.index', 
                           'col.index', 
                           'moore.empty',
                           'moore.safe',
                           'moore.risk',
                           'risk.score',
                           'n.eggs')
  
  neighborhood.totals <- array(dim = c(length(c(A)), 10, length(range.vec)))
  neighborhood.totals[,1,] <- c(A)
  colnames(neighborhood.totals) <- c('patch.type', 
                                     'row.index', 
                                     'col.index', 
                                     'total.eggs.safe',
                                     'total.eggs.risk',
                                     'n.safe.patches',
                                     'n.risk.patches',
                                     'n.empty',
                                     'mean.eggs.safe',
                                     'mean.eggs.risk')
  
  
  
  for (j in 1:length(range.vec)){
    for (i in 1:length(c(A))){
      
      ROW <- which(index == i, arr.ind = TRUE)[1]
      COL <- which(index == i, arr.ind = TRUE)[2]
      
      range <- range.vec[j]
      
      row.range <- (ROW - range):(ROW + range)
      row.range[row.range <= 0] <- row.range[row.range <= 0] - 1
      row.range[row.range > (nrow(A))] <- row.range[row.range > (nrow(A))] + 1
      
      
      col.range <- (COL - range):(COL + range)
      col.range[col.range <= 0] <- col.range[col.range <= 0] - 1
      col.range[col.range > (ncol(A))] <- col.range[col.range > (ncol(A))] + 1
      
      v.range <- row.range %% (nrow(A)+1)
      h.range <- col.range %% (ncol(A)+1)
      
      moore <- A[v.range, h.range]
      moore[range+1, range+1] <- NA
      
      moore.empty <- sum(moore == 0, na.rm = TRUE)
      moore.safe <- sum(moore == 1, na.rm = TRUE)
      moore.risk <- sum(moore == 2, na.rm = TRUE)
      
      risk.score <- moore.risk / (length(moore)-1)
    
      ######## fill in the big table
      big.table[i, 2:3, j] <- which(index == i, arr.ind=TRUE)
      big.table[i, 4:6, j] <- c(moore.empty, moore.safe, moore.risk)
      big.table[i,7,j] <- risk.score
      big.table[i, 8, j] <- E[ROW, COL]    #eggs
      
      #####
      tmp.moore <- A[v.range, h.range]
      egg.moore <- E[v.range, h.range]
      
      egg.moore.empty <- egg.moore[tmp.moore == 0]
      egg.moore.safe <- egg.moore[tmp.moore == 1]
      egg.moore.risk <- egg.moore[tmp.moore == 2]
      
      ## mean eggs
      
      
      #### fill in  neighborhood table
      neighborhood.totals[i, 2:3, j] <- which(index == i, arr.ind=TRUE)
      neighborhood.totals[i, 4, j] <- sum(egg.moore.safe, na.rm = TRUE)
      neighborhood.totals[i, 5, j] <- sum(egg.moore.risk, na.rm = TRUE)
      neighborhood.totals[i, 6, j] <- length(egg.moore.safe)
      neighborhood.totals[i, 7, j] <- length(egg.moore.risk)
      neighborhood.totals[i, 8, j] <- length(egg.moore.empty)
      neighborhood.totals[i, 9, j] <- tapply(c(egg.moore), c(tmp.moore), mean, na.rm = TRUE)["1"]
      neighborhood.totals[i, 10, j] <- tapply(c(egg.moore), c(tmp.moore), mean, na.rm = TRUE)["2"]
      
    }
  }
  
  output = list(big.table = big.table,
                neighborhood.totals = neighborhood.totals)
  
  
  return(output)
}
