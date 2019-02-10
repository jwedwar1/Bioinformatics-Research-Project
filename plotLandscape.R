plotLandscape <- function(landscape){
  
  A <- landscape
  
  col.vec <- c(0, rgb(0,0,1, 0.25), rgb(1,0,0, 0.25))
    
  
  par(mar = c(1,1,1,1))
  plot(1,1, cex = 0, xlim = c(1,nrow(A)), 
                     ylim=c(1,ncol(A)),
       axes = FALSE, ann = FALSE)
    points(rep(1, ncol(A)), 1:nrow(A),
         pch = 19,
         col = col.vec[A[,1]+1],
         cex =2)
    for (i in 2:nrow(A)){
      points(rep(i, ncol(A)), 1:nrow(A),
             pch = 19,
             col = col.vec[A[,i]+1],
             cex = 2)
    }
  box()
  
}
