env.vec <- c(1,2,3,4)
frag.vec <- c(1,2,3,4)
prop.vec <- c(0,0.2,0.4,0.6)

#number of copies of landscapes with each parameter combination
repetitions <- as.integer(10)

list_size <- length(env.vec) * length(frag.vec) * length(prop.vec) * repetitions
#results <- vector()
results <- vector("list", list_size)
i <- 1
for(a in 1:length(env.vec)){
  for (b in 1:length(frag.vec)){
    for (c in 1:length(prop.vec)){
      for(d in 1:repetitions){
        wavescape <- generateWavelet_landscape(LANDSCAPE.SIZE = 64,
                                               ENV = env.vec[a],
                                               FRAG = frag.vec[b],
                                               PROP.MATRIX = prop.vec[c],
                                               RISK.QUANTILE = 0.6)
        #print(wavescape)
        landscape_item <- c(env.vec[a],frag.vec[b],prop.vec[c],d)
        list1 <- list(landscape_item, wavescape)
        results[[i]] <- list1
        
        i <- i+1
      }
    }
  }
}
