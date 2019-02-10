setwd('C:\\Users\\James\\Documents\\Bioinformatics Research\\My Code')
source('wavelet_landscape.R')
source('population_function.R')
source('moore_function.R')


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
        
        sim <- pop.habitatselection(POP.SIZE = 100,
                                    LANDSCAPE = wavescape,
                                    RISK.MAG = 0.6,
                                    PERCEPTION = 0.1,
                                    MVT = 0.5,
                                    MVT.MOD = 0.1,
                                    MEM.DEPTH = 0,
                                    MEM.WEIGHT = 0,
                                    RANDOM.START = TRUE)
        
        
      #moore <- moore.summary(wavescape, wavescape, 2)
        
        #sim[3] is the egg landscape
        #sim[2] is the regular landscape
        #print(sim[[2]])
        
        #moore2 <- moore.summary()
        
        results[[i]] <- list1
        
        i <- i+1
      }
    }
  }
}

moore <- moore.summary(sim[[2]], sim[[3]], 2)


new_moore = moore[[1]]



graph <- vector()
graphCount <- 1

for(i in 1:nrow(new_moore)){
  #if eggCount isn't 0
  if(new_moore[[i, 8, 1]] != 0){
    graphCount <- graphCount+1
  }
}

#make empty array the size of however many points we have (graphCount)
graphResults <- vector("list", graphCount)
k <- 1

for(i in 1:nrow(new_moore)){
  if(new_moore[[i, 8, 1]] != 0){
    riskScore <- new_moore[[i, 7, 1]]
    eggNum <- new_moore[[i, 8, 1]]
    item2 <- c(riskScore, eggNum)
    graphResults[[k]] <- item2
    k <- k+1
    

  }
}

# remove the last element which is null (figure out why this is happening later)
graphResults <- graphResults[-length(graphResults)]
xList <- vector("list", length(graphResults))
yList <- vector("list", length(graphResults))
counter <- 1

for(i in 1:length(graphResults)){
  x <- (graphResults[[i]][1])
  y <- (graphResults[[i]][2])
  xList[[i]] <- x
  yList[[i]] <- y
  i <- i+1
}

plot(xList, yList, xlab = "Risk Score", ylab = "# of Eggs")
#title(main = NULL, sub = NULL, xlab = "Risk Score", ylab = "# of Eggs")






# save the landscape list data
save(results, file="landscape_list.RData")
?save


#comment order of list [1]