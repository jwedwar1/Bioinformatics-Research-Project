
setwd('C:\\Users\\James\\Documents\\Bioinformatics Research\\My Code')
source('wavelet_landscape.R')
source('population_function.R')
source('moore_function.R')


env.vec <- c(1,2,3,4)
frag.vec <- c(1,2,3,4)
prop.vec <- c(0,0.2,0.4,0.6)

#number of copies of landscapes with each parameter combination
#repetitions <- as.integer(10)
repetitions <- as.integer(2)

list_size <- length(env.vec) * length(frag.vec) * length(prop.vec) * repetitions
#results <- vector()
results <- vector("list", list_size)
two_slopes <- vector("list", list_size)
three_slopes <- vector("list", list_size)
i <- 1

# for each of the factors, generate a landscape, do a population function, moore function, plot number of eggs vs. risk, and get slope of the linear regression
for(a in 1:length(env.vec)){
  for (b in 1:length(frag.vec)){
    for (c in 1:length(prop.vec)){
      for(d in 1:repetitions){
        #generate a landscape with the environmental factors
        wavescape <- generateWavelet_landscape(LANDSCAPE.SIZE = 64,
                                               ENV = env.vec[a],
                                               FRAG = frag.vec[b],
                                               PROP.MATRIX = prop.vec[c],
                                               RISK.QUANTILE = 0.6)
        
        # put the factors together in a vector
        landscape_item <- c(env.vec[a],frag.vec[b],prop.vec[c],d)
        
        # run the population function with the landscape we generated
        sim <- pop.habitatselection(POP.SIZE = 100,
                                    LANDSCAPE = wavescape,
                                    RISK.MAG = 0.6,
                                    PERCEPTION = 0.1,
                                    MVT = 0.5,
                                    MVT.MOD = 0.1,
                                    MEM.DEPTH = 0,
                                    MEM.WEIGHT = 0,
                                    RANDOM.START = TRUE)
        
        
        
        # run the moore function with the landscape and egg landscape of the population function simulation
        #sim[2] is the regular landscape
        #sim[3] is the egg landscape
        moore <- moore.summary(sim[[2]], sim[[3]], 3)
      
        
        # run another moore on the same simulation but with a bigger neighborhood - start with 3
        # make another list to hold the results or just add to the 3d array
        # this will be a parallel thing - the same individuals are running around laying eggs, we're just looking at bigger neighborhoods
        # possibly the moore function also calculates for all smaller neighborhoods, so may not have to run twice
        # size 3 may also have size 2 data
        
        # get big.table[3] - add risky and safe together from moore table to make sure you're getting the right size
        # compare histograms of all_slopes for different sized moore neighborhoods
        # 
        
        # get the table containing risk score and number of eggs
        new_moore = moore[[1]]
        
        graph <- vector()
        graphCount2 <- 1
        graphCount3 <- 1
        
        # go through each habitat patch to get the size of habitable areas (number of points on the plot)
        for(j in 1:nrow(new_moore)){
          #if landscape isn't 0 (uninhabitable)
          # moore range of 2
          if(new_moore[[j, 1, 2]] != 0){
            graphCount2 <- graphCount2+1
          }
          # moore range of 3
          if(new_moore[[j, 1, 3]] != 0){
            graphCount3 <- graphCount3+1
          }
        }
        
        #make empty array the size of however many points we have (graphCount)
        graphResults2 <- vector("list", graphCount2)
        graphResults3 <- vector("list", graphCount3)
        k <- 1
        kk <-1
        
        # go through each habitat patch again, adding the risk score and vector for each patch to a list
        for(l in 1:nrow(new_moore)){
          #if landscape isn't 0 (uninhabitable)
          if(new_moore[[l, 1, 2]] != 0){
            # the risk score is the 7th column
            riskScore <- new_moore[[l, 7, 2]]
            # the number of eggs in the neighborhood is the 8th column
            eggNum <- new_moore[[l, 8, 2]]
            # make the risk score and number of eggs into a vector and add it to the graphResults list
            item2 <- c(riskScore, eggNum)
            graphResults2[[k]] <- item2
            k <- k+1
          }
          if(new_moore[[l, 1, 3]] != 0){
            # the risk score is the 7th column
            riskScore <- new_moore[[l, 7, 3]]
            # the number of eggs in the neighborhood is the 8th column
            eggNum <- new_moore[[l, 8, 3]]
            # make the risk score and number of eggs into a vector and add it to the graphResults list
            item3 <- c(riskScore, eggNum)
            graphResults3[[kk]] <- item3
            kk <- kk+1
          }
        }
        
        # remove the last element which is null (figure out why this is happening later)
        graphResults2 <- graphResults2[-length(graphResults2)]
        graphResults3 <- graphResults3[-length(graphResults3)]
        
        # make empty vectors to hold the x and y coordinates of the graph
        xList2 <- vector("list", length(graphResults2))
        yList2 <- vector("list", length(graphResults2))
        
        xList3 <- vector("list", length(graphResults3))
        yList3 <- vector("list", length(graphResults3))
        counter <- 1
        
        # add the x and y coordinates from graphResults into separate x and y lists
        # in the future, this step can probably be done immediately, and graphResults can just be gotten rid of
        # this should save some memory and make the script a little faster
        # make sure all of this works and is correct first though
        for(m in 1:length(graphResults2)){
          x <- (graphResults2[[m]][1])
          y <- (graphResults2[[m]][2])
          xList2[[m]] <- x
          yList2[[m]] <- y
          m <- m+1
        }
        for(mm in 1:length(graphResults3)){
          x <- (graphResults3[[mm]][1])
          y <- (graphResults3[[mm]][2])
          xList3[[mm]] <- x
          yList3[[mm]] <- y
          mm <- mm+1
        }
        
        # actually plot the graph of risk score vs number of eggs
        # this can also probably be gotten rid of, but keep for now to see the plots and make sure they're correct
        # nvm i need it
        plot(xList2, yList2, xlab = "Risk Score 2", ylab = "# of Eggs")


        # can't do linear regression on a list, so use unlist() function on x and y coordinate lists
        x2 <- unlist(xList2)
        y2 <- unlist(yList2)
        
        
        
        # create the linear regression of the graph
        abline(line2 <- lm(y2 ~ x2))
        
        plot(xList3, yList3, xlab = "Risk Score 3", ylab = "# of Eggs")        
        
        x3 <- unlist(xList3)
        y3 <- unlist(yList3)
        
        abline(line3 <- lm(y3 ~ x3))
        
        # get the slope of that linear regression
        slope2 <- coef(line2)[2]
        slope3 <- coef(line3)[2]
        
        # make a list of all the results
        list1 <- list(slope2, slope3, landscape_item, wavescape)
        # add that list to the results list, creating a 3D array
        results[[i]] <- list1
        
        # add the slope to a list of all slopes
        # this is another thing I think I don't really need, and can be removed later for efficiency
        # just keeping it for now bc it makes the histograms really easy. Can instead be done with a for loop through results
        two_slopes[[i]] <- slope2
        three_slopes[[i]] <- slope3
        
       
        
        # increment the counter for the results array
        i <- i+1
      }
    }
  }
}






# save the landscape list data
save(results, file="landscape_list.RData")
?save


#comment order of results list
# in results (what is saved):

# [[i]][[1]] is the slope

# [[i]][[2]] is a vector of the different factors
# [[i]][[2]][[1]] is env
# [[i]][[2]][[2]] is frag
# [[i]][[2]][[3]] is prop
# [[i]][[2]][[4]] is repetition number

# [[i]][[3]] is the wavescape (landscape)
