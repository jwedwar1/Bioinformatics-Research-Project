### run functions
source('landscapes/generateModuleLandscape_function.R')
source('landscapes/plotLandscape.R')
source('landscapes/shuffleLandscape.R')
source('population_function.R')
source('moore_function.R')
source('module_neighborhood_function.R')
source('plotEggs.R')
source('landscapes/wavelet_landscape.R')


##############################################################################
##############################################################################
structure.vec <- c(TRUE)
shuffle.vec <- c(TRUE)
risk.vec <- c(0, 0.2)
perception.vec <- c(0.7)
mvt.vec <- c(1)
mvt.mod.vec <- c(0)
mem.depth.vec <- c(10)
mem.weight.vec <- c(0, 0.05, 0.1)


replicates <- 10

directions.table <- array(dim = c( length(structure.vec)*
                                   length(shuffle.vec)*
                                   length(risk.vec)*
                                   length(perception.vec)*
                                   length(mvt.vec)*
                                   length(mvt.mod.vec) * 
                                   length(mem.depth.vec) *
                                   length(mem.weight.vec) *
                                   replicates, 14) )



colnames(directions.table) <- c('id', 
                             'replicate',
                             'structure.vec', 
                             'shuffle.vec', 
                             'risk.vec', 
                             'perception.vec', 
                             'mvt.vec', 
                             'mvt.mod.vec',
                             'mem.depth.vec',
                             'mem.weight.vec',
                             'local.coef',
                             'local.p.value',
                             'regional.coef',
                             'regional.p.value')

number.of.eggs <- rep(NA, nrow(directions.table))

count <- 0
##############################################################################
for (ay in 1:length(structure.vec) ) {
  for (be in 1:length(shuffle.vec) ) {
    
    
    # Generate the landscape
    a <- generateModuleLandscape(MATRIX.SIZE = 0, 
                                 PATCH.DIM = 4, 
                                 MODULE.DIM = 6, 
                                 STRUCTURE = structure.vec[ ay ],
                                 SHUFFLE = shuffle.vec[ be ])
    
    
    for (ce in 1:length(risk.vec) ) {
      for (de in 1:length(perception.vec) ) {
        for (ee in 1:length(mvt.vec) ) {
          for (ef in 1:length(mvt.mod.vec) ) {
            for (ach in 1:length(mem.depth.vec) ) {
              for (eye in 1:length(mem.weight.vec) ) {
                
            for (ge in 1:replicates){

count <- count + 1            
directions.table[count, 1:8] <- c(count, 
                                  ge,
                                  structure.vec[ ay ] * 1, 
                                  shuffle.vec[ be ] * 1, 
                                  risk.vec[ ce ], 
                                  perception.vec[ de ], 
                                  mvt.vec[ ee ], 
                                  mvt.mod.vec[ ef ]) 


## Simulation the population
sim <- pop.habitatselection(POP.SIZE = 250,
                            LANDSCAPE = a$module.landscape,
                            RISK.MAG = risk.vec[ ce ],
                            PERCEPTION = perception.vec[ de ],
                            MVT = mvt.vec[ ee ],
                            MVT.MOD = mvt.mod.vec[ ef ],
                            MEM.DEPTH = mem.depth.vec[ ach ],
                            MEM.WEIGHT = mem.weight.vec[ eye ])

## Moore neighborhoods
# moore.out <- moore.summary(LANDSCAPE = a$module.landscape,
#                            EGG.LANDSCAPE = sim$egg.landscape,
#                            MOORE.RANGE = 3)

## Module neighborhoods
module.out <- module.neighborhood(LANDSCAPE = a$module.landscape,
                                  EGG.LANDSCAPE = sim$egg.landscape, 
                                  MODULE.DIM = a$inputs$MODULE.DIM,
                                  MOD.EXTRACT = a$module.extract, 
                                  MOD.INDEX = a$mod.index)


number.of.eggs[ count ] <- sum(sim$egg.landscape)

fit.local <- lm(module.out$module.table[,"mean.egg.safe"] ~ module.out$module.table[,"n.risky"])
summary(fit.local)

  directions.table[count, 11:12] <- c(summary(fit.local)$coef[2,1], summary(fit.local)$coef[2,4])

  
if (perception.vec[ de ] + risk.vec[ ce ] < 1) {
  fit.regional <- lm(module.out$module.table[,"mean.egg.safe"] ~ module.out$mean.module.risk)
  summary(fit.regional)

    directions.table[count, 13:14] <- c(summary(fit.regional)$coef[2,1], summary(fit.regional)$coef[2,4])
  }


print(count / nrow(directions.table))

                }
              }
            }
          }
        }
      }
    }
  }
}



# directions.table[which(directions.table[,11] < 0), 1:10]
# directions.table[which(directions.table[,12] < 0.05), 1:10]
# 
# directions.table[which(directions.table[,13] < 0.05), 1:10]
# directions.table[which(directions.table[,14] < 0.05), 1:10]


x <- array(dim = c(replicates, 14, nrow(directions.table) / replicates))
count <- 0
for (i in seq(1, nrow(directions.table), by = dim(x)[1])){
  count <- count + 1
  x[,,count] <- directions.table[i:(i + (dim(x)[1] - 1)),]
}
colnames(x) <-colnames(directions.table)

######
# par(mfrow=c(1,2))
local.estimates <- x[,11,]
local.pvalues <- x[,12,]
local.sig.ind <- which(local.pvalues <= 0.05, arr.ind = TRUE)

barplot(table(local.sig.ind[,2]) / dim(x)[1],
        main = 'number significant/local/', 
        ylim = c(0,1))

local.estimates[which(local.pvalues > 0.05)] <- NA

n.sig.local <- colSums(!is.na(local.estimates))
barplot(colMeans(local.estimates, na.rm = TRUE),
        main = 'mean of sig estimates/local')

## Local boxplot
boxplot( x[,11,], col = rgb(0,0,1,0.3))
abline(h = 0, lty = 2, lwd = 0.5)
for (i in 1:16){
 if (t.test(x[,11,i], rep(0, replicates))$p.value <= 0.05){
      text(i, 0 , '*', cex = 4)
     }
}

        
## (SIGNIFICANT REPLCIATES ONLY)        
boxplot(local.estimates, col = rgb(0,0,1,0.3),
        main = 'mean of sig estimates/local', width = n.sig.local)
abline(h = 0, lty = 2, lwd = 0.5)
points(1:16, colMeans(local.estimates, na.rm = TRUE), pch = 19, col = 4)
local.t.test.sig <- rep(NA, 16)
for (i in which(n.sig.local > 1)){
  local.t.test.sig[i] <- t.test(local.estimates[,i], rep(0, replicates))$p.value < 0.05
  if (local.t.test.sig[i] == TRUE){text(i, 0 , '*', cex = 4)}
}


regional.estimates <- x[,13,]
regional.pvalues <- x[,14,]
regional.sig.ind <- which(regional.pvalues <= 0.05, arr.ind = TRUE)

barplot(table(regional.sig.ind[,2]) / dim(x)[1],
        main = 'number significant/regional/', 
        ylim = c(0,1))

regional.estimates[which(regional.pvalues > 0.05)] <- NA

n.sig.regional <- colSums(!is.na(regional.estimates))
barplot(colMeans(regional.estimates, na.rm = TRUE),
        main = 'mean of sig estimates/regional')

## Regional boxplot
boxplot(x[,13,], col = rgb(1,0,0,0.3),
        main = 'mean of sig estimates/regional')
abline(h = 0, lty = 2, lwd = 0.5)

## (SIGNIFICANT REPLCIATES ONLY)        
boxplot(regional.estimates, col = rgb(1,0,0,0.6),
        main = 'mean of sig estimates/regional', width = n.sig.regional)
abline(h = 0, lty = 2, lwd = 0.5)
points(1:16, colMeans(regional.estimates, na.rm = TRUE), pch = 19, col = 4)
regional.t.test.sig <- c(NA, rep(16))
for (i in which(n.sig.regional > 1)){
  regional.t.test.sig[i] <- t.test(regional.estimates[,i], rep(0, replicates))$p.value < 0.05
  if (regional.t.test.sig[i] == TRUE){text(i, 0 , '*', cex = 4)}
}



# par(mfrow=c(1,2))
hist(local.estimates[which(local.pvalues <= 0.05)], col = rgb(0,0,1,0.5), 
     xlab = "Estimates", main = "Local")
sum(local.estimates[which(local.pvalues <= 0.05)] > 0) / length(local.estimates[which(local.pvalues <= 0.05)])

hist(regional.estimates[which(regional.pvalues <= 0.05)], col = rgb(1,0,0,0.5), 
     xlab = "Estimates", main = "Regional")
sum(regional.estimates[which(regional.pvalues <= 0.05)] > 0) / length(regional.estimates[which(regional.pvalues <= 0.05)])


############################################################################################################
############################################################################################################

### results by treatment
full.local.estimates <- x[,11,]
full.regional.estimates <- x[,13,]

par(mfrow=c(1,2))
boxplot(full.local.estimates, col = rgb(0,0,1,0.6))
abline(h=0,lty=2)
boxplot(full.regional.estimates, col = rgb(1,0,0,0.6))
abline(h=0,lty=2)


par(mfrow = c(4,4), mar = c(1,1,0,0))
for (i in 1:16){
  hist(full.local.estimates[,i], col = rgb(0,0,1,0.6))
  abline(v = 0, col = 2, lwd = 2)
  }

for (i in 1:16){
  hist(full.regional.estimates[,i], col = rgb(1,0,0,0.6))
  abline(v = 0, col = 2, lwd = 2)
}

boxplot(array(number.of.eggs, dim = c(replicates, length(number.of.eggs)/replicates)),
        col = rgb(0,1,0.5,0.5),
        main = "number of eggs laid")
