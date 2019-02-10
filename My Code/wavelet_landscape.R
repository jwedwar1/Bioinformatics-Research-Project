### Modified code from Lasky and Keitt AmNat 2015

# source('plotLandscape_function.R')
  
library(waveslim)
library(lattice)

generateWavelet_landscape <- function(LANDSCAPE.SIZE = 128, ENV, FRAG, PROP.MATRIX = 0, RISK.QUANTILE = 0.5){
  
  ### function to re-scale wavelet coefficients
  waveSynth2 <- function(dim, varFun, ..., wavelet = "haar") {
    require(waveslim)
    varFun <- match.fun(varFun)
    dim <- rep(dim, length.out = 2)
    dim <- 2 ^ round(log2(dim))
    x <- matrix(rnorm(prod(dim)), dim[1], dim[2])
    nlev <- log2(min(dim))
    w <- dwt.2d(x, wavelet, nlev)
    freq <- 1 / 2 ^ rep(seq(1, nlev), each = 3)
    for (i in 1:length(freq)) w[[i]] <- w[[i]] * sqrt(varFun(freq[i], ...))
    idwt.2d(w)
  }
  
  ### Specific functional form for rescaling wavelet coefficients
  
  charGauss <- function(x, sigma) exp(-sigma ^ 2 * x ^ 2 / 2)
  
        ### GENERATE WAVELET LANDSCAPE ###
        
        landscape.size <- LANDSCAPE.SIZE
        
        E <- ENV
        y <- waveSynth2(landscape.size, charGauss, exp(E), wavelet = "la8")
        
        ### Rescale environment to range from 0-100
        y <- y - min(y)
        y <- y * 99.9999/max(y)
        
        ### fragmentation mask
        F <- FRAG
        x <- waveSynth2(landscape.size, charGauss, exp(F), wavelet = "la8")
        
        ## Values of (-1)indicates matrix, which is 'prop.matrix' of landscape
        prop.matrix <- PROP.MATRIX
        
        if (prop.matrix == 0){
              x[1:length(x)] <- 1
            } else {
              x[x < quantile(x, prop.matrix)] <- -1
              x[x > quantile(x, prop.matrix)] <- 1
        }
        
        ###  Combine the reserve mask and the underlying environmental grid
        y2 <- x * y
        y2[y2 < 0] <- -1
        y2[y2 == 0] <- 1
        
        ### Convert landscape to 0,1,2 scale (matrix, safe, risk)
        tmp.y2 <- y2
        
        ## Proprtion of risky patches in landscape
        risk.quantile <- RISK.QUANTILE
        
        tmp.y2[y2 == -1] <- 0
        tmp.y2[tmp.y2 > 0 & tmp.y2 <= quantile(tmp.y2[tmp.y2 > 0], risk.quantile)] <- 1
        tmp.y2[tmp.y2 > quantile(tmp.y2[tmp.y2 > 0], risk.quantile)] <- 2
        
        wavelet_landscape <- tmp.y2
  
  return(wavelet_landscape)
}

#
wavescape <- generateWavelet_landscape(LANDSCAPE.SIZE = 64,
                                        ENV = 2,
                                        FRAG = 2,
                                        PROP.MATRIX = 0.0,
                                        RISK.QUANTILE = 0.6)
#plotLandscape(wavescape)