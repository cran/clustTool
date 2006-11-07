"prepare" <-
function( x, scaling="classical", transformation="logarithm", powers="none" ){
  ## ---------------------------------------------------------------------------
  ## x ... matrix or data frame
  ## scaling: - classical
  ##          - robust (median)
  ##          - onestep (onestep)
  ##          - nonescale
  ##          - logcentered
  ## transformation: - logarithm
  ##                 - boxcox (powers must be chosen)
  ##                 - bcOpt (with optimal powers)
  ## powers ... vector of powers (default = "none")
  ## ---------------------------------------------------------------------------
  ### scaling:
  classical <- function(x){ scale(x) }
  robust <- function(x){ scale(x, center=apply(x, 2, median)) }
  onestep <- onestep <- function(x){
    mMedian <- median(x)
    mMad <- mad(x)
    constant <- 3/1.486
    limit1 <- mMedian + constant * mMad
    limit2 <- mMedian - constant * mMad
    w1 <- which( x > limit1 )
    w2 <- which( x < limit2 )
    x[w1] <- limit1
    x[w2] <- limit2
    mean(x)
  }
  robust2 <- function(x){ scale(x, center=apply(x, 3, onestep)) }
  nonescale <- function(x){ x }
  ### transformation:
  logarithm <- function(x){ log(x) }
  boxcox <- function(x, powers){ box.cox(x, powers) }
  bcOpt <- function(x){
                       b <- box.cox.powers(x)$lambda
                       box.cox(x, b)
                       }
  nonetrans <- function(x){ x }
  logcentered <- function(x){ 
    xgeom=10^apply(log10(x),1,mean)
    x2=x/xgeom
    x2
  }
  get(scaling)( get(transformation)( x ) )
}

