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
  ##                 - logcentered
  ##                 - logratio
  ##                 - iso
  ## powers ... vector of powers (default = "none")
  ## ---------------------------------------------------------------------------
  ### scaling:
  classical <- function(x){ scale(x) }
  robust <- function(x){ t(t((t(t(x) - apply(x, 2, median))))/apply(x,2,mad)) }
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
  logratio <- function(x){
    w <- which(colnames(x) == sel)
    log10(x[,-w])-log10(x[,w])%*%t(rep(1,2))
  }
  #iso <- function(x){
  # PF, 05.04.2007
  # isometric transformation
  # INPUT:
  # x ... compositional data
  # OUTPUT
  # x.iso ... 2 columns matrix 
  # isometric transformation according to paper:
  #  x.iso=matrix(NA,nrow=nrow(x),ncol=ncol(x)-1)
  #   for (l in 1:nrow(x.iso)){
  #     for (i in 1:ncol(x.iso)){
  #      x.iso[l,i]=sqrt((i)/(i+1))*log(((prod(x[l,1:i]))^(1/i))/(x[l,i+1]))
  #     }
  #   }
  #  return(x.iso)
  #}
  iso <- function(x){
  # PF, 05.04.2007, fast version MT, 05.05.2007
  # isometric transformation
  # INPUT:
  # x ... compositional data
  # OUTPUT
  # x.iso ... 2 columns matrix 
  # isometric transformation according to paper:    
    x.iso=matrix(NA,nrow=nrow(x),ncol=ncol(x)-1)
    for (i in 1:ncol(x.iso)){
      x.iso[,i]=sqrt((i)/(i+1))*log(((apply(as.matrix(x[,1:i]), 1, prod))^(1/i))/(x[,i+1]))
    }
  return(x.iso)
  }
  get(scaling)( get(transformation)( x ) )
}

