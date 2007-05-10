"clust" <-
function( x=Cassini$x, k=3, method="kmeansHartigan",
                   seed=set.seed(123), distMethod="euclidean",
                   qtclustsize=0.7, iter.max=100, eps=0.1,
                   vals = TRUE, alt = NULL, coord=NULL, bic=NULL){
   if( method == "fixmahal" && vals == TRUE ){
     stop("vals are not available with method fixmahal")
   }
   
   seed
   vp <- FALSE    # varplot information
   if( distMethod == "rf" ){
     cat("\n *** calculating random forest proximity measure...\n")
     flush.console()
     d <- sqrt( 1 - randomForest(x, proximity=TRUE)$proximity)
   }
   if( distMethod == "correlation" ){
     d <- 1 - abs(cor(t(x)))
   }
   if( distMethod == "robustCorrelation" ){
     d <- 1 - abs(covMcd(t(x), cor=TRUE)$cor)
   }
   cosadistCalc <- function(x, method){
   ##cosadist part:
     platform="windows"
     cosadir="E:/matthias/R/cosa/"
     source("E:\\matthias\\R\\cosa\\r_cosa.q")
     if( any( method == c("hclustSingle", "hclustComplete", "hclustAverage",
                    "hclustWard", "hclustMcquitty", "hclustMedian",
                    "hclustcentroid"  ) ) ){ d <- cosadist(x,niter=70) } else {
     d <- cosadist(x,niter=80)
     N <- dim(x)[1]
     m <- matrix(0, ncol=N, nrow=N)
     ind=0
     f2 <- function(){
     for( i in 1:N ){
       if( i + 1 <= N ){
         for( j in (i+1):N ){
         ind <- ind + 1
             m[i,j] <- m[j,i] <- d[ind]

        }
      }
     }
     return(m)
    }
    d <- f2()
   }
   d
   }
   ## end cosadist part
   if( distMethod == "cosadist" ) d <- cosadistCalc(x=x, method=method)
   if( any(distMethod == c("gower", "bray", "kulczynski", "chord")) ) d <- gdist(x, method = distMethod)
   if( any(distMethod == c("morisita", "horn", "mountford")) ) d <- vegdist(x, method = distMethod)   
   menge1 <- c("gower", "bray", "kulczynski", "chord")
   menge2 <- c("morisita", "horn", "mountford")
   ##menge2 <- c(menge1, "euclidean", "rf", "cosadist")
   menge3 <- c("maximum", "canberra")
   
   findCenter <- function(x, clustering, k){
     a1 <- matrix(nrow=k, ncol=ncol(x))
     for( i in 1:k ){
       a1[i,] <- apply(x[ which( clustering == i ), , drop=FALSE ], 2, mean)
     }
     a1
   }
   
   clust <- list()
    seed
   cat(paste("\n *** running", method, "cluster algorithm...\n"))
   flush.console()   
   if( method == "kmeansHartigan" ) {
     if( distMethod == "euclidean" ) {
       d <- x 
       vp <- TRUE
     }
     ##if( distMethod != "euclidean" && distMethod != "rf" && distMethod != "cosadist") d <- dist(x, method = distMethod )
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- kmeans(d, k, algorithm="Hartigan-Wong", iter.max=iter.max, nstart=5 )
     clust$cluster <- a$cluster
     clust$centers <- a$centers
     clust$size <- a$size
     ### Achtung: mit eigener Distanzmatrix falsches Ergebnis!!!
   }
   if( method == "kmeansLloyd" ) {
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- kmeans(d, k, algorithm="Lloyd", iter.max=iter.max, nstart=5 )
     clust$cluster <- a$cluster
     clust$centers <- a$centers
     clust$size <- a$size
    }
   if( method == "kmeansForgy" ) {
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- kmeans(d, k, algorithm="Forgy", iter.max=iter.max, nstart=5 )
     clust$cluster <- a$cluster
     clust$centers <- a$centers
     clust$size <- a$size
   }
   if( method == "kmeansMacQueen" ) {
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- kmeans(d, k, algorithm="MacQueen", iter.max=iter.max, nstart=5 )
     clust$cluster <- a$cluster
     clust$centers <- a$centers
     clust$size <- a$size
   }
   if( method == "cmeansUfcl" ){
     if( any(distMethod == c("euclidean", "manhattan")) ){
       d <- x
       vp <- TRUE
     }
     ##if( all(distMethod != c("euclidean", "manhattan", "rf","cosadist")) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge3) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod)     
     a <- cmeans(d, k, method="ufcl")
     clust$cluster <- a$cluster
     clust$centers <- a$centers
     clust$size <- a$size
     clust$membership <- a$membership
   }
   if( method == "pam" || method == "clara" ){
     if( any(distMethod == c("euclidean", "manhattan")) ){
       d <- x; vp <- TRUE; a <- get(method)(d, k, metric=distMethod)
     }
     if( any(distMethod == menge1) ) gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod)      
     ##if( all(distMethod != c("euclidean", "manhattan", "rf","cosadist")) ) {
     if( any(distMethod == menge3 ) ){
       d <- dist(x, distMethod )
       a <- get(method)(d, k)
     }
     if( distMethod == "rf" ){
       a <- get(method)(d, k)
     }
     if( distMethod == "correlation" ){
       a <- get(method)(d, k)
     }
     if( distMethod == "cosadist" ){
       a <- get(method)(d, k)
     }
     if( any(distMethod == c(menge1,menge2)) ) a <- get(method)(d,k)
     clust$cluster <- a$cluster
     clust$center <- a$med
     clust$size <- a$clusinfo[,1]
   }
   if( method == "fanny" ){
     if( any(distMethod == c("euclidean", "manhattan")) ){
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == menge3) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod)   
     a <- fanny(d, k)
     clust$cluster <- a$cluster
     clust$center <- a$med
     clust$size <- a$clusinfo[,1]
     clust$membership <- a$mem
   }
   if( method == "bclust" ){
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan",menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod)   
     a <- bclust(d, k)
     clust$cluster <- a$cluster
     clust$center <- a$centers
     clust$size <- table(a$cluster)
   }
   if( method == "cmeans" || method == "cshell" ){
     if( any(distMethod == c("euclidean", "manhattan")) ){
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == menge3) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod)   
     a <- get(method)(d, k)
     clust$cluster <- a$cluster
     clust$centers <- a$centers
     clust$size <- a$size
     clust$membership <- a$membership
   }
   if( method == "Mclust" ){
     vp <- TRUE
     a <- Mclust(x, k, k)
     clust$cluster <- a$classification
     clust$center <- t(a$mu)
     clust$size <- table(a$classification)
     #clust$bic <- a$bic
     clust$BIC <- a$bic
     clust$model <- a$model
   }
   if( method == "kccaKmeans" ){
      cat("\n --------- \n Note: \n If an error message (detach(package:kernlab) is printed,")
      cat("\n ignore the error, please.")
      cat("\n Package kernlab *must* be detached, before running the algorithm!\n")
      flush.console()
      try(detach(package:kernlab))
      cat("\n --------- \n")  
      flush.console()
     if( distMethod == "euclidean" ){ a <- kcca(x, k, family=kccaFamily("kmeans"))
        vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod)
     if( distMethod != "euclidean" ){ 
       a <- kcca(as.matrix(d), k, family=kccaFamily("kmeans"))
     }
     clust$cluster <- a@cluster
     clust$center <- a@centers
     clust$size <- table(a@cluster)
   }
   if( method == "kccaKmedians" ){
      cat("\n --------- \n Note: \n If an error message (detach(package:kernlab) is printed,")
      cat("\n ignore the error, please.")
      cat("\n Package kernlab *must* be detached, before running the algorithm!\n")
      flush.console()
     try(detach(package:kernlab)) 
      cat("\n --------- \n")   
      flush.console()
     if( distMethod == "manhattan" ){ a <- kcca(x, k, family=kccaFamily("kmedians"))
        vp <- TRUE
     }
     if( any(distMethod == c("euclidean", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     if( distMethod != "manhattan" ){ 
       a <- kcca(as.matrix(d), k, family=kccaFamily("kmedians"))
     }
     clust$cluster <- a@cluster
     clust$center <- a@centers
     clust$size <- table(a@cluster)
   }
  if( method == "kccaAngle" ){
      cat("\n --------- \n Note: \n If an error message (detach(package:kernlab) is printed,")
      cat("\n ignore the error, please.")
      cat("\n Package kernlab *must* be detached, before running the algorithm!\n")
      flush.console()
     try(detach(package:kernlab)) 
      cat("\n --------- \n")   
      flush.console()
     if( distMethod == "euclidean" ){ a <- kcca(x, k, family=kccaFamily("angle"))
        vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     if( distMethod != "euclidean" ){ 
       a <- kcca(as.matrix(d), k, family=kccaFamily("angle"))
     }
     clust$cluster <- a@cluster
     clust$center <- a@centers
     clust$size <- table(a@cluster)
   }
   if( method == "kccaJaccard" ){
      cat("\n --------- \n Note: \n If an error message (detach(package:kernlab) is printed,")
      cat("\n ignore the error, please.")
      cat("\n Package kernlab *must* be detached, before running the algorithm!\n")
      flush.console()
     try(detach(package:kernlab)) 
      cat("\n --------- \n")   
      flush.console()
     if( distMethod == "euclidean" ){ a <- kcca(x, k, family=kccaFamily("jaccard"))
        vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     if( distMethod != "euclidean" ){ 
       a <- kcca(as.matrix(d), k, family=kccaFamily("jaccard"))
     }
     clust$cluster <- a@cluster
     clust$center <- a@centers
     clust$size <- table(a@cluster)
   }
   if( method == "kccaEjaccard" ){
      cat("\n --------- \n Note: \n If an error message (detach(package:kernlab) is printed,")
      cat("\n ignore the error, please.")
      cat("\n Package kernlab *must* be detached, before running the algorithm!\n")
      flush.console()
     try(detach(package:kernlab)) 
      cat("\n --------- \n")   
      flush.console()
     if( distMethod == "euclidean" ){ a <- kcca(x, k, family=kccaFamily("ejaccard"))
        vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     if( distMethod != "euclidean" ){ 
       a <- kcca(as.matrix(d), k, family=kccaFamily("ejaccard"))
     }
     clust$cluster <- a@cluster
     clust$center <- a@centers
     clust$size <- table(a@cluster)
   }
   if( method == "cclustKmeans" ){
     if( any( distMethod == c("euclidean","manhattan") ) ){ d <- x 
       vp <- TRUE
       a <- cclust(x, k, method="kmeans", dist=distMethod)
     }       
     ##if( any(distMethod == menge3) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod)
     a <- cclust(x, k, method="kmeans")
     clust$cluster <- a@cluster
     clust$center <- a@centers
     clust$size <- table(a@cluster)
   }
   if( method == "cclustHardcl" ){
     if( any( distMethod == c("euclidean","manhattan") )){ d <- x 
       vp <- TRUE
       a <- cclust(x, k, method="hardcl", dist=distMethod)
     }       
     ##if( any(distMethod == menge3) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod)
     a <- cclust(x, k, method="hardcl")
     clust$cluster <- a@cluster
     clust$center <- a@centers
     clust$size <- table(a@cluster)
   }
   if( method == "cclustNeuralgas" ){
     if( any( distMethod == c("euclidean","manhattan") )){ d <- x 
       vp <- TRUE
       a <- cclust(x, k, method="neuralgas", dist=distMethod)
     }       
     ##if( any(distMethod == menge3) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ){ d <- gdist(x, method=distMethod)
       stop("cclust can only deal with euclidean or manhattan distances")
     }  
     if( any(distMethod == menge2) ){ d <- vegdist(x, method=distMethod)
       stop("cclust can only deal with euclidean or manhattan distances")
     }  
     ##a <- cclust(as.matrix(d), k, method="neuralgas")
     clust$cluster <- a@cluster
     clust$center <- a@centers
     clust$size <- table(a@cluster)
   }
   if( method == "qtclustKmeans" ){
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- qtclust(x, qtclustsize, family=kccaFamily("kmeans"))
     clust$cluster <- a@cluster
     clust$center <- a@centers
     clust$size <- a@size
   }
   if( method == "qtclustKmedian" ){
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- qtclust(x, qtclustsize, family=kccaFamily("kmedian"))
     clust$cluster <- a@cluster
     clust$center <- a@centers
     clust$size <- a@size
   }
   if( method == "qtclustAngle" ){
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- qtclust(x, qtclustsize, family=kccaFamily("angle"))
     clust$cluster <- a@cluster
     clust$center <- a@centers
     clust$size <- a@size
   }
   if( method == "qtclustJaccard" ){
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- qtclust(x, qtclustsize, family=kccaFamily("jaccard"))
     clust$cluster <- a@cluster
     clust$center <- a@centers
     clust$size <- a@size
   }
   if( method == "qtclustEjaccard" ){
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- qtclust(x, qtclustsize, family=kccaFamily("ejaccard"))
     clust$cluster <- a@cluster
     clust$center <- a@centers
     clust$size <- a@size
   }
   if( method == "dbscan" ){
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- dbscan(x, eps=eps)
     clust$cluster <- a$classification
     clust$center <- "none"
     clust$size <- table(a$classification)
   }
   if( method == "speccRbfdot" ){
     library(kernlab)
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- specc(as.matrix(d),centers=k)
     clust$cluster <- a@.Data
     clust$center <- a@centers
     clust$size <- a@size
   }
   if( method == "speccPolydot" ){
     library(kernlab)
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod )
     a <- specc(as.matrix(d),centers=k, kernel="polydot")
     clust$cluster <- a@.Data
     clust$center <- a@centers
     clust$size <- a@size
   }   #  vanilladot tanhdot laplacedot besseldot anovadot splinedot
   if( method == "speccVanilladot" ){
     library(kernlab)
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- specc(as.matrix(x),centers=k, kernel="vanilladot")
     clust$cluster <- a@.Data
     clust$center <- a@centers
     clust$size <- a@size
   }
   if( method == "speccTanhdot" ){
     library(kernlab)
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- specc(as.matrix(x),centers=k, kernel="tanhdot")
     clust$cluster <- a@.Data
     clust$center <- a@centers
     clust$size <- a@size
   }
   if( method == "speccLaplacedot" ){
     library(kernlab)
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- specc(as.matrix(x),centers=k, kernel="laplacedot")
     clust$cluster <- a@.Data
     clust$center <- a@centers
     clust$size <- a@size
   }
  if( method == "speccBesseldot" ){
     library(kernlab)
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- specc(as.matrix(x),centers=k, kernel="besseldot")
     clust$cluster <- a@.Data
     clust$center <- a@centers
     clust$size <- a@size
   }
   if( method == "speccAnovadot" ){
     library(kernlab)
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- specc(as.matrix(x),centers=k, kernel="anovadot")
     clust$cluster <- a@.Data
     clust$center <- a@centers
     clust$size <- a@size
   }
   if( method == "speccSplinedot" ){
     library(kernlab)
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- specc(as.matrix(x),centers=k, kernel="splinedot")
     clust$cluster <- a@.Data
     clust$center <- a@centers
     clust$size <- a@size
   }
   if( method == "fixmahal" ){
     if( distMethod == "euclidean" ){ 
       d <- x
       vp <- TRUE
     }
     if( any(distMethod == c("manhattan", menge3)) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     f <- try(fixmahal(x))
     f2 <- try(fpclusters(f))
     clust$cluster <- f2
     clust$center <- "none"
     clust$size <- "none"
     clust$fixmahl <- TRUE
   }
   if( method == "hclustSingle" ){
     ##if( method != "rf" && distMethod != "cosadist") d <- dist(x, method = distMethod) 
     if( any(distMethod == c("euclidean", "manhattan", menge3) ) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- cutree(hclust(d, method="single"), k)
     clust$cluster <- as.numeric(a)
     clust$center <- as.numeric(findCenter(x, a, k))
     clust$size <- table(a)
   }
   if( method == "hclustComplete" ){
     if( any(distMethod == c("euclidean", "manhattan", menge3) ) ) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- cutree(hclust(d, method="complete"), k)
     clust$cluster <- as.numeric(a)
     clust$center <- findCenter(x, a, k)
     clust$size <- table(a)
   }
   if( method == "hclustAverage" ){
     if( any(distMethod == c("euclidean", "manhattan", menge3) )) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- cutree(hclust(d, method="average"), k)
     clust$cluster <- as.numeric(a)
     clust$center <- findCenter(x, a, k)
     clust$size <- table(a)
   }
   if( method == "hclustWard" ){
     if( any(distMethod == c("euclidean", "manhattan", menge3) )) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- cutree(hclust(d, method="ward"), k)
     clust$cluster <- as.numeric(a)
     clust$center <- findCenter(x, a, k)
     clust$size <- table(a)
   }
   if( method == "hclustMcquitty" ){
     if( any(distMethod == c("euclidean", "manhattan", menge3) )) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- cutree(hclust(d, method="mcquitty"), k)
     clust$cluster <- as.numeric(a)
     clust$center <- findCenter(x, a, k)
     clust$size <- table(a)
   }
   if( method == "hclustMedian" ){
     if( any(distMethod == c("euclidean", "manhattan", menge3) )) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- cutree(hclust(d, method="median"), k)
     clust$cluster <- as.numeric(a)
     clust$center <- findCenter(x, a, k)
     clust$size <- table(a)
   }
   if( method == "hclustcentroid" ){
     if( any(distMethod == c("euclidean", "manhattan", menge3) )) d <- dist(x, method = distMethod )
     if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
     if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod) 
     a <- cutree(hclust(d, method="centroid"), k)
     clust$cluster <- as.numeric(a)
     clust$center <- findCenter(x, a, k)
     clust$size <- table(a)
   }
   ### validity: ---------------------------------------------------------------
   cat("\n *** calculating validity measure... \n")
   flush.console()
   if( vals == FALSE ) clust.val <- m <- NA
   if( vals == TRUE && length(alt) > 1){
     if( length(coord) == 0 ){
       clust.val <- cluster.stats(dist(x), clust$cluster, as.numeric(alt))
     } else { clust.val <- cluster.stats(dist(coord), clust$cluster, as.numeric(alt)) }
     
     m <- data.frame( average.between = round(clust.val$average.between, 3), 
                    average.within = round(clust.val$average.within, 3),
                    avg.silwidth = round(clust.val$avg.silwidth, 3), 
                    hubertgamma = round(clust.val$hubertgamma, 3),
                    dunn = round(clust.val$dunn, 3), 
                    wb.ratio = round(clust.val$wb.ratio, 3),
                    corrected.rand = round(clust.val$corrected.rand, 3),
                    row.names = paste(method, "-", distMethod, sep=""))
                    
   }
   if( vals == TRUE && length(alt) == 0){
     if( length(coord) == 0 ){
       clust.val <- cluster.stats(dist(x), clust$cluster)
     } else { clust.val <- cluster.stats(dist(coord), clust$cluster) }
     m <- data.frame( average.between = round(clust.val$average.between, 3), 
                    average.within = round(clust.val$average.within, 3),
                    avg.silwidth = round(clust.val$avg.silwidth, 3), 
                    hubertgamma = round(clust.val$hubertgamma, 3),
                    dunn = round(clust.val$dunn, 3), 
                    wb.ratio = round(clust.val$wb.ratio, 3),
                    corrected.rand = NA,
                    row.names = paste(method, "-", distMethod, sep=""))
   }
   if( length(bic) > 0 ){
     cl <- Mclust(x,k,k)
     bics <- vector()
     for(i in 1:k ){
       bics[i] <- min(EMclust(x[cl$class==i,], 1), na.rm=TRUE)
     }
     ##m <- cbind(m,bics)
     clust$bic <- bics
   }
  clust$xdata <- x
  clust$method <- method
  clust$distMethod <- distMethod
  clust$k <- k
  clust$valTF <- vals
  clust$valMeasures <- m
  clust$silwidths <- clust.val$clus.avg.silwidths
  clust$separation <- clust.val$separation
  clust$diameter <- clust.val$diameter
  clust$average.distance <- clust.val$average.distance
  clust$median.distance <- clust.val$median.distance
  clust$average.toother <- clust.val$average.toother
  #clust$bics <- clust$bics
  clust$vp <- vp
  class(clust) <- "clust"
  if( vp == TRUE ){ colnames(clust$center) <- colnames(x)}
  class(clust) <- "clust"
  invisible(clust)
}

