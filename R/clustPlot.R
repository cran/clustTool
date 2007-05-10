"clustPlot" <-
function(coord, clust, k, val="silwidths", which.plot=c(1,2), Map="kola.background", texth=0.75){
  if(k < 13){r1 <- 3;r2 <- 4}
  if(k < 10){r1 <- 3;r2 <- 3}
  if(k < 7){r1 <- 2;r2 <- 3}
  if(k < 5){r1 <- 2;r2 <- 2}
  if(k < 3){r1 <- 1;r2 <- 2}
  attach(clust)
  vals <- get(val)
  detach(clust)
  if( which.plot == 1 || all(which.plot == c(1,2)) ){   ### erster und zweiter plot
  par(mfrow=c(r1,r2),pty="s",xaxt="n", yaxt="n", mar=c(0,0,0,0), omi=c(0,0,0.3,0),
      bg="white", fg="black")

  if( any( clust$method == c("cmeans", "cmeansUfcl")) ){
    ###g <- rep(0, 10)
    ###grey.col <- rep(0, 10)
    ###for( j in 1:10){
    ###  g[j] <- 2 * j / (10 * k)
    ###  grey.col[j] <- 1 - (j / 10)
    ###}
    for(i in 1:k){
      plot(coord, col=0, xlab="",ylab="")
      text(x=800000, y= 7870000, length(which(clust$cluster==i)))
      #title(paste(xname,"-",methodname,"cluster",i))
      ###for(j in 1:10){
      ###  points(coord[clust$mem[,i] > g[j], ],pch=15,col=gray(grey.col[j]))
      ### }
      points(coord, pch=15, col=gray(1-clust$mem[,i]))
      if( Map == "kola.background" ) plotKola.background(which.map=c(1,2,3,4),map.col=c(5,"grey","grey","grey"),map.lwd=c(1,1,1,1),
                         add.plot=TRUE)
    }
  #  legend( x=800000, y= 8000000, legend=g, col=c(gray(grey.col[1]), gray(grey.col[2]), gray(grey.col[3]), gray(grey.col[4]),
  #        gray(grey.col[5]), gray(grey.col[6]), gray(grey.col[7]), gray(grey.col[8]), gray(grey.col[9]), gray(grey.col[10])),
  #        pch=rep(15, 10) )
  vp1 <- viewport(x=0.5,y=0.5, w=1,height=1)
  pushViewport(vp1)
  if( clust$method != "Mclust" ){
    grid.text(x=0.5,y=0.98,
      label=paste(clust$method, clust$distMethod, paste(names(clust$xdata), collapse=""), "Memberships" ))
  } else if( clust$method == "Mclust" ){     grid.text(x=0.5,y=0.98,
      label=paste(clust$method, paste(names(clust$xdata), collapse=""), "Memberships" ))
  }
  popViewport()
  X11()
  }
  #X11()
  par(mfrow=c(r1,r2),pty="s",xaxt="n", yaxt="n", mar=c(0,0,0,0), omi=c(0,0,0.3,0),
      bg="white", fg="black")

  ###untere <- vals + abs(min(vals))+0.05
  ###fac1 <- 1/abs(max(untere))
  ###grays <- gray(1 - untere*fac1)
  if( all( vals > 0 ) ){
    grays <- gray(1 - vals/max(vals))
  } else {
      v <- vals
      v <- scale(vals)
      v <- v+abs(min(v))+0.05
      v2 <- 1 - v/max(v)
      v2[v2 > 0.9] <- 0.9
      grays <- gray(v2)
    }
  for(i in 1:k){
    plot(coord, col=gray(0.95), xlab="", ylab="means", pch=15)
    if( Map == "kola.background" ) plotKola.background(which.map=c(1,2,3,4),map.col=c(5,"grey","grey","grey"),map.lwd=c(1,1,1,1),
                         add.plot=TRUE)
    if(length(val) > 0){
    points(coord[clust$cluster==i,], pch=15, col=grays[i])
    } else { points(coord[clust$cluster==i,], pch=15, col=4) }
    text(x=800000, y= 7850000, paste("obs =",length(which(clust$cluster==i))))
    text(x=720000, y= 7890000, paste(val, "=", round(vals[i],2)))
    text(x=373951, y=7882172, i, cex=1.3)
  }
  vp1 <- viewport(x=0.5,y=0.5, w=1,height=1)
  pushViewport(vp1)
  if(clust$method != "Mclust"){
    #grid.text(x=0.5,y=0.98,
    #  label=paste(clust$method, clust$distMethod, paste(names(clust$xdata), collapse=""), val ))
     grid.text(x=0.5,y=0.965,
    label=clust$method, gp=gpar(cex=1.3))     
     } else if( clust$method == "Mclust" ){
    #grid.text(x=0.5,y=0.98,
    #  label=paste(clust$method, paste(names(clust$xdata), collapse=""), val ))
     grid.text(x=0.5,y=0.965,
    label="Mclust", gp=gpar(cex=1.3))
    }
   popViewport()
    pushViewport(vp1)
  if(clust$method != "Mclust"){
    #grid.text(x=0.5,y=0.98,
    #  label=paste(clust$method, clust$distMethod, paste(names(clust$xdata), collapse=""), val ))
     grid.text(x=0.5,y=0.965,
    label=clust$method, gp=gpar(cex=1.3))     
     } else if( clust$method == "Mclust" ){
    #grid.text(x=0.5,y=0.98,
    #  label=paste(clust$method, paste(names(clust$xdata), collapse=""), val ))
     grid.text(x=0.5,y=0.965,
    label="Mclust", gp=gpar(cex=1.3))
    }
   popViewport()
  }
  if( clust$vp == FALSE ) cat("\n --------- \n *** Note:\n elementplot is useless and not printed, \n because distances are used for clustering \n and not the data itself \n --------- \n")
  if( clust$vp == TRUE ){ yes <- TRUE } else { yes <- FALSE }
  if( all(which.plot==c(1,2)) && yes == TRUE ){ X11() }
  if( (all(which.plot==c(1,2)) && yes == TRUE) || (yes==TRUE & which.plot==2) ){
  ##cent <- matrix(clust$centers,ncol=k,byrow=T)
  # names of the variables
  rnam <- colnames(clust$center)
  # p ... Anzahl der Variablen
  # k ... Anzahl der Cluster
  p <- dim(clust$center)[2]
  #name=""
  for(j in 1:p){
    rnam[j] <- substring(rnam[j],1,2)
  }
  #######rownames(clust$centers) <- rnam
  ma <- max(abs(clust$center))
  # create the plot
  par(mfrow=c(1,1),cex=1,cex.axis=1,cex.lab=1.5,xaxt="s",yaxt="s")
  plot(clust$center[,1],type="n",xlim=range(0,1),ylim=range(-ma-0.3,ma+0.3),
  ylab="cluster means",xlab="", xaxt="n")
  segments(0, 0, 1, 0)
  #segments(0, 0.5, 1, 0.5, lty = 2)
  #segments(0, -0.5, 1, -0.5, lty = 2)
  if( clust$method == "Mclust" ){  } else{
    title(paste(clust$method, ",", clust$distMethod)) }
  bb <- c(0,1)
  bb1 <- bb/k
  ba <- seq(from = bb1[1], by = bb1[2])
  ba1 <- ba[2]/20
  ba2 <- c(0,ba1)
  segments(0,-ma,0,ma)
  for(i in 1:(k+1)){
    segments(ba[i],-ma,ba[i],ma)
  }
  # create weights
  weight <- 0
  for(i in 1:k){
    weight[i] <- clust$size[i]
  }
  sumweight <- sum(as.numeric(weight))
  # text
  for(j in 1:k){
    text(seq(from=ba[j]+ba[2]/p,to=ba[j+1]-ba[2]/p,
      by=(ba[j+1]-ba[j]-2*ba[2]/p)/(p-1)), clust$center[j,], rnam,
    col="black",cex=texth)
    text(ba[j]+ba[2]/2,(ma+0.1)*(-1),paste(j,sep=""),col="black",cex=1.3)
    text(ba[j]+ba[2]/2,ma+0.3,paste(as.numeric(weight[j], sep="")),
    #substring(as.numeric(weight[j])/sumweight,1,4),sep=""),
    col=1,cex=1.2)
  }
mtext("Number of observations for each cluster", cex=1.3)
mtext("Cluster number", side=1, cex=1.3)
  }
}

