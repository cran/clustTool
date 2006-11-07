"print.clust" <-
function(x, ...){
 cat("\n ------------------------------------------------------- ")
 cat("\n Clustering method:         ")
 print(x$method)
 cat("\n Number of clusters:          ")
 print(x$k)
 cat("\n size of clusters:            ")
 print(x$size)
 cat("\n Transformation of the data:  ")
 print(x$trans)   
 cat("\n Standardization of the data: ")
 print(x$scaling)
 cat("\n Distance measure:            ")
 print(x$distMethod)
 cat("\n ------------------------------------------------------- ")
 cat("\n Detailed informations are saved in object clust1")
 cat("\n Try names(clust1) or look at the help pages of clust")
 #cat("\n Cluster centers: \n ")
 #print(x$centers)
 #NextMethod("print")
 #invisible(x)
}

