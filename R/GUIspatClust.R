"GUIspatClust" <-
function(){
  data(bhorizon)
  data(chorizon)
  data(humus)
  data(kola.background)

Map <- "no"  
require(tcltk)

 putRcmdr <-
function (x, value) 
assign(x, value, envir = RcmdrEnv())


fontHeading1 <- tkfont.create(family="times",size=12,weight="bold")
tt <- tktoplevel()
tkwm.title(tt,"spatClust GUI")
#tt1 <- tktoplevel()
PressedDaten <- function()
{
 fileNameDaten <- tclvalue(tkgetOpenFile())
 if (!nchar(fileNameDaten)){
    tkmessageBox(message="No file was selected!")
  } else{
    tkmessageBox(message=paste("The file selected was",fileNameDaten))
  }
 print(fileNameDaten)
}
PressedActive <- function()
{

#require(tcltk)
tt<-tktoplevel()
scr <- tkscrollbar(tt, repeatinterval=5,
                                   command=function(...)tkyview(tl,...))
tl<-tklistbox(tt,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
tkgrid(tklabel(tt,text="Set Active Data Set"))
tkgrid(tl,scr)
tkgrid.configure(scr,rowspan=4,sticky="n")
listDataSets <-
function (envir = .GlobalEnv, ...) 
{
    Vars <- ls(envir = envir, all.names = TRUE)
    if (length(Vars) == 0) 
        return(Vars)
    names(which(sapply(Vars, function(.x) is.data.frame(get(.x)))))
}
dataSetNames <- listDataSets()
for (i in 1:length(dataSetNames))
{
    tkinsert(tl,"end",dataSetNames[i])
}
tkselection.set(tl,2)  # Default fruit is Banana.  Indexing starts at zero.

OnOK <- function()
{
    dataSetChoice <- dataSetNames[as.numeric(tkcurselection(tl))+1]
    tkdestroy(tt)
    #msg <- paste("Good choice! ",dataSetChoice,"s are delicious!",sep="")
    #tkmessageBox(message=msg)
    ActiveDataSet(get(dataSetChoice))
        
}
OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
tkgrid(OK.but)
tkfocus(tt)
# selectActiveDataSet()
  
}

  
#Daten.but1 <- tkbutton(tt, text="load R Data", command=PressedDaten)
#Daten.butActive1 <- tkbutton(tt, text="Set Active Data", command=PressedActive)
#Daten.but <- tkgrid(tklabel(tt,text="(optional) Load R data:", font=fontHeading1), Daten.but1, tklabel(tt, text="(1) Set active data set", font=fontHeading1), Daten.butActive1)
#if(dim(.activeDataSet)[2] > 1) tkgrid(tklabel(tt, text=colnames(.activeDataSet)))  ## Auflistung im Tablet.



###   Variablenselektion:   ###########################

#varSelection <- logical()
#varSelection <- vector()  #n
#putRcmdr(value=vector(), x=c("varSelection"))

PressedVariablen <- function()
{
   tt1 <- tktoplevel()
   fontVars <- tkfont.create(family="courier",size=12)
 varName <- colnames(.activeDataSet) 
   cbValue <- cb <- list()
 for( i in 1:length(varName) ){
   if( exists("varSelection") ){
     v1 <- varSelection
     v1 <- ifelse(v1 == TRUE, "1", ifelse(v1==FALSE, FALSE, FALSE))
     cbValue[[i]] <- tclVar(v1[i])
   } else {
   cbValue[[i]] <- tclVar("1")
     }
   cb[i] <- tkcheckbutton(tt1)
   tkconfigure(cb[[i]], variable=cbValue[[i]])
   if( i %in% seq(2,300,2) ){
     j <- i - 1
     tkgrid(tklabel(tt1,text=varName[j], font=fontVars),cb[[j]], tklabel(tt1, text=varName[i], font=fontVars), cb[[i]], sticky="w")
   }
   if( i %in% seq(1,301,3) & i == length(varName) ){
     tkgrid(tklabel(tt1, text=varName[i], font=fontVars), cb[[i]], sticky="w")
   }
}   
 OnOK <- function(){
   CVAL <- vector()
   CVAL <- rep(TRUE,length(varName))
   for(i in 1:length(varName)) CVAL[i] <- as.character(tclvalue(cbValue[[i]]))
   CVAL <- ifelse(CVAL == "0", FALSE, ifelse(CVAL=="1", TRUE, FALSE))
   #ActiveDataSet(subset(.activeDataSet, select=CVAL))
   putRcmdr(value=CVAL, x=c("varSelection"))
   print(colnames(ActiveDataSet()))
   tkdestroy(tt1)
   return(CVAL)
 }
 OK.but <- tkbutton(tt1,text="OK",command=OnOK)
 tkbind(entry.Name, "<Return>",OnOK)
 tkgrid(tklabel(tt1, text="      "), OK.but)
 tkfocus(tt1)
}

###   Selection of Coordinates:   #######################################
RcmdrEnv <- function() {
    pos <-  match("RcmdrEnv", search())
    if (is.na(pos)) { # Must create it
        RcmdrEnv <- list()
        attach(RcmdrEnv, pos = length(search()) - 1)
        rm(RcmdrEnv)
        pos <- match("RcmdrEnv", search())
        }
    return(pos.to.env(pos))
    }

PressedCoord <- function()
{
tt<-tktoplevel()
scr <- tkscrollbar(tt, repeatinterval=5,
                                   command=function(...)tkyview(tl,...))
tl <- tklistbox(tt,height=4,selectmode="multiple",yscrollcommand=function(...)tkset(scr,...),background="white")
tkgrid(tklabel(tt,text="Select Coordinates"))
tkgrid(tl,scr)
tkgrid.configure(scr,rowspan=4,sticky="n")
dataSetVariables <- colnames(ActiveDataSet())
for (i in 1:length(dataSetVariables))
{
    tkinsert(tl,"end",dataSetVariables[i])
}
tkselection.set(tl,2)  # Default fruit is Banana.  Indexing starts at zero.

OnOK <- function()
{
    dataSetChoice <- dataSetVariables[as.numeric(tkcurselection(tl))+1]
    coord <- list()
    putRcmdr(value=dataSetChoice, x=c("coordNames"))
    #save(subset(ActiveDataSet(), select=coordNames), file="coords.RData")
    tkdestroy(tt)
    #ActiveDataSet(dataSetChoice)
    #assign(coord, dataSetChoice)    
}
OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
tkgrid(OK.but)
tkfocus(tt)
# selectActiveDataSet()
  
}

#Daten.butCoord1 <- tkbutton(tt, text="Coordinates", command=PressedCoord)
#Coord.but <- tkgrid( tklabel(tt, text="(3) Select coordinates", font=fontHeading1), Daten.butCoord1, tklabel(tt, text="(2) Selection of variables:", font=fontHeading1), Var.but1)


###   Karte hereinholen   #################################################

PressedMap <- function()
{
tt<-tktoplevel()
scr <- tkscrollbar(tt, repeatinterval=5,
                                   command=function(...)tkyview(tl,...))
tl <- tklistbox(tt,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
tkgrid(tklabel(tt,text="Select Map"))
tkgrid(tl,scr)
tkgrid.configure(scr,rowspan=4,sticky="n")
Maps <- ls(envir = .GlobalEnv)
for (i in 1:length(Maps))
{
    tkinsert(tl,"end",Maps[i])
}
tkselection.set(tl,2)  # Default fruit is Banana.  Indexing starts at zero.

OnOK <- function()
{
    dataSetChoice <- Maps[as.numeric(tkcurselection(tl))+1]
    coord <- list()
    putRcmdr(value=dataSetChoice, x=c("Map"))
    tkdestroy(tt)
}
OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
tkgrid(OK.but)
tkfocus(tt)
# selectActiveDataSet()
  
}

###   Hierachischer Plot   ##################################################

#varSelection
#hclust(prepare(subset(ActiveDataSet(), select=varSelection), transformation=rbVal10, scaling=rbVal11), k=as.numeric(NameVal), method=rbVal1, distMethod=rbVal2

###   Buttons im GUI   ######################################################

Daten.but1 <- tkbutton(tt, text="Load R Data (optional)", command=PressedDaten)
Daten.butActive1 <- tkbutton(tt, text="Set Active Data", command=PressedActive)
Var.but1 <- tkbutton(tt, text="Select Variables", command=PressedVariablen)

tkgrid(tklabel(tt, text=""))
Daten.but <- tkgrid(Daten.but1,  tklabel(tt, text=""), Var.but1)

#Daten.butActive1, tklabel(tt, text="Set active data set", font=fontHeading1))

Daten.butCoord1 <- tkbutton(tt, text="Select Coordinates", command=PressedCoord)

Coord.but <- tkgrid( Daten.butActive1,  tklabel(tt, text="", font=fontHeading1), Daten.butCoord1 )


Daten.butMap1 <- tkbutton(tt, text="Load Map (optional)", command=PressedMap)
Daten.but <- tkgrid(Daten.butMap1, tklabel(tt, text="", font=fontHeading1))


###   Transformation, Standardisierung   #################################################

fontHeading <- tkfont.create(family="times",size=14,weight="bold",slant="italic")


## fuer Transformation:
rb100 <- tkradiobutton(tt)
rb101 <- tkradiobutton(tt)
rb102 <- tkradiobutton(tt)

## fuer Standardisierung:
rb105 <- tkradiobutton(tt)
rb106 <- tkradiobutton(tt)
rb107 <- tkradiobutton(tt)
rb108 <- tkradiobutton(tt)
rb109 <- tkradiobutton(tt)

rbValue10 <- tclVar("log")
rbValue11 <- tclVar("classical")

tkconfigure(rb100,variable=rbValue10,value="nonetrans")
tkconfigure(rb101,variable=rbValue10,value="log")
tkconfigure(rb102,variable=rbValue10,value="bcOpt")
tkconfigure(rb105,variable=rbValue11,value="nonescale")
tkconfigure(rb106,variable=rbValue11,value="classical")
tkconfigure(rb107,variable=rbValue11,value="robust1")
#tkconfigure(rb108,variable=rbValue11,value="robust2")

tkgrid(tklabel(tt,text=""))
tkgrid(tklabel(tt,text="Transformation", font=fontHeading), tklabel(tt,text=" "),tklabel(tt, text="Scaling", font=fontHeading))
tkgrid(tklabel(tt,text="None "),rb100, tklabel(tt,text="None "),rb105,sticky="w" )
tkgrid(tklabel(tt,text="Log "),rb101, tklabel(tt,text="Classical "),rb106,sticky="w" )
tkgrid(tklabel(tt,text="Box-Cox "),rb102, tklabel(tt,text="Robust (Median, MAD) "),rb107,sticky="w" )
#tkgrid(tklabel(tt,text=" "), tklabel(tt,text="Robust (downweighting) "),rb108,sticky="w" )


######################################################

rb1 <- tkradiobutton(tt)
rb2 <- tkradiobutton(tt)
rb3 <- tkradiobutton(tt)
rb4 <- tkradiobutton(tt)
rb5 <- tkradiobutton(tt)
rb6 <- tkradiobutton(tt)
rb7 <- tkradiobutton(tt)
rb8 <- tkradiobutton(tt)
rb9 <- tkradiobutton(tt)
rb10 <- tkradiobutton(tt)
rb11 <- tkradiobutton(tt)
rb12 <- tkradiobutton(tt)
rb13 <- tkradiobutton(tt)
rb14 <- tkradiobutton(tt)
rb15 <- tkradiobutton(tt)
rb16 <- tkradiobutton(tt)
rb17 <- tkradiobutton(tt)
rb18 <- tkradiobutton(tt)
rb19 <- tkradiobutton(tt)
rb20 <- tkradiobutton(tt)
rb21 <- tkradiobutton(tt)
rb22 <- tkradiobutton(tt)
rb23 <- tkradiobutton(tt)
rb24 <- tkradiobutton(tt)
rb25 <- tkradiobutton(tt)
rb26 <- tkradiobutton(tt)
rb27 <- tkradiobutton(tt)
rb28 <- tkradiobutton(tt)
rb29 <- tkradiobutton(tt)
rb30 <- tkradiobutton(tt)
rb31 <- tkradiobutton(tt)
rb32 <- tkradiobutton(tt)
rb33 <- tkradiobutton(tt)
rb34 <- tkradiobutton(tt)
rb35 <- tkradiobutton(tt)
rb36 <- tkradiobutton(tt)
rb37 <- tkradiobutton(tt)

rbValue1 <- tclVar("kmeansHartigan")
rbValue2 <- tclVar("euclidean")
rbValue3 <- tclVar("2")
rbValue4 <- tclVar("silwidths")

Name <- tclVar("9")
entry.Name <-tkentry(tt,width="2",textvariable=Name)


tkconfigure(rb1,variable=rbValue1,value="kmeansHartigan")
tkconfigure(rb2,variable=rbValue1,value="clara")
tkconfigure(rb3,variable=rbValue1,value="bclust")
tkconfigure(rb4,variable=rbValue1,value="Mclust")
tkconfigure(rb5,variable=rbValue1,value="kccaKmeans")
tkconfigure(rb6,variable=rbValue1,value="speccPolydot")
tkconfigure(rb7,variable=rbValue1,value="cclustNeuralgas")
tkconfigure(rb8,variable=rbValue1,value="cmeans")
tkconfigure(rb9,variable=rbValue1,value="hclustAverage")
tkconfigure(rb10,variable=rbValue1,value="kccaKmedians")
tkconfigure(rb11,variable=rbValue2,value="euclidean")
tkconfigure(rb12,variable=rbValue2,value="manhattan")
tkconfigure(rb13,variable=rbValue2,value="rf")
tkconfigure(rb14,variable=rbValue2,value="bray")
tkconfigure(rb15,variable=rbValue2,value="gower")
##tkconfigure(rb16,variable=rbValue2,value="chord")
##tkconfigure(rb17,variable=rbValue2,value="horn")
tkconfigure(rb18,variable=rbValue2,value="kulczynski")
tkconfigure(rb19,variable=rbValue2,value="morisita")
tkconfigure(rb36,variable=rbValue2,value="correlation")
tkconfigure(rb37,variable=rbValue2,value="robustCorrelation")
tkconfigure(rb35,variable=rbValue2,value="none")
tkconfigure(rb20,variable=rbValue3,value="2")
tkconfigure(rb21,variable=rbValue3,value="3")
tkconfigure(rb22,variable=rbValue3,value="4")
tkconfigure(rb23,variable=rbValue3,value="5")
tkconfigure(rb24,variable=rbValue3,value="6")
tkconfigure(rb25,variable=rbValue3,value="7")
tkconfigure(rb26,variable=rbValue3,value="8")
tkconfigure(rb27,variable=rbValue3,value="9")
tkconfigure(rb28,variable=rbValue4,value="silwidths")
tkconfigure(rb29,variable=rbValue4,value="diameter")
tkconfigure(rb30,variable=rbValue4,value="average.distance")
tkconfigure(rb31,variable=rbValue4,value="median.distance")
tkconfigure(rb33,variable=rbValue4,value="average.toother")
tkconfigure(rb32,variable=rbValue4,value="separation")
tkconfigure(rb34,variable=rbValue4,value="bic")



tkgrid(tklabel(tt,text=""))

tkgrid(tklabel(tt,text=""))
tkgrid(tklabel(tt,text="Distance measure", font=fontHeading, background="white"),
       tklabel(tt,text="                        "), tklabel(tt,text="Clustering algorithm", font=fontHeading, background="white"),sticky="w")
tkgrid(tklabel(tt,text="Euclidean "),rb11, tklabel(tt,text="kmeansHartigan "),rb1,sticky="w" )
tkgrid(tklabel(tt,text="Manhattan "),rb12, tklabel(tt,text="clara "),rb2,sticky="w" )
tkgrid(tklabel(tt,text="Random Forest "),rb13,  tklabel(tt,text="bclust "),rb3,sticky="w")
tkgrid(tklabel(tt,text="Bray "),rb14,  tklabel(tt,text="Mclust "),rb4,sticky="w")
tkgrid(tklabel(tt,text="Gower "),rb15,  tklabel(tt,text="kccaKmeans "),rb5,sticky="w")
tkgrid(tklabel(tt,text="Kulczynski "),rb18,  tklabel(tt,text="speccPolydot "),rb6,sticky="w")
tkgrid(tklabel(tt,text="morisita "),rb19,  tklabel(tt,text="cclustNeuralgas "),rb7,sticky="w")
tkgrid(tklabel(tt,text="correlation distance "),rb36,  tklabel(tt,text="cmeans "),rb8,sticky="w")
#tkgrid(tklabel(tt,text="robust correlation distance "),rb37,  tklabel(tt,text="hclustAverage "),rb9,sticky="w")
tkgrid(tklabel(tt,text="none for Mclust "),rb35,  tklabel(tt,text="kccaKmedians "),rb10,sticky="w")
tkgrid(tklabel(tt,text=""))
#tkgrid(tklabel(tt,text="Choose a distance measure", font=fontHeading))
#tkgrid(tklabel(tt,text="euclidean "),rb11)
#tkgrid(tklabel(tt,text="manhattan "),rb12)
#tkgrid(tklabel(tt,text="randomForest "),rb13)
#tkgrid(tklabel(tt,text="bray "),rb14)
#tkgrid(tklabel(tt,text="gower "),rb15)
###tkgrid(tklabel(tt,text="chord "),rb16)
###tkgrid(tklabel(tt,text="horn "),rb17)
#tkgrid(tklabel(tt,text="kulczynski "),rb18)
#tkgrid(tklabel(tt,text="morisita "),rb19)
#tkgrid(tklabel(tt,text="correlation distance "),rb36)
#tkgrid(tklabel(tt,text="robust correlation distance "),rb37)
#tkgrid(tklabel(tt,text="none for Mclust "),rb35)
tkgrid(tklabel(tt,text=""))


tkgrid(tklabel(tt,text="Local validity measure", font=fontHeading, background="white"),        tklabel(tt,text="                        "),
tklabel(tt,text="Number of clusters:", font=fontHeading, background="white"))


OnOK <- function()
{
    rbVal1 <- as.character(tclvalue(rbValue1))
    if(rbVal1 == "Mclust"){
      rbValue2 <- tclVar("none")
    }
    rbVal2 <- as.character(tclvalue(rbValue2))
    rbVal3 <- as.character(tclvalue(rbValue3))
    rbVal4 <- as.character(tclvalue(rbValue4))
    rbVal10 <- as.character(tclvalue(rbValue10))
    rbVal11 <- as.character(tclvalue(rbValue11))    
    	NameVal <- tclvalue(Name)
    #tkdestroy(tt)
    X11()
    cat("\n --------- \n Please, wait a moment \n")
    clust1 <- clust(x=prepare(subset(ActiveDataSet(), select=varSelection), transformation=rbVal10, scaling=rbVal11), k=as.numeric(NameVal), method=rbVal1, distMethod=rbVal2)   #NameVal statt rbVal3
    clust1$scaling = rbVal11
    clust1$trans = rbVal10
    #save(clust1, file="clust1.RData")#, envir=.GlobalEnv)#, envir=.GlobalEnv)
    #load("clust1.RData")
    assign("clust1", clust1, envir=.GlobalEnv)
    clustPlot(coord=subset(ActiveDataSet(), select=coordNames), clust=clust1, k=as.numeric(NameVal), val=rbVal4)           #NameVal statt rbVal3
    print.clust(clust1)
}


#tkgrid(tklabel(tt,text="Number of clusters:", font=fontHeading, background="white"),
#       tklabel(tt,text="                        "), tklabel(tt,text="Local validity measure", font=fontHeading, background="white"))
tkgrid(tklabel(tt,text="silwidths"),rb28, entry.Name, sticky="w")
#tkgrid(tklabel(tt,text="2 "),rb20)
#tkgrid(tklabel(tt,text="3 "),rb21)
#tkgrid(tklabel(tt,text="4 "),rb22)
#tkgrid(tklabel(tt,text="5 "),rb23)
#tkgrid(tklabel(tt,text="6 "),rb24)
#tkgrid(tklabel(tt,text="7 "),rb25)
#tkgrid(tklabel(tt,text="8 "),rb26)
#tkgrid(tklabel(tt,text="9 "),rb27)
#tkgrid(tklabel(tt,text=""))
#tkgrid(tklabel(tt,text="Local validity measure", font=fontHeading, background="white"))
#tkgrid(tklabel(tt,text="silwidths"),rb28,sticky="w")
tkgrid(tklabel(tt,text="diameter"),rb29,sticky="w")
tkgrid(tklabel(tt,text="average distance"),rb30,sticky="w")
OK.but <- tkbutton(tt,text="SHOW PLOTS",command=OnOK)
tkbind(entry.Name, "<Return>",OnOK)
tkgrid(tklabel(tt,text="median distance"),rb31,  OK.but, sticky="w")
a1 <- tclvalue(Name)
tkgrid(tklabel(tt,text="separation"),rb32,sticky="w")
tkgrid(tklabel(tt,text="average.toother"),rb33,sticky="w")
tkgrid(tklabel(tt,text="BIC"),rb34, sticky="w")
tkgrid(tklabel(tt, text="      "), tklabel(tt, text="      "), tklabel(tt, text="Copyright Templ 2006") )

OnOK <- function()
{
    rbVal1 <- as.character(tclvalue(rbValue1))
    if(rbVal1 == "Mclust"){
      rbValue2 <- tclVar("none")
    }
    rbVal2 <- as.character(tclvalue(rbValue2))
    rbVal3 <- as.character(tclvalue(rbValue3))
    rbVal4 <- as.character(tclvalue(rbValue4))
    rbVal10 <- as.character(tclvalue(rbValue10))
    rbVal11 <- as.character(tclvalue(rbValue11))    
    	NameVal <- tclvalue(Name)
    #tkdestroy(tt)
    X11()
    cat("\n --------- \n Please, wait a moment \n")
    clust1 <- clust(x=prepare(subset(ActiveDataSet(), select=varSelection), transformation=rbVal10, scaling=rbVal11), k=as.numeric(NameVal), method=rbVal1, distMethod=rbVal2)   #NameVal statt rbVal3
    clust1$scaling = rbVal11
    clust1$trans = rbVal10
    save(clust1, file="clust1.RData")#, envir=.GlobalEnv)
    #load("clust1.RData")
    clustPlot(coord=subset(ActiveDataSet(), select=coordNames), clust=clust1, k=as.numeric(NameVal), val=rbVal4)           #NameVal statt rbVal3
    print.clust(clust1)
}
OK.but <- tkbutton(tt,text="SHOW PLOTS",command=OnOK)
tkbind(entry.Name, "<Return>",OnOK)
#tkgrid(tklabel(tt, text="Copyright Templ 2006"), tklabel(tt, text="      "), OK.but, sticky="w")
a1 <- tclvalue(Name)
tkfocus(tt)

}

