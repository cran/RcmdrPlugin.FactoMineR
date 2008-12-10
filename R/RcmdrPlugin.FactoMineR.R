#! Ensemble des interfaces graphiques associées au package FactoMineR

################################################################################
################################# FONCTION DEFMACRO ############################
#           utilisée indirectement pour la construction des GUI                #
################################################################################
# the Rcmdr if it is not already loaded
# Some Rcmdr dialogs for the TeachingDemos package

# last modified: 30 May 2007 by J. Fox

# Note: the following function (with contributions from Richard Heiberger) 
# can be included in any Rcmdr plug-in package to cause the package to load
# the Rcmdr if it is not already loaded
.packageName <- "RcmdrPlugin.FactoMineR"

.First.lib <- function(libname, pkgname){
    if (!interactive()) return()
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
    if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
        }
    }

#! Ensemble des interfaces graphiques associées au package FactoMineR

################################################################################
################################# FONCTION DEFMACRO ############################
#           utilisée indirectement pour la construction des GUI                #
################################################################################
  defmacro <- function(..., expr){
    expr <- substitute(expr)
    len <- length(expr)
    expr[3:(len+1)] <- expr[2:len]
    ## delete "macro" variables starting in ..
    expr[[2]] <- quote(on.exit(remove(list=objects(pattern="^\\.\\.", all.names=TRUE))))
    a <- substitute(list(...))[-1]
    ## process the argument list
    nn <- names(a)
    if (is.null(nn)) nn <- rep("", length(a))
    for (i in seq(length=length(a))){
        if (nn[i] == "") {
            nn[i] <- paste(a[[i]])
            msg <- paste(a[[i]], gettext("not supplied", domain="R-Rcmdr"))
            a[[i]] <- substitute(stop(foo), list(foo = msg))
            }
        }
    names(a) <- nn
    a <- as.list(a)
    ff <- eval(substitute(
        function(){
            tmp <- substitute(body)
            eval(tmp, parent.frame())
            },
        list(body = expr)))
    ## add the argument list
    formals(ff) <- a
    ## create a fake source attribute
    mm <- match.call()
    mm$expr <- NULL
    mm[[1]] <- as.name("macro")
    expr[[2]] <- NULL # get "local" variable removal out of source
    attr(ff, "source") <- c(deparse(mm), deparse(expr))
    ## return the macro
    ff
    }

#############################FIN FONCTION DEFMACRO #############################

###############

readDataSetFacto <- function() {
    initializeDialog(title=gettextRcmdr("Read Text Data From File, Clipboard, or URL"))
    optionsFrame <- tkframe(top)
    dsname <- tclVar(gettextRcmdr("Dataset"))
    entryDsname <- ttkentry(optionsFrame, width="20", textvariable=dsname)
    radioButtons(optionsFrame, "location", buttons=c("local", "clipboard", "url"),
        labels=gettextRcmdr(c("Local file system", "Clipboard", "Internet URL")), title=gettextRcmdr("Location of Data File"))
    headerVariable <- tclVar("1")
    nameRows <- tclVar("0")
    headerCheckBox <- tkcheckbutton(optionsFrame, variable=headerVariable)
    rowCheckBox <- tkcheckbutton(optionsFrame, variable=nameRows)
 ##   clipboardVariable <- tclVar("0")
 ##   clipboardCheckBox <- tkcheckbutton(optionsFrame, variable=clipboardVariable)
    radioButtons(optionsFrame, "delimiter", buttons=c("whitespace", "commas", "tabs"),
        labels=gettextRcmdr(c("White space", "Commas", "Tabs")), title=gettextRcmdr("Field Separator"))
    otherButton <- ttkradiobutton(delimiterFrame, variable=delimiterVariable, value="other")
    otherVariable <- tclVar("")
    otherEntry <- ttkentry(delimiterFrame, width="4", textvariable=otherVariable)
    radioButtons(optionsFrame, "decimal", buttons=c("period", "comma"),
        labels=gettextRcmdr(c("Period [.]", "Comma [,]")), title=gettextRcmdr("Decimal-Point Character"))
    missingVariable <- tclVar("NA")
    missingEntry <- ttkentry(optionsFrame, width="8", textvariable=missingVariable)
    onOK <- function(){
        closeDialog()
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            errorCondition(recall=readDataSet,
                message=gettextRcmdr("You must enter a name for the data set."))
                return()
                }
        if (!is.valid.name(dsnameValue)){
            errorCondition(recall=readDataSet,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                readDataSet()
                return()
                }
            }
##        clip <- tclvalue(clipboardVariable) == "1"
        location <- tclvalue(locationVariable)
        file <- if (location == "clipboard") "clipboard"
            else if (location == "local") tclvalue(tkgetOpenFile(filetypes=
                gettextRcmdr('{"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}} {"All Files" {"*"}}')))
            else {
                initializeDialog(subdialog, title=gettextRcmdr("Internet URL"))
                onOKsub <- function(){
                    closeDialog(subdialog)
                    }
                urlFrame <- tkframe(subdialog)
                urlVar <- tclVar("")
                url <- ttkentry(urlFrame, font=getRcmdr("logFont"), width="30", textvariable=urlVar)
                urlXscroll <- ttkscrollbar(urlFrame,
                        orient="horizontal", command=function(...) tkxview(url, ...))
                tkconfigure(url, xscrollcommand=function(...) tkset(urlXscroll, ...))
                subOKCancelHelp()
                tkgrid(url, sticky="w")
                tkgrid(urlXscroll, sticky="ew")
                tkgrid(urlFrame, sticky="nw")
                tkgrid(subButtonsFrame, sticky="w")
                dialogSuffix(subdialog, rows=2, columns=1, focus=url, onOK=onOKsub)
                tclvalue(urlVar)
                }
        if (file == "") {
            if (getRcmdr("grab.focus")) tkgrab.release(top)
            tkdestroy(top)
            return()
            }
        head <- tclvalue(headerVariable) == "1"
        row <- tclvalue(nameRows) == "1"
        delimiter <- tclvalue(delimiterVariable)
        del <- if (delimiter == "whitespace") ""
            else if (delimiter == "commas") ","
            else if (delimiter == "tabs") "\\t"
            else tclvalue(otherVariable)
        miss <- tclvalue(missingVariable)
        dec <- if (tclvalue(decimalVariable) == "period") "." else ","
        if (row) command <- paste('read.table("', file,'", header=', head,
            ', sep="', del, '", na.strings="', miss, '", dec="', dec, '", row.names=1, strip.white=TRUE)', sep="")
        else command <- paste('read.table("', file,'", header=', head,
            ', sep="', del, '", na.strings="', miss, '", dec="', dec, '", strip.white=TRUE)', sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        result <- justDoIt(command)
        if (class(result)[1] !=  "try-error"){
            assign(dsnameValue, result, envir=.GlobalEnv)
            activeDataSet(dsnameValue)
            }
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="read.table")
    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, sticky="w")
    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Variable names in file:")), headerCheckBox, sticky="w")
    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Row names in the first columns:")), rowCheckBox, sticky="w")
##    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Read data from clipboard:")), clipboardCheckBox, sticky="w")
    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Missing data indicator:")), missingEntry, sticky="w")
    tkgrid(locationFrame, sticky="w")
    tkgrid(labelRcmdr(delimiterFrame, text=gettextRcmdr("Other")), otherButton,
        labelRcmdr(delimiterFrame, text=gettextRcmdr("  Specify:")), otherEntry, sticky="w")
    tkgrid(delimiterFrame, sticky="w", columnspan=2)
    tkgrid(decimalFrame, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=5, columns=1)
    }

###################### Fin importation

#! version retravaillé par JMA le 14/12/2006 08:58:52


FactoPCA<-function()                                                                                        #FactoPCA function which will ba called by OK button of the PCA window
{
  require(tcltk)                                                                                            #Loading of package tcltk
  require(FactoMineR)                                                                                       #Loading of package FactoMineR


#    Création des fonctions pour les options via nouvelle fenêtre graphique


  #! fonction pour le choix des variables qualitatives supplémentaires
  Fillu.funct<-defmacro(label, firstLabel, expr=
  {                                                                                                         #function to choose supplementary categorical variables
    env<-environment()
    variablefact<-NULL                                                                                      #creation of an empty object: variablefact
    .FilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    .factors<-Factors()
    OnFillu<-function()                                                                                     #function launched by clicking on button "select supplementary factors"
    {
      if(length(.factors)==0) errorCondition(recall=NULL, message=gettextRcmdr("No Factor available"))      #message if no factor in the dataset

      FilluWin<-tktoplevel()                                                                                #creation of a new tcltk window
      tkwm.title(FilluWin,gettextRcmdr("Choice of supplementary factors"))                                  #title of the window
      #création de la fonction FOK.funct
      FOK.funct<-function()                                                                                 #function launched by clicking on OK after having chosen sup fact
      {
        fact.select<-listfact.nom[as.numeric(tkcurselection(listfact))+1]                                   #creation of a list box with factors
        if(length(fact.select)==0) {                                                                        #if nothing selected, no factor affected to variablefact and the window is closed
          assign("variablefact", NULL, envir=env)
          tclvalue(.FilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Fillu.but, fg="black")
          tkdestroy(FilluWin)
          return()
        }
        assign("variablefact", fact.select, envir=env)                                                      #else, selected factors are affected to variablefact then the window is closed
        tclvalue(.FilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Fillu.but, fg="blue")                                                                   #button "select sup fact" becomes blue
        tkdestroy(FilluWin)
      }

      # création et mise en page de la fenetre Fillu
      listfact<-tklistbox(FilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrfact,...)) #empty list box
      scrfact <-tkscrollbar(FilluWin,repeatinterval=5,command=function(...)tkyview(listfact,...))                                #scroll bar
      listfact.nom<-NULL
      indice<-0
      for (i in (1:ncol(donnee))) {
        if (is.factor(donnee[,i])) {                                                                                             #we're interested only in categorical variables
          tkinsert(listfact,"end",vars[i])                                                                                       #list box is completed with all available factors
          listfact.nom<-c(listfact.nom,vars[i])
          if(vars[i] %in% variablefact) tkselection.set(listfact, indice)                                                        #factors' selection
          indice<-indice+1
        }
      }

      FOK.but<-tkbutton(FilluWin, text="OK", width=16,command=FOK.funct)                                                         #OK button to launch the function above
                                                                                                                                                      #page setting
      tkgrid(tklabel(FilluWin, text=""))                                                                                                              #blank line
        tkgrid(tklabel(FilluWin, text = gettextRcmdr("Select supplementary factor(s)"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")        #text
        tkgrid(listfact, scrfact, sticky = "nw")                                                                                                      #list box and scroll bar side by side
        tkgrid.configure(scrfact, sticky = "ens", columnspan=1)                                                                                       #scroll bar's configuration
        tkgrid.configure(listfact, sticky = "ew", column=1, columnspan=1)                                                                             #list box's config
        tkgrid(tklabel(FilluWin, text=""))                                                                                                            #blank line
        tkgrid(FOK.but, column=1,columnspan=1, sticky="ew")                                                                                           #OK button
        tkgrid(tklabel(FilluWin, text=""))                                                                                                            #blank line
        tkgrid.columnconfigure(FilluWin,0, minsize=25)                                                                                                #window configuration: column nb 0 with size 25
      tkgrid.columnconfigure(FilluWin,2, minsize=25)                                                                                                  #window configuration: column nb 2 with size 25
  }

   FilluFrame<-tkframe(IlluFrame)                                                                                                                     #new frame of main window to put "select sup fact" button
   if(length(.factors)==0){                                                                                                                           #if no factors in the dataset, grey button with message
     Fillu.but<-tkbutton(FilluFrame, text=gettextRcmdr("No factors available"), borderwidth=3)
     tkconfigure(Fillu.but, fg="grey")
   }
   else Fillu.but<-tkbutton(FilluFrame, textvariable=.FilluLabel, command=OnFillu, borderwidth=3)                                                     #else, function OnFillu is launched
   tkgrid(Fillu.but, sticky="ew")                                                                                                                     #setting of this button in the new frame

##   Fillu.but<-tkbutton(FilluFrame, textvariable=.FilluLabel, command=OnFillu, borderwidth=3)
##   tkgrid(Fillu.but, sticky="ew")
  })

  #! fonction pour le choix des variables quantitatives supplémentaires
  Dillu.funct<-defmacro(label, firstLabel, expr=                                                                                         #function to choose supplementary continuous variables
  {
    env<-environment()
    variableillu<-NULL                                                                                                                   #creation of an empty object: variableillu
    .DilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnDillu<-function()                                                                                                                  #function launched by clicking on "select sup variables"
    {
      DilluWin<-tktoplevel()                                                                                                             #creation of a new tcltk window
      tkwm.title(DilluWin,gettextRcmdr("Select supplementary variables"))                                                                #title of the window
      #création de la fonction DOK.funct
      DOK.funct<-function()                                                                                                              #function launched by clicking on OK after habing chosen sup cont variables
      {
        vsup.select<-listvar.nom[as.numeric(tkcurselection(listvar))+1]                                                                  #list box
        if(length(vsup.select)==0)                                                                                                       #if nothing selected, no variable affected to variableillu and the window is closed
        {
          assign("variableillu", NULL, envir=env)
          tclvalue(.DilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Dillu.but, fg="black")
          tkdestroy(DilluWin)
          return()
        }
        assign("variableillu", vsup.select, envir=env)                                                                                   #else, chosen variables are affected to variableillu and then the window is closed
        tclvalue(.DilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Dillu.but, fg="blue")                                                                                                #"select sup var" button  becomes blue
        tkdestroy(DilluWin)
      }

      # création et mise en page de la fenetre Dillu
      listvar<-tklistbox(DilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrvar,...))            #empty list box
      scrvar <-tkscrollbar(DilluWin,repeatinterval=5,command=function(...)tkyview(listvar,...))                                           #scroll bar
      listvar.nom<-NULL
      indice<-0
      for (i in (1:ncol(donnee))) {
          if (is.numeric(donnee[,i])) {                                                                                                   #we're interested only in numerical variables
            tkinsert(listvar,"end",vars[i])                                                                                               #continuous variables are put in list box
            listvar.nom<-c(listvar.nom,vars[i])
            if(vars[i] %in% variableillu) tkselection.set(listvar, indice)                                                                #selected cont var
            indice<-indice+1
          }
      }

      DOK.but<-tkbutton(DilluWin, text="OK", width=16,command=DOK.funct)                                                                  #OK button to launch the function above

        tkgrid(tklabel(DilluWin, text=""))                                                                                                           #page setting: blank line
        tkgrid(tklabel(DilluWin, text = gettextRcmdr("Select supplementary variable(s)"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")     #text
        tkgrid(listvar, scrvar, sticky = "nw")                                                                                                       #list box and scroll bar side by side
        tkgrid.configure(scrvar, sticky = "ens", columnspan=1)                                                                                       #scroll bar's configuration
        tkgrid.configure(listvar, sticky = "ew", column=1, columnspan=1)                                                                             #list box's configuration
        tkgrid(tklabel(DilluWin, text=""))                                                                                                           #blank line
        tkgrid(DOK.but, column=1,columnspan=1, sticky="ew")                                                                                          #OK button
        tkgrid(tklabel(DilluWin, text=""))                                                                                                           #blank line
        tkgrid.columnconfigure(DilluWin,0, minsize=25)                                                                                               #window configuration: column 0 will be of size 25
        tkgrid.columnconfigure(DilluWin,2, minsize=25)                                                                                               #window configuration: column 2 will be of size 25
  }

   DilluFrame<-tkframe(IlluFrame)                                                                                                                    #new frame on the main window to put "select sup var" button
   Dillu.but<-tkbutton(DilluFrame, textvariable=.DilluLabel, command=OnDillu, borderwidth=3)                                                         #"select sup var" button
   tkgrid(Dillu.but, sticky="ew")                                                                                                                    #"select sup var" button put in its frame
  })


  #! fonction pour le choix des individus supplémentaires
  Iillu.funct<-defmacro(label, firstLabel, expr=                                                                                  #function to choose sup individuals
  {
    env<-environment()
    individuillu<-NULL                                                                                                            #creation of an empty object: individuillu
    .IilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnIillu<-function()                                                                                                           #function launched by clicking on "select sup ind"
    {
      IilluWin<-tktoplevel()                                                                                                      #creation of a new tcltk window
      tkwm.title(IilluWin,gettextRcmdr("Select supplementary individuals"))                                                       #title of the window
      #création de la fonction IOK.funct
      IOK.funct<-function()                                                                                                       #function to select sup ind
      {
        ind.select<-rows[as.numeric(tkcurselection(listind))+1]                                                                   #list box
        if(length(ind.select)==0)                                                                                                 #if no ind selected, nothing put in individuillu and the window is closed
        {
          assign("individuillu", NULL, envir=env)
          tclvalue(.IilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Iillu.but, fg="black")
          tkdestroy(IilluWin)
          return()
        }
        assign("individuillu", ind.select, envir=env)                                                                             #else selected ind are put in individuillu then the window is closed
#        tclvalue(.IilluLabel)<-paste(label, ": OK", sep=" ")
        tclvalue(.IilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Iillu.but, fg="blue")                                                                                         #"select sup ind" button becomes blue
        tkdestroy(IilluWin)
      }

      # création et mise en page de la fenetre Fillu
      listind<-tklistbox(IilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrind,...))    #empty list box
      scrind <-tkscrollbar(IilluWin,repeatinterval=5,command=function(...)tkyview(listind,...))                                   #scroll bar
      indice<-0
      for (i in (1:nrow(donnee))) {
          tkinsert(listind,"end",rows[i]) # On renseigne la liste                                                                 #list box is completed with individuals
          if(rows[i] %in% individuillu) tkselection.set(listind,indice)                                                           #selected individuals
          indice<-indice+1
      }

      IOK.but<-tkbutton(IilluWin, text="OK", width=16,command=IOK.funct)                                                          #OK button to launch the function above

      tkgrid(tklabel(IilluWin, text=""))                                                                                                             #page setting: blank line
      tkgrid(tklabel(IilluWin, text = gettextRcmdr("Select supplementary individual(s)"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")     #text
      tkgrid(listind, scrind, sticky = "nw")                                                                                                         #list box and scroll bar side by side
      tkgrid.configure(scrind, sticky = "ens", columnspan=1)                                                                                         #scroll bar's config
      tkgrid.configure(listind, sticky = "ew", column=1, columnspan=1)                                                                               #list box's config
      tkgrid(tklabel(IilluWin, text=""))                                                                                                             #blank line
      tkgrid(IOK.but, column=1,columnspan=1, sticky="ew")                                                                                            #OK button
      tkgrid(tklabel(IilluWin, text=""))                                                                                                             #blank line
      tkgrid.columnconfigure(IilluWin,0, minsize=25)                                                                                                 #configuration of column nb 0: size 25
      tkgrid.columnconfigure(IilluWin,2, minsize=25)                                                                                                 #configuration of column nb 2: size 25
  }

   IilluFrame<-tkframe(IlluFrame)                                                                                                                    #new frame in the main window
   Iillu.but<-tkbutton(IilluFrame, textvariable=.IilluLabel, command=OnIillu, borderwidth=3)                                                         #creation of "select sup ind" button
   tkgrid(Iillu.but, sticky="ew")                                                                                                                    #new button put in new frame
  })


  #! fonction pour la gestion des options graphiques
  PLOT.PCA<-defmacro(label, firstLabel, expr=                                      #function to choose graphical options
  {
    env<-environment()
    compteur.graph<-0                                                              #variables initialization
    #déclaration des variables
    Rchoix<-TRUE                                                                   #plotting graph of individuals
    RTitle<-NULL                                                                   #title for graph of ind
    Rinvisible<-NULL                                                               #invisible elements for graph of ind
    Rlabel<-c("ind", "ind.sup", "quali")                                           #different lables for graph of ind
    Rcol.ind<-Rcol.ind.tmp<-"black"                                                #color for ind
    Rcol.ind.sup<-Rcol.ind.sup.tmp<-"blue"                                         #color for sup ind
    Rcol.quali<-Rcol.quali.tmp<-"magenta"                                          #color for sup fact
    Rhabillage<-"none"                                                             #color according...
    RXlimInd<-NULL                                                                 #limit for x axes
    RYlimInd<-NULL                                                                 #limit for y axes

    Wchoix=TRUE                                                                    #plotting graph of variables
    WTitle<-NULL                                                                   #title of graph of var
    Wlabel<-c("var", "quanti.sup")                                                 #labels
    Wlim.cos<-0.                                                                   #selection on cos2
    Wcol.quanti.sup<-Wcol.quanti.sup.tmp<-"blue"                                   #color for sup var
    Wcol.var<-Wcol.var.tmp<-"black"                                                #color for var
    WXlimVar<-NULL                                                                 #limit for x axes
    WYlimVar<-NULL                                                                 #limit for y axes

    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))

    OnPlot<-function()                                                                        #function to be launched by clicking on "graphical options" button
    {
      PlotWin<-tktoplevel()                                                                   #new tcltk window
      tkwm.title(PlotWin, gettextRcmdr("Graphical options"))                                  #title
      tkwm.geometry(PlotWin, "-100+50")                                                       #size

      #création de la fonction onOKsub
      onOKsub<-function()                                                                     #function launched by clicking on OK
      {
        assign("compteur.graph", compteur.graph+1, envir=env)                                 #after clicking on OK, "graph opt" button becomes blue
#        if(compteur.graph>0) tclvalue(.PlotLabel)<-paste(label, ": Seen", sep=" ")
        if(compteur.graph>0) tclvalue(.PlotLabel)<-paste(label, "", sep=" ")
        tkconfigure(Plot.but, fg="blue")

        # gestion des entrées de la partie graphique des individus
        if(tclvalue(ind.check.value)==1) assign("Rchoix", TRUE, envir=env)                    #if the user has chosen to plot the graph of individuals, TRUE is affected to Rchoix, else FALSE is affected
        else assign("Rchoix", FALSE, envir=env)

        if(Rchoix)                                                                            #if Rchoix is TRUE
        {
          if (tclvalue(Titre)==" ") assign("RTitle", NULL, envir=env)                         #if the user did not write anything for the title, then RTitle becomes NULL, else it takes what the user wrote
          assign("RTitle", tclvalue(Titre), envir=env)

          label.tmp.ind<-tclvalue(label.ind.checkValue)
          label.tmp.ind.sup<-tclvalue(label.ind.sup.checkValue)
          label.tmp.quali.sup<-tclvalue(label.quali.sup.checkValue)
          assign("Rlabel", NULL, envir=env)                                                   #at first Rlabel is NULL, then it is completed with "ind", "ind.sup" and "quali" if the user has chosen to plot those labels
          if(label.tmp.ind==1) assign("Rlabel", c(Rlabel, "ind"), envir=env)
          if(label.tmp.ind.sup==1) assign("Rlabel", c(Rlabel, "ind.sup"), envir=env)
          if(label.tmp.quali.sup==1) assign("Rlabel", c(Rlabel, "quali"), envir=env)

          assign("Rcol.ind", Rcol.ind.tmp, envir=env)                                         #Rcol.ind takes the color the user has chosen for individuals
          assign("Rcol.ind.sup", Rcol.ind.sup.tmp, envir=env)                                 #Rcol.ind.sup takes the color the user has chosen for sup individuals
          assign("Rcol.quali", Rcol.quali.tmp, envir=env)                                     #Rcol.quali takes the color the user has chosen for sup factors

          inv.ind.tmp<-tclvalue(inv.ind.checkValue)
          inv.ind.sup.tmp<-tclvalue(inv.ind.sup.checkValue)
          inv.quali.tmp<-tclvalue(inv.quali.checkValue)
          assign("Rinvisible", NULL, envir=env)                                               #at first Rinvisible is NULL, then it is completed with "ind", "ind.sup" and "quali" if the user has chosen to hide those elements
          if(inv.ind.tmp=="1") assign("Rinvisible", c(Rinvisible, "ind"), envir=env)
          if(inv.ind.sup.tmp=="1") assign("Rinvisible", c(Rinvisible, "ind.sup"), envir=env)
          if(inv.quali.tmp=="1") assign("Rinvisible", c(Rinvisible, "quali"), envir=env)

          habillage.tmp<-listgraph.nom[as.numeric(tkcurselection(listgraph))+1]               #individuals are colored according to what the user chose
          if(length(habillage.tmp)==0) assign("Rhabillage","none", envir=env)                 #if nothing chosen, hbillage ="none"
          else assign("Rhabillage", habillage.tmp, envir=env)

          if(tclvalue(XlimIndMin)=="" | tclvalue(XlimIndMax)=="") assign("RXlimInd", NULL, envir=env)                   #when no choice for x limits, RXlimInd=NULL
          else assign("RXlimInd", c(as.numeric(tclvalue(XlimIndMin)), as.numeric(tclvalue(XlimIndMax))), envir=env)     #else RXlimInd is what the user chose
          if(tclvalue(YlimIndMin)=="" | tclvalue(YlimIndMax)=="") assign("RYlimInd", NULL, envir=env)                   #when no choice for y limits, RYlimInd=NULL
          else assign("RYlimInd", c(as.numeric(tclvalue(YlimIndMin)), as.numeric(tclvalue(YlimIndMax))), envir=env)     #else RYlimInd is what the user chose
        }


        # gestion des entrées de la partie graphique des variables
        if(tclvalue(var.check.value)==1) assign("Wchoix", TRUE, envir=env)                     #if the user has chosen to plot the graph of variables, TRUE is affected to Wchoix, else FALSE is affected
        else assign("Wchoix", FALSE, envir=env)

        if(Wchoix)                                                                             #if Wchoix=T
        {
          if (tclvalue(WTitre)==" ") assign("WTitle", NULL, envir=env)                         #if the user did not write anything for the title, then WTitle becomes NULL, else it takes what the user wrote
          assign("WTitle", tclvalue(WTitre), envir=env)

          assign("Wlim.cos", tclvalue(WlimCosValue), envir=env)                                #Wlim.cos takes the value the user chose

          label.tmp.var<-tclvalue(label.var.checkValue)
          label.tmp.quanti.sup<-tclvalue(label.quanti.sup.checkValue)
          assign("Wlabel", NULL, envir=env)                                                    #at first Wlabel is NULL, then it is completed with "var" and "quanti.sup" if the user has chosen to plot those labels
          if(label.tmp.var==1) assign("Wlabel", c(Wlabel, "var"), envir=env)
          if(label.tmp.quanti.sup==1) assign("Wlabel", c(Wlabel, "quanti.sup"), envir=env)

          assign("Wcol.var", Wcol.var.tmp, envir=env)                                          #Wcol.var takes the color the user has chosen for active variables
          assign("Wcol.quanti.sup", Wcol.quanti.sup.tmp, envir=env)                            #Wcol.quanti.sup takes the color the user has chosen for supplementary variables

        }

        tkdestroy(PlotWin)                                                                     #window is closed
      }

    # construction de la partie graphique des individus
    PlotIndFrame<-tkframe(PlotWin, borderwidth=5, relief="groove")                             #new frame for all the choices concerning graph of individuals

    RchoixFrame<-tkframe(PlotIndFrame,borderwidth=2)                                           #frame for the button to choose whether or not to plot the graph of individuals
    ind.check<-tkcheckbutton(RchoixFrame)                                                      #creation of a check button
    if(Rchoix) ind.check.value<-tclVar("1")                                                    #ind.check.value=1 if Rchoix=T
    else ind.check.value<-tclVar("0")                                                          #else ind.check.value=0
    tkconfigure(ind.check, variable=ind.check.value)                                           #assigning variable to check button: will appear as clicked  or unclicked according to ind.check.value (1 or 0)
    tkgrid(tklabel(RchoixFrame, text=gettextRcmdr("Plot individuals graph"), font=font2),ind.check)       #positionning text and check button side by side
    tkgrid(tklabel(RchoixFrame, text="  "))                                                               #blank line

    RTitleFrame<-tkframe(PlotIndFrame,borderwidth=2)                                            #frame for the title
    if (is.null(RTitle)) Titre <- tclVar(" ")                                                   #default value is nothing
    else Titre<-tclVar(RTitle)                                                                  #if the user writes sth, it becomes the value of RTitle
    Titre.entry <-tkentry(RTitleFrame,width="40",textvariable=Titre)                            #case to enter the title
    tkgrid(tklabel(RTitleFrame,text=gettextRcmdr("Title of the graph")),Titre.entry)            #positionning text and case side by side

    RinvisibleFrame<-tkframe(PlotIndFrame,borderwidth=2)                                        #frame for invisible elements
    inv.ind.check<-tkcheckbutton(RinvisibleFrame)                                               #check button for individuals
    if ("ind" %in% Rinvisible) inv.ind.checkValue<-tclVar("1")                                  #inv.ind.checkValue=1 if "ind" is in Rinvisible
    else inv.ind.checkValue<-tclVar("0")
    inv.ind.sup.check<-tkcheckbutton(RinvisibleFrame)                                           #check button for sup individuals
    if ("ind.sup" %in% Rinvisible) inv.ind.sup.checkValue<-tclVar("1")                          #inv.ind.sup.checkValue=1 if "ind.sup" is in Rinvisible
    else inv.ind.sup.checkValue<-tclVar("0")
    inv.quali.check<-tkcheckbutton(RinvisibleFrame)                                             #check button for sup individuals
    if ("quali" %in% Rinvisible) inv.quali.checkValue<-tclVar("1")                              #inv.quali.checkValue<=1 if "quali" is in Rinvisible
    else inv.quali.checkValue<-tclVar("0")
    tkconfigure(inv.ind.check, variable=inv.ind.checkValue)                                     #assigning inv.ind.checkValue to check button, button will appear clicked or unclicked according to it
    tkconfigure(inv.ind.sup.check, variable=inv.ind.sup.checkValue)                             #assigning inv.ind.sup.checkValue to check button
    tkconfigure(inv.quali.check, variable=inv.quali.checkValue)                                 #assigning inv.ind.sup.checkValue to check button
    if (!is.null(variablefact)|!is.null(individuillu)){                                         #if there are supplementary factors or individuals
      tkgrid(tklabel(RinvisibleFrame, text=gettextRcmdr("Hide some elements:")), columnspan=6, sticky="w")           #positionning text
      tkgrid(tklabel(RinvisibleFrame, text="ind"),inv.ind.check, tklabel(RinvisibleFrame, text="ind sup"),inv.ind.sup.check, tklabel(RinvisibleFrame, text="quali"),inv.quali.check, sticky="w")       #and check buttons
    }

    RlabelFrame<-tkframe(PlotIndFrame,borderwidth=2)                                                 #frame for labels
    label.ind.check<-tkcheckbutton(RlabelFrame)                                                      #check button for labels of individuals
    if ("ind" %in% Rlabel) label.ind.checkValue<-tclVar("1")                                         #label.ind.checkValue=1 if "ind" is in Rlabel
    else label.ind.checkValue<-tclVar("0")
    tkconfigure(label.ind.check, variable=label.ind.checkValue)                                      #assigning label.ind.checkValue to the button
    tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Label for the active individuals")),label.ind.check)    #positionning text and button
    label.ind.sup.check<-tkcheckbutton(RlabelFrame)                                                        #check button for labels of sup individuals
    if ("ind.sup" %in% Rlabel) label.ind.sup.checkValue<-tclVar("1")                                       #label.ind.sup.checkValue=1 if "ind.sup" is in Rlabel
    else label.ind.sup.checkValue<-tclVar("0")
    tkconfigure(label.ind.sup.check, variable=label.ind.sup.checkValue)                                    #assigning label.ind.sup.heckValue to the button
    if(!is.null(individuillu)) tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Label for the supplementary individuals")),label.ind.sup.check) #positionning text and button
    label.quali.sup.check<-tkcheckbutton(RlabelFrame)                                                         #check button for labels of sup fact
    if ("quali" %in% Rlabel) label.quali.sup.checkValue<-tclVar("1")                                          #label.quali.sup.checkValue=1 if "ind.sup" is in Rlabel
    else label.quali.sup.checkValue<-tclVar("1")
    tkconfigure(label.quali.sup.check, variable=label.quali.sup.checkValue)                                   #assigning label.quali.sup.checkValue to the button
    if(!is.null(variablefact)) tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Label for the supplementary factor")), label.quali.sup.check) #positionning text and button

    RcolFrame<-tkframe(PlotIndFrame,borderwidth=2)                                                             #frame for choice of colors
    Rcol.ind.value <- Rcol.ind
    canvas.ind <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.ind.value)                                 #button to change color of individuals
    ChangeColor.ind <- function()                                                                              #function to change color of individuals
    {
      Rcol.ind.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.ind.value,title=gettextRcmdr("Choose a color")))   #assigning chosen color to Rcol.ind.value
      if (nchar(Rcol.ind.value)>0)
      {
        tkconfigure(canvas.ind,bg=Rcol.ind.value)                                                                        #canva's color becomes the chosen one
        assign("Rcol.ind.tmp", Rcol.ind.value, envir=env)                                                                #assigning Rcol.ind.value to Rcol.ind.tmp
      }
    }
    ChangeColor.ind.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.ind)              #button to launch function above
    tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color of the active individuals")),canvas.ind,ChangeColor.ind.button)   #settings: text, canva and button

    Rcol.ind.sup.value<-Rcol.ind.sup                                                                                     #same for supplementary individuals
    canvas.ind.sup <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.ind.sup.value)
    ChangeColor.ind.sup <- function()
    {
      Rcol.ind.sup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.ind.sup.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Rcol.ind.sup.value)>0)
      {
        tkconfigure(canvas.ind.sup,bg=Rcol.ind.sup.value)
        assign("Rcol.ind.sup.tmp", Rcol.ind.sup.value, envir=env)
      }
    }
    ChangeColor.ind.sup.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.ind.sup)
    if(!is.null(individuillu)) tkgrid(tklabel(RcolFrame, text=gettextRcmdr("color for supplementary individuals")),canvas.ind.sup,ChangeColor.ind.sup.button)

    Rcol.quali.value<- Rcol.quali                                                                                          #same for sup factors
    canvas.quali <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.quali.value)
    ChangeColor.quali <- function()
    {
      Rcol.quali.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.quali.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Rcol.quali.value)>0)
      {
        tkconfigure(canvas.quali,bg=Rcol.quali.value)
        assign("Rcol.quali.tmp", Rcol.quali.value, envir=env)
      }
    }
    ChangeColor.quali.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.quali)
    if(!is.null(variablefact)) tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color for factors")),canvas.quali,ChangeColor.quali.button)

    RhabillageFrame<-tkframe(PlotIndFrame,borderwidth=2)                                                                                               #new frame for coloring individuals according to sth
    listgraph<-tklistbox(RhabillageFrame,height=4, selectmode="single",exportselection="FALSE",yscrollcommand=function(...) tkset(scrgraph,...))       #empty list box
    scrgraph <-tkscrollbar(RhabillageFrame,repeatinterval=5,command=function(...)tkyview(listgraph,...))            #scroll bar
    listgraph.nom<-"ind"
    tkinsert(listgraph,"end",gettextRcmdr("by.individual"))                                                         #putting "by.individual" in the list box
    if(Rhabillage=="ind") tkselection.set(listgraph, 0)                                                             #color by individual if the user chose it
    indice<-1
##    for (i in 1:ncol(donnee)) {
##      if(is.factor(donnee[,i])) {
##        tkinsert(listgraph,"end",vars[i])
##        listgraph.nom<-c(listgraph.nom,vars[i])
##        if(vars[i]==Rhabillage) tkselection.set(listgraph, indice)
##        indice<-indice+1
##      }
##    }
    if (!is.null(variablefact)){                                                                                    #if there are supplementary variables, they are added to the list box
      for (j in 1:ncol(donnee)){
       if(vars[j] %in% variablefact){
        tkinsert(listgraph,"end",vars[j])
        listgraph.nom<-c(listgraph.nom,vars[j])
        if(Rhabillage==vars[j]) tkselection.set(listgraph, indice)                                                  #selection of the chosen factor
        indice<-indice+1
      }}
    }
    tkgrid(tklabel(RhabillageFrame, text=gettextRcmdr("Coloring for individuals")))                                #settings: text
    tkgrid(listgraph, scrgraph, sticky = "nw")                                                                     #list box and scroll bar
    tkgrid.configure(scrgraph, sticky = "wns")                                                                     #configuration for scroll bar
    tkgrid.configure(listgraph, sticky = "ew")                                                                     #config for list box

    RlimFrame<-tkframe(PlotIndFrame,borderwidth=2)                                                                 #new frame for x and y limits
    if(is.null(RXlimInd)) XlimIndMin<-tclVar("")                                                                   #if nothing is written, XlimIndMin=NULL
    else XlimIndMin<-tclVar(paste(RXlimInd[1]))                                                                    #else it equal to RXlimInd
    XlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMin)                                        #entry for x minimum limit value
    if (is.null(RXlimInd)) XlimIndMax<- tclVar("")                                                                 #same for x maximum limit
    else XlimIndMax<-tclVar(paste(RXlimInd[1]))
    XlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMax)
    tkgrid(tklabel(RlimFrame,text=gettextRcmdr("x limits of the graph:")),XlimIndMin.entry, XlimIndMax.entry)      #positionning of text, entry for min and entry for max limits for axis x
    if(is.null(RYlimInd)) YlimIndMin<- tclVar("")                                                                  #same for y axis
    else YlimIndMin<-tclVar(paste(RYlimInd[1]))
    YlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMin)
    if (is.null(RYlimInd)) YlimIndMax<- tclVar("")
    else YlimIndMax<-tclVar(paste(RYlimInd[2]))
    YlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMax)
    tkgrid(tklabel(RlimFrame,text=gettextRcmdr("y limits of the graph:")),YlimIndMin.entry,YlimIndMax.entry)

    #mise en page des différents frames de PlotIndFrame
    tkgrid(RchoixFrame)                                                                                            #positionning of the different frames:
    tkgrid(RTitleFrame)                                                                                            #title frame
    tkgrid(RinvisibleFrame)                                                                                        #invisible elements frame
    tkgrid(RlabelFrame)                                                                                            #labels frame
    tkgrid(tklabel(PlotIndFrame, text=" "))                                                                        #blank line
    tkgrid(RcolFrame)                                                                                              #colors frame
    tkgrid(RhabillageFrame)                                                                                        #coloring frame
    tkgrid(tklabel(PlotIndFrame, text=" "))                                                                        #blank line
    tkgrid(RlimFrame)                                                                                              #axes limits frame
    tkgrid(tklabel(PlotIndFrame, text=" "))                                                                        #blank line

    # construction de la partie graphique des variables
    PlotVarFrame<-tkframe(PlotWin, borderwidth=5, relief="groove")                                                 #frame for all the choices for the variables graph

    WchoixFrame<-tkframe(PlotVarFrame,borderwidth=2)
    var.check<-tkcheckbutton(WchoixFrame)                                                                          #frame for choosing whether or not to plot the variables graph
    if(Wchoix) var.check.value<-tclVar("1")                                                                        #1 if yes
    else var.check.value<-tclVar("0")                                                                              #else 0
    tkconfigure(var.check, variable=var.check.value)                                                               #assigning value to check button (appears clicked or unclicked
    tkgrid(tklabel(WchoixFrame, text=gettextRcmdr("Plot variables graph"), font=font2),var.check)                  #positionning
    tkgrid(tklabel(WchoixFrame, text="  "))                                                                        #blank line

    WTitleFrame<-tkframe(PlotVarFrame,borderwidth=2)                                                               #title frame
    if (is.null(WTitle)) WTitre <- tclVar(" ")
    else WTitre<-tclVar(WTitle)
    WTitre.entry <-tkentry(WTitleFrame,width="40",textvariable=WTitre)
    tkgrid(tklabel(WTitleFrame,text=gettextRcmdr("Title of the graph")),WTitre.entry)

    WcosFrame<-tkframe(PlotVarFrame,borderwidth=2)                                                                 #cos2 frame
    WlimCosValue<-tclVar(paste(Wlim.cos))                                                                          #WlimCosValue=default value at first
    WlimCos.entry<-tkentry(WcosFrame, width=5, textvariable=WlimCosValue)                                          #creation of the entry for the user to type a value
    tkgrid(tklabel(WcosFrame,text=gettextRcmdr("Draw variables with a cos2 >:")),WlimCos.entry)                    #positionning text and entry in the frame

    WlabelFrame<-tkframe(PlotVarFrame,borderwidth=2)                                                               #labels frame
    label.var.check<-tkcheckbutton(WlabelFrame)
    if ("var" %in% Wlabel) label.var.checkValue<-tclVar("1")
    else label.var.checkValue<-tclVar("0")
    tkconfigure(label.var.check, variable=label.var.checkValue)
    tkgrid(tklabel(WlabelFrame, text=gettextRcmdr("Labels for the active variables")),label.var.check)
    label.quanti.sup.check<-tkcheckbutton(WlabelFrame)
    if ("quanti.sup" %in% Wlabel) label.quanti.sup.checkValue<-tclVar("1")
    else label.quanti.sup.checkValue<-tclVar("0")
    tkconfigure(label.quanti.sup.check, variable=label.quanti.sup.checkValue)
    if(!is.null(variableillu)) tkgrid(tklabel(WlabelFrame, text=gettextRcmdr("Labels for the supplementary variables")),label.quanti.sup.check)

    WcolFrame<-tkframe(PlotVarFrame,borderwidth=2)                                                                  #color frame
    Wcol.var.value <- Wcol.var
    canvas.var <- tkcanvas(WcolFrame,width="80",height="25",bg=Wcol.var.value)
    ChangeColor.var <- function()
    {
      Wcol.var.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Wcol.var.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Wcol.var.value)>0) {
        tkconfigure(canvas.var,bg=Wcol.var.value)
        assign("Wcol.var.tmp", Wcol.var.value, envir=env)
      }
    }
    ChangeColor.var.button <- tkbutton(WcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.var)
    tkgrid(tklabel(WcolFrame, text=gettextRcmdr("Color for active variables")),canvas.var,ChangeColor.var.button)

    Wcol.quanti.sup.value<-Wcol.quanti.sup
    canvas.quanti.sup <- tkcanvas(WcolFrame,width="80",height="25",bg=Wcol.quanti.sup.value)
    ChangeColor.quanti.sup <- function()
    {
      Wcol.quanti.sup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Wcol.quanti.sup.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Wcol.quanti.sup.value)>0) {
        tkconfigure(canvas.quanti.sup,bg=Wcol.quanti.sup.value)
        assign("Wcol.quanti.sup.tmp", Wcol.quanti.sup.value, envir=env)
      }
    }
    ChangeColor.quanti.sup.button <- tkbutton(WcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.quanti.sup)
    if(!is.null(variableillu)) tkgrid(tklabel(WcolFrame, text=gettextRcmdr("Color for supplementary variables")),canvas.quanti.sup,ChangeColor.quanti.sup.button)

    #mise en page des différents frames de PlotVarFrame                                                          #positionning all frames
    tkgrid(WchoixFrame)                                                                                          #plotting or not frame
    tkgrid(WTitleFrame)                                                                                          #title frame
    tkgrid(WcosFrame)                                                                                            #cos2 frame
    tkgrid(WlabelFrame)                                                                                          #labels frame
    tkgrid(tklabel(PlotVarFrame, text=" "))                                                                      #blank line
    tkgrid(WcolFrame)                                                                                            #color frame
    tkgrid(tklabel(PlotVarFrame, text=" "))                                                                      #blank line

    subOKCancelHelp(PlotWin, "plot.PCA")                                                                         #creating OK, Cancel and Help buttons. Help being associated to ?plot.PCA
    tkgrid(PlotIndFrame,PlotVarFrame)                                                                            #positionning of individuals and variables frames side by side
    tkgrid.configure(PlotVarFrame, sticky="n")
    tkgrid(subButtonsFrame, sticky="ew", columnspan=2)                                                           #positionning buttons OK, Cancel and Help
    }

    PlotFrame<-tkframe(IlluFrame)                                                                                #new frame in the main window
    Plot.but<-tkbutton(PlotFrame, textvariable=.PlotLabel, command=OnPlot, borderwidth=3)                        #creation of a new button in that new frame. Clicking on this button will open the window with graphical options
    tkgrid(Plot.but, sticky="ew")                                                                                #positionning the new button
  })


  #! fonction pour la réinitialisation des paramètres
  Reinitializ.funct<-function()                                                                                  #function to re-initialize everything
  {
    tkdestroy(top)                                                                                               #window is closed
    FactoPCA()                                                                                                   #and re-opened
  }


  #! fonction pour le choix des éléments de sortie                                                               #choice of output options
  Sortie.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.sortie<-0
    #déclaration des variables
    RFichier <- ""                                                                                               #write results in a file
    Rpropre<-FALSE                                                                                               #print eigenvalues
    Rvariable<-FALSE                                                                                             #print results for active variables
    Rindividu<-FALSE                                                                                             #print results for active individuals
    Rindsup<-FALSE                                                                                               #print results for supplementary individuals
    Rquantisup<-FALSE                                                                                            #print results for supplementary variables
    Rqualisup<-FALSE                                                                                             #print results for supplementary factors
    Rdescdim<-FALSE                                                                                              #print results of the description of dimensions

    .SortieLabel<-tclVar(paste(firstLabel, "", sep=" "))

    OnSortie<-function()                                                                                         #function launched by clicking on the "outputs" button
    {
      SortieWin<-tktoplevel()                                                                                    #new tcltk window
      tkwm.title(SortieWin,"Outputs")                                                                            #title

      #création de la fonction onOKsub
      onOK.sortie<-function()                                                                                    #function launched by clicking on OK
      {
        assign("compteur.sortie", compteur.sortie+1, envir=env)                                                  #when clicking on OK, "compteur.sortie" goes from 0 to 1 and the "output" button of the main window becomes blue
        if(compteur.sortie>0) tclvalue(.SortieLabel)<-paste(label, "", sep=" ")
        tkconfigure(Sortie.but, fg="blue")

        if(tclvalue(eigValue)=="1") assign("Rpropre", TRUE, envir=env)                                           #if "eigenvalues" check button is checked, Rpropre is TRUE
        else assign("Rpropre", FALSE, envir=env)                                                                 #else it is FALSE

        if(tclvalue(varValue)=="1") assign("Rvariable", TRUE, envir=env)                                         
        else assign("Rvariable", FALSE, envir=env)

        if(tclvalue(indValue)=="1") assign("Rindividu", TRUE, envir=env)
        else assign("Rindividu", FALSE, envir=env)

        if(tclvalue(ind.sup.Value)=="1") assign("Rindsup", TRUE, envir=env)
        else assign("Rindsup", FALSE, envir=env)

        if(tclvalue(quanti.sup.Value)=="1") assign("Rquantisup", TRUE, envir=env)
        else assign("Rquantisup", FALSE, envir=env)

        if(tclvalue(quali.sup.Value)=="1") assign("Rqualisup", TRUE, envir=env)
        else assign("Rqualisup", FALSE, envir=env)

        if(tclvalue(descdimValue)=="1") assign("Rdescdim", TRUE, envir=env)
        else assign("Rdescdim", FALSE, envir=env)

        if (tclvalue(Fichier)=="") assign("RFichier", NULL, envir=env)
        assign("RFichier", tclvalue(Fichier), envir=env)

        tkdestroy(SortieWin)
      }
                                                                                                                
      eig.lab <-tklabel(SortieWin, text=gettextRcmdr("Eigenvalues"))                                            #label for the "eigenvalues" check button
        eig.check <- tkcheckbutton(SortieWin)                                                                   #creation of the check button
        if(Rpropre) eigValue <- tclVar("1")                                                                     #variable eigValue is equal to 1 or 0 according to the value or Rpropre
        else eigValue <- tclVar("0")
        tkconfigure(eig.check,variable=eigValue)                                                                #the button appears checked or unchecked according to eigValue

      var.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for active variables"))
        var.check <- tkcheckbutton(SortieWin)
        if(Rvariable) varValue <- tclVar("1")
        else varValue <- tclVar("0")
        tkconfigure(var.check,variable=varValue)

      ind.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for active individuals"))
        ind.check <- tkcheckbutton(SortieWin)
        if(Rindividu) indValue <- tclVar("1")
        else indValue <- tclVar("0")
        tkconfigure(ind.check,variable=indValue)

      ind.sup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for supplementary individuals"))
        ind.sup.check <- tkcheckbutton(SortieWin)
        if(Rindsup) ind.sup.Value <- tclVar("1")
        else ind.sup.Value <- tclVar("0")
        tkconfigure(ind.sup.check,variable=ind.sup.Value)

      quanti.sup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for supplementary variables"))
        quanti.sup.check <- tkcheckbutton(SortieWin)
        if(Rquantisup) quanti.sup.Value <- tclVar("1")
        else quanti.sup.Value <- tclVar("0")
        tkconfigure(quanti.sup.check,variable=quanti.sup.Value)

      quali.sup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for supplementary factors"))
        quali.sup.check <- tkcheckbutton(SortieWin)
        if(Rqualisup) quali.sup.Value <- tclVar("1")
        else quali.sup.Value <- tclVar("0")
        tkconfigure(quali.sup.check,variable=quali.sup.Value)

      descdim.lab<-tklabel(SortieWin, text=gettextRcmdr("Description of the dimensions"))
        descdim.check<-tkcheckbutton(SortieWin)
        if(Rdescdim) descdimValue<-tclVar("1")
        else descdimValue<-tclVar("0")
        tkconfigure(descdim.check,variable=descdimValue)

      RFichierFrame<-tkframe(SortieWin,borderwidth=2)
      if (is.null(RFichier)) Fichier <- tclVar("")
      else Fichier<-tclVar(RFichier)
      Fichier.entry <-tkentry(RFichierFrame,width="40",textvariable=Fichier)
      tkgrid(tklabel(RFichierFrame,text=gettextRcmdr("Print results on a 'csv' file")),Fichier.entry)

      SortieOK.but<-tkbutton(SortieWin,text="OK",width=16,command=onOK.sortie)                                                #OK button associated to the function above

        tkgrid(tklabel(SortieWin, text = gettextRcmdr("Select output options"), fg ="blue"),  columnspan = 2, sticky = "w")   #settings. Text
        tkgrid(tklabel(SortieWin, text = " "))                                                                                #blank line
        tkgrid(eig.lab,eig.check,sticky="w")                                                                                  #"eigenvalues" check button
        tkgrid(var.lab,var.check,sticky="w")                                                                                  #"results for var" check button
        tkgrid(ind.lab,ind.check,sticky="w")                                                                                  #"results for ind" check button
        if (!is.null(individuillu)) tkgrid(ind.sup.lab,ind.sup.check,sticky="w")                                              #"results for sup ind" check button if sup ind have been selected
        if (!is.null(variableillu)) tkgrid(quanti.sup.lab,quanti.sup.check,sticky="w")                                        #"results for sup var" check button if sup var have been selected
        if (!is.null(variablefact)) tkgrid(quali.sup.lab,quali.sup.check,sticky="w")                                          #"results for sup fact" check button if sup fact have been selected
        tkgrid(descdim.lab,descdim.check,sticky="w")                                                                          #"results for description of the dimension" check button
        tkgrid(tklabel(SortieWin, text = " "))                                                                                #blank line
        tkgrid(RFichierFrame)                                                                                                 #frame for the option to print or not results in a file
        tkgrid(SortieOK.but)                                                                                                  #OK button
   }

    SortieFrame<-tkframe(IlluFrame)                                                                                           #new frame in the main window
    Sortie.but<-tkbutton(SortieFrame, textvariable=.SortieLabel, command=OnSortie, borderwidth=3)                             #"output" button
    tkgrid(Sortie.but, sticky="ew")                                                                                           #new button positionned in the new frame
  })

  #! fonction HCPC                                                                                           

  Hcpc.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    .HcpcLabel<-tclVar(paste(firstLabel, "", sep=" "))
    compteur.hcpc<-0
    Rclassif<-0                                                                                      #variable for the choice: perform HCPC or not
    Rmeth <- -1                                                                                      #variable for the choice of the method
    Rconsolid<-0                                                                                     #variable for the choice of consolidation
    Rgraphhcpc<-1                                                                                    #variable for graphs
    Rreshcpc<-0                                                                                      #variable for printing results
    Rminhcpc<-3                                                                                      #variable for the minimum nb of clusters
    Rmaxhcpc<-10                                                                                     #variable for the maximum nb of clusters

    OnHCPC <- function()                                                                             #function launched by clicking on "perform HCPC after PCA"
    {

      HcpcWin<-tktoplevel()                                                                          #creation of a tcltk window
      tkwm.title(HcpcWin, gettextRcmdr("HCPC options"))                                              #title of the window

      onOKHcpc <- function()                                                                         #function launched by clicking on OK
      {
        assign("compteur.hcpc", compteur.hcpc+1, envir=env)                                          #"perform HCPC after PCA" button becomes blue after licking on OK
        if(compteur.hcpc>0) tclvalue(.HcpcLabel)<-paste(label, "", sep=" ")
        tkconfigure(Hcpc.but, fg="blue")

        if(tclvalue(methValue)=="interactive") assign("Rmeth", 0, envir=env)                         #assigning value to Rmeth: 0 or -1
        else assign("Rmeth", -1, envir=env)

        if(tclvalue(consolidValue)=="1") assign("Rconsolid",TRUE, envir=env)                         #assigning value to Rconsolid: TRUE or FALSE
        else assign("Rconsolid",FALSE,envir=env)

        if(tclvalue(graphhcpcValue)=="1") assign("Rgraphhcpc",TRUE,envir=env)                        #assigning value to Rgraphhcpc: TRUE or FALSE
        else assign("Rgraphhcpc",FALSE,envir=env)

        if(tclvalue(reshcpcValue)=="1") assign("Rreshcpc",TRUE,envir=env)                            #assigning value to Rreshcpc: TRUE or FALSE
        else assign("Rreshcpc",FALSE,envir=env)

        assign("Rminhcpc",as.numeric(tclvalue(minhcpc)),envir=env)                                   #assigning value to Rminhcpc: numeric value
        assign("Rmaxhcpc",as.numeric(tclvalue(maxhcpc)),envir=env)                                   #assigning value to Rmaxhcpc: numeric value

        assign("Rclassif",TRUE,envir=env)                                                            #assigning value to Rclassif: TRUE
        tkdestroy(HcpcWin)                                                                           #window is closed
      }
      OKHcpc.but<-tkbutton(HcpcWin, text="OK", width=8,command=onOKHcpc)  #OK button

      onCancelHcpc <- function()                                          #function launched by clicking on Cancel button
      {
        assign("Rclassif",FALSE,envir=env)                                #Rclassif=FALSE
        tkdestroy(HcpcWin)                                                #the window is closed
      }
      CancelHcpc.but<-tkbutton(HcpcWin, text="Cancel", width=8,command=onCancelHcpc)   #Cancel button

      tkgrid(tklabel(HcpcWin, text=""))                                   #settings: blank line
      tkgrid(tklabel(HcpcWin, text = gettextRcmdr("Hierarchical Clustering on Principal Components"), fg = "darkred"), column=1, columnspan = 8, sticky = "ew")                   #settings: text

      meth1 <- tkradiobutton (HcpcWin)                                    #creation of a radio button
      meth1.lab <- tklabel(HcpcWin,text=gettextRcmdr("interactive"))      #label of the button
      meth2 <- tkradiobutton (HcpcWin)                                    #other radio button
      meth2.lab <- tklabel(HcpcWin,text=gettextRcmdr("automatic"))        #label
      methValue <- tclVar("interactive")                                  #variable associated with the radio buttons
      meth.lab <- tklabel(HcpcWin,text=gettextRcmdr("Choice of the number of clusters: "))  #main label
      tkconfigure(meth1,variable=methValue,value="interactive")                             #assigning value to the first radio button
      tkconfigure(meth2,variable=methValue,value="automatic")                               #assigning value to the second radio button

      minmaxhcpc.label<-tklabel(HcpcWin,text=gettextRcmdr("The optimal number of clusters is chosen between:"))                                                              #text for the choice of the nb of clusters

      minhcpc<-tclVar("3")                                                #default value for min nb
      maxhcpc<-tclVar("10")                                               #default value for max nb
      minhcpc.entry <-tkentry(HcpcWin,width="3",textvariable=minhcpc)     #text zone for the min nb
      maxhcpc.entry <-tkentry(HcpcWin,width="3",textvariable=maxhcpc)     #text zone for the max nb

      consolid.lab <- tklabel(HcpcWin,text=gettextRcmdr("Consolidate clusters "))   #label for consolidation
      consolid.check <- tkcheckbutton(HcpcWin)                                      #check button
      if(Rconsolid) consolidValue<-tclVar("1")                                      #assigning value according to Rconsolid's value
      else consolidValue<-tclVar("0")                                               
      tkconfigure(consolid.check,variable=consolidValue)                            #assigning consolidValue to the check button

      graphhcpc.lab <- tklabel(HcpcWin,text=gettextRcmdr("Print graphs "))          #label for graphs
      graphhcpc.check <- tkcheckbutton(HcpcWin)                                     #check button
      if(Rgraphhcpc) graphhcpcValue <- tclVar("1")                                  #value depending on Rgraphhcpc
      else graphhcpcValue <- tclVar("0")
      tkconfigure(graphhcpc.check,variable=graphhcpcValue)                          #assigning graphhcpcValue to the check button

      reshcpc.lab <- tklabel(HcpcWin,text=gettextRcmdr("Print results for clusters "))  #label
      reshcpc.check <- tkcheckbutton(HcpcWin)                                           #check button
      if(Rreshcpc) reshcpcValue<-tclVar("1")                                            #value depending on Rreshcpc
      else reshcpcValue <- tclVar("0")
      tkconfigure(reshcpc.check,variable=reshcpcValue)                                  #assigning reshcpcValue to the check button

      tkgrid(tklabel(HcpcWin,text=gettextRcmdr("Select options for the HCPC"), fg = "blue"), column=1, columnspan=8, sticky="we")                                                       #settings: text
      tkgrid(tklabel(HcpcWin,text=""))                                            #blank line
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr(paste('Clustering is performed on the first ', tclvalue(ncp.val), ' dimensions of the PCA',sep=""))),column=1,columnspan=4,sticky="w")   #text which takes the nb of dimensions chosen in the main window
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr("(Change your choice in the main options to change this number)")),column=1,columnspan=4,sticky="w")                                     #text
      tkgrid(tklabel(HcpcWin,text=""))                                            #blank line
      tkgrid(meth.lab,meth1.lab,meth1)                                            #positionning text concerning the choice of the method and the first radio button side by side)
      tkgrid(meth2.lab,meth2)                                                     #positionning radio button nb 2 and its label
      tkgrid(tklabel(HcpcWin,text=""))                                            #blank line
      tkgrid(minmaxhcpc.label,minhcpc.entry , maxhcpc.entry)                      #positionning everything that concerns the nb of clusters
      tkgrid(tklabel(HcpcWin,text=""))                                            #blank line
      tkgrid(consolid.lab,consolid.check)                                         #positionning text and button for consolidation
      tkgrid(graphhcpc.lab,graphhcpc.check)                                       #positionning label and button for graphs
      tkgrid(reshcpc.lab,reshcpc.check)                                           #positionning label and button for results
      tkgrid(tklabel(HcpcWin,text=""))                                            #blank line
      tkgrid(OKHcpc.but, CancelHcpc.but)                                          #OK and Cancel buttons
      tkgrid(tklabel(HcpcWin, text=""))                                           #blank line

      tkgrid.configure(minmaxhcpc.label,meth.lab,consolid.lab,graphhcpc.lab,reshcpc.lab,column=1,columnspan=4,sticky="w")                                                         #configuration of everything with the number of the column it will appear in (8 columns in a window) and the position in the column (w: west, e: east)
      tkgrid.configure(minhcpc.entry,column=7,columnspan=1,sticky="e")
      tkgrid.configure(maxhcpc.entry,column=8,columnspan=1,sticky="w")
      tkgrid.configure(meth1,meth2,consolid.check,graphhcpc.check,reshcpc.check,column=8,sticky="e")
      tkgrid.configure(meth1.lab,column=6,columnspan=2,sticky="w")
      tkgrid.configure(meth2.lab,column=6,columnspan=2,sticky="w")
      tkgrid.configure(OKHcpc.but,column=2,columnspan=1,sticky="w")
      tkgrid.configure(CancelHcpc.but,column=6,columnspan=1,sticky="e")

      tkgrid.columnconfigure(HcpcWin,0, minsize=3)                                #configuration of the size of some columns
      tkgrid.columnconfigure(HcpcWin,5, minsize=5)
      tkgrid.columnconfigure(HcpcWin,8, minsize=3)

}
    Hcpc2Frame<-tkframe(HcpcFrame)                                                #new frame in the HCPC frame
    Hcpc.but<-tkbutton(Hcpc2Frame, textvariable=.HcpcLabel, command=OnHCPC, borderwidth=3)  #button in this new frame
    tkgrid(Hcpc.but, sticky="ew")                                                 #positionning the button
})


  #! fonction associée au bouton Appliquer, execute sans détruire la fenêtre top
  OnAppliquer<-function()                                                         #funtion launched by clicking on "Apply"
  {

    # liste de toutes les variables interne créées      (** mise en forme incomplète)
      # sur la fenetre principale
#         listdesc         **
#         resu.val         **
#         ncp.val          **
#         reduitValue      **
#         Axe1
#         Axe2
      # dans les boutons des fenêtres illustratives
#         variablefact     **
#         variableillu     **
#         individuillu     **
      # dans le bouton Plot PCA
#         Rchoix
#         RTitle
#         Rlabel
#         Rcol.ind
#         Rcol.ind.sup
#         Rcol.quali
#         Rhabillage       **
#         RXlimInd
#         RYlimInd
#         Wchoix
#         WTitle
#         Wlabel
#         Wlim.cos
#         Wcol.quanti.sup
#         Wcol.var
#         WXlimVar
#         WYlimVar
      # dans le bouton affichage sortie
#         Rpropre
#             Rvariable
#           Rindividu
#           Rindsup
#           Rquantisup
#           Rqualisup


    # récupération des paramètres de la fenêtre principale
    nom.res<-tclvalue(resu.val)                                                   #putting resu.val in nom.res
    if (length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0) justDoIt(paste('remove (',nom.res,')'))                                                                    #if object res already exists, it's removed
    if(length(as.numeric(tkcurselection(listdesc)))<2) varActives<-listdesc.nom
    else varActives<-listdesc.nom[as.numeric(tkcurselection(listdesc))+1]
    varActives <- varActives[!(varActives%in%variableillu)]                       #varActives is the list of selected active variables
    reduction<-TRUE                                                               #scaling=T by default
    if(tclvalue(reduitValue)=="0") reduction<-FALSE                               #if "sclaing" check button is unchecked, scaling=F
    ncp<-as.numeric(tclvalue(ncp.val))                                            #putting chosen nb of dim in ncp
    Axe<-c(as.numeric(tclvalue(Axe1)), as.numeric(tclvalue(Axe2)))                #putting chosen axes in Axe

    # gestion du tableau de données pour l'ACP

    if(!is.null(individuillu)) {                                                  #if sup ind have been chosen, active ind are the remaining ones
      ind.actif<-rows[-which(rows %in% individuillu)]
      if(!is.null(variableillu)) {                                                #a new dataset is created by the function commande.data which uses everything concerning active and supplementary variables and individuals. Different cases are separated according to presence or absence of those elements
        if(!is.null(variablefact)) commande.data<-paste(activeDataSet(),'.', 'PCA', '<-', activeDataSet(), '[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '") ,c("', paste(varActives, collapse='", "'), '", "', paste(variableillu, collapse='", "'), '", "', paste(variablefact, collapse='", "'), '")]', sep="")
        else commande.data<-paste(activeDataSet(),'.', 'PCA', '<-', activeDataSet(), '[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '") ,c("', paste(varActives, collapse='", "'), '", "', paste(variableillu, collapse='", "'), '")]', sep="")
      }
      else {
        if(!is.null(variablefact)) commande.data<-paste(activeDataSet(),'.', 'PCA', '<-', activeDataSet(), '[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '") ,c("', paste(varActives, collapse='", "'), '", "', paste(variablefact, collapse='", "'), '")]', sep="")
        else commande.data<-paste(activeDataSet(),'.', 'PCA', '<-', activeDataSet(), '[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '") ,c("', paste(varActives, collapse='", "'), '")]', sep="")
      }
    }
    else
    {
      if(!is.null(variableillu)) {
        if(!is.null(variablefact)) commande.data<-paste(activeDataSet(),'.', 'PCA', '<-', activeDataSet(), '[, c("', paste(varActives, collapse='", "'), '", "', paste(variableillu, collapse='", "'), '", "', paste(variablefact, collapse='", "'), '")]', sep="")
        else commande.data<-paste(activeDataSet(),'.', 'PCA', '<-', activeDataSet(), '[, c("', paste(varActives, collapse='", "'), '", "', paste(variableillu, collapse='", "'), '")]', sep="")
      }
      else {
        if(!is.null(variablefact)) commande.data<-paste(activeDataSet(),'.', 'PCA', '<-', activeDataSet(), '[, c("', paste(varActives, collapse='", "'), '", "', paste(variablefact, collapse='", "'), '")]', sep="")
        else commande.data<-paste(activeDataSet(),'.', 'PCA', '<-', activeDataSet(), '[, c("', paste(varActives, collapse='", "'), '")]', sep="")
      }
    }
    justDoIt(commande.data)                                       #commande.data is launched
    logger(commande.data)
    donnee.depart<-activeDataSet()
    activeDataSet(paste(activeDataSet(),'.', 'PCA', sep=""))      #the new dataset (where columns and rows have been re-ordered) is called dataset_name.PCA

    # gestion de la commande réalisant l'ACP
    if(!is.null(individuillu)) {
      ind.actif<-rows[-which(rows %in% individuillu)]
      if(!is.null(variableillu)) {                               #PCA is performed by commande.acp which uses the name of the active dataset (created right above), scale.unit, ncp, ind.sup, quanti.sup and quali.sup. graph is FALSE by default
        if(!is.null(variablefact)) commande.acp<-paste(nom.res, '<-PCA(', activeDataSet(), ', scale.unit=', reduction, ', ncp=', ncp, ', ind.sup=c(', nrow(get(.activeDataSet))-length(individuillu)+1, ': ', nrow(get(.activeDataSet)), '), quanti.sup=c(', length(varActives)+1, ': ', length(varActives)+ length(variableillu), '), quali.sup=c(', length(varActives)+length(variableillu)+1, ': ', length(varActives)+length(variableillu)+length(variablefact), '), graph = FALSE)', sep="")
        else commande.acp<-paste(nom.res, '<-PCA(', activeDataSet(), ', scale.unit=', reduction, ', ncp=', ncp, ', ind.sup=c(', nrow(get(.activeDataSet))-length(individuillu)+1, ': ', nrow(get(.activeDataSet)), '), quanti.sup=c(', length(varActives)+1, ': ', length(varActives)+ length(variableillu), '), graph = FALSE)', sep="")
      }
      else {
        if(!is.null(variablefact)) commande.acp<-paste(nom.res, '<-PCA(', activeDataSet(), ', scale.unit=', reduction, ', ncp=', ncp, ', ind.sup=c(', nrow(get(.activeDataSet))-length(individuillu)+1, ': ', nrow(get(.activeDataSet)), '), quali.sup=c(', length(varActives)+1, ': ', length(varActives)+length(variablefact), '), graph = FALSE)', sep="")
        else commande.acp<-paste(nom.res, '<-PCA(', activeDataSet(), ', scale.unit=', reduction, ', ncp=', ncp, ', ind.sup=c(', nrow(get(.activeDataSet))-length(individuillu)+1, ': ', nrow(get(.activeDataSet)), '), graph = FALSE)', sep="")
      }
    }
    else {
      if(!is.null(variableillu)) {
        if(!is.null(variablefact)) commande.acp<-paste(nom.res, '<-PCA(', activeDataSet(), ' , scale.unit=', reduction, ', ncp=', ncp, ', quanti.sup=c(', length(varActives)+1, ': ', length(varActives)+ length(variableillu), '), quali.sup=c(', length(varActives)+length(variableillu)+1, ': ', length(varActives)+length(variableillu)+length(variablefact), '), graph = FALSE)', sep="")
        else commande.acp<-paste(nom.res, '<-PCA(', activeDataSet(), ' , scale.unit=', reduction, ', ncp=', ncp, ', quanti.sup=c(', length(varActives)+1, ': ', length(varActives)+ length(variableillu), '), graph = FALSE)', sep="")
      }
      else{
        if(!is.null(variablefact)) commande.acp<-paste(nom.res, '<-PCA(', activeDataSet(), ' , scale.unit=', reduction, ', ncp=', ncp, ', quali.sup=c(', length(varActives)+1, ': ', length(varActives)+length(variablefact), '), graph = FALSE)', sep="")
        else commande.acp<-paste(nom.res, '<-PCA(', activeDataSet(), ' , scale.unit=', reduction, ', ncp=', ncp, ', graph = FALSE)', sep="")
      }
    }
    justDoIt(commande.acp)                         #commande.acp is performed
    logger(commande.acp)


    #Commande de la fonction HCPC

    if(Rclassif==TRUE){                            #if Rclassif is TRUE then HCPC is performed with options chosen by the user
      commande.hcpc<-paste(nom.res,'.hcpc', '<-HCPC(', nom.res, ' ,nb.clust=', Rmeth, ',consol=', Rconsolid,',min=', Rminhcpc,',max=',Rmaxhcpc,',graph=', Rgraphhcpc, ')', sep="")
    justDoIt(commande.hcpc)
    logger(commande.hcpc)
      if(Rreshcpc==TRUE){                          #if Rreshcpc is TRUE then results are printed
        doItAndPrint(paste(nom.res,'.hcpc$data.clust[,ncol(res.hcpc$data.clust),drop=F]', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.var', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.axes', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.ind', sep=""))
      }
    }

    # gestion des graphiques

    if (length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0) {if (get(nom.res)$eig[1,2]==100) doItAndPrint(paste('"No graph can be plot: data are unidimensional"'))}     #error message in case data are unidimensional
    if((Rchoix)&length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0){  #command for the graph of individuals
    if (get(nom.res)$eig[1,2]!=100) {
      if ((Rhabillage!="none") & (Rhabillage!="ind")) {
        Rhabillage<-which(colnames(get(.activeDataSet))==Rhabillage)
        if(length(Rhabillage)==0) Rhabillage<-"none"
      }
      if (Rhabillage=="none") Rhabillage<-paste('"', Rhabillage, '"', sep="")
      if (Rhabillage=="ind") Rhabillage<-paste('"', Rhabillage, '"', sep="")

      commande.plotInd<-paste('plot.PCA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="ind", habillage=', Rhabillage, ', col.ind="', Rcol.ind, '", col.ind.sup="', Rcol.ind.sup, '", col.quali="', Rcol.quali, '", label=c("', paste(Rlabel, collapse='", "'), '")', sep="")
      if (!is.null(RXlimInd)) commande.plotInd<-paste(commande.plotInd, ', xlim=c(', paste(RXlimInd, collapse=", "), ')')
      if (!is.null(RYlimInd)) commande.plotInd<-paste(commande.plotInd, ', ylim=c(', paste(RYlimInd, collapse=", "), ')')
      if (!is.null(Rinvisible)) commande.plotInd<-paste(commande.plotInd, ', invisible=c("', paste(Rinvisible, collapse='", "'),'")', sep='')
      if (is.null(RTitle)) commande.plotInd <- paste(commande.plotInd,')', sep="")
      else {
        if (RTitle ==" ") commande.plotInd <- paste(commande.plotInd,')', sep="")
        else commande.plotInd <- paste(commande.plotInd,', title="', RTitle,'")', sep="")
      }
      justDoIt(commande.plotInd)
      logger(commande.plotInd)
    }}


    if((Wchoix)&length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0){  #command for the graph of variables
    if (get(nom.res)$eig[1,2]!=100) {
      commande.plotVar<-paste('plot.PCA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="var", col.var="', Wcol.var, '", col.quanti.sup="', Wcol.quanti.sup, '", label=c("', paste(Wlabel, collapse='", "'), '"), lim.cos2.var=', Wlim.cos, sep="")
      if (is.null(WTitle)) commande.plotVar <- paste(commande.plotVar,')', sep="")
      else {
        if (WTitle ==" ") commande.plotVar <- paste(commande.plotVar,')', sep="")
        else commande.plotVar <- paste(commande.plotVar,', title="', WTitle,'")', sep="")
      }
      justDoIt(commande.plotVar)
      logger(commande.plotVar)
    }}


    # gestion de l'édition de certains resultats                             
    if (RFichier==""){                                                       #printing the results
      if(Rpropre) doItAndPrint(paste( nom.res, '$eig', sep=""))
      if(Rvariable) doItAndPrint(paste( nom.res, '$var', sep=""))
      if(Rindividu) doItAndPrint(paste( nom.res, '$ind', sep=""))
      if(Rindsup) doItAndPrint(paste( nom.res, '$ind.sup', sep=""))
      if(Rquantisup) doItAndPrint(paste( nom.res, '$quanti.sup', sep=""))
      if(Rqualisup) doItAndPrint(paste( nom.res, '$quali.sup', sep=""))
      if(Rdescdim) doItAndPrint( paste('dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '))', sep=""))
    }
    else {                                                                  #or saving them in a file
      Fich = RFichier
      if (substr(Fich,1,1)!='"') Fich = paste('"',Fich,sep='')
      if (substr(Fich,nchar(Fich),nchar(Fich))!='"') Fich = paste(Fich,'"',sep='')
      append = FALSE
      if(Rpropre){
        doItAndPrint(paste('write.infile(', nom.res, '$eig, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rvariable){
        doItAndPrint(paste('write.infile(', nom.res, '$var, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rindividu){
        doItAndPrint(paste('write.infile(', nom.res, '$ind, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rindsup){
        doItAndPrint(paste('write.infile(', nom.res, '$ind.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rquantisup){
        doItAndPrint(paste('write.infile(', nom.res, '$quanti.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rqualisup){
        doItAndPrint(paste('write.infile(', nom.res, '$quali.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rdescdim) doItAndPrint(paste('write.infile(dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), ')), file =',Fich,',append=',append,')', sep=""))
    }

    # Re-chargement du tableau de départ et supression du tableau temporaire
    activeDataSet(donnee.depart)                                                    #reloading of the "real" dataset
    justDoIt(paste('remove(',activeDataSet(),'.PCA)',sep=""))                       #the new one is removed
    logger(paste('remove(',activeDataSet(),'.PCA)',sep=""))
  }


    #! fonction associée au bouton OK, execute et détruit l'interface graphique    #function associated with the OK button
  onOK<-function()
  {
    OnAppliquer()                                                                 #OnAppliquer function is performed
    # destuction de la fenêtre Top
    closeDialog(top)                                                              #then window is closed
  }



################################################################################
#                   Création de la fenêtre top                                 #  #creation of the main window
################################################################################
  top<-tktoplevel(borderwidth=10)                                                 #tcltk window
  tkwm.title(top, gettextRcmdr("PCA"))                                            #title
  tkwm.geometry(top, "-100+50")                                                   #size

  # définition des polices
  font2<-tkfont.create(family="times",size=12,weight="bold")                      #font for subtitles
  fontheading<-tkfont.create(family="times",size=18,weight="bold")                #font for titles

  # récupération du jeu de données actif
  donnee<-get(.activeDataSet)                                                     #getting active dataset
  vars<-colnames(donnee)                                                          #getting variables names
  rows<-rownames(donnee)                                                          #getting individuals names

  # création de la liste pour le choix des variables acives
  listdesc<-tklistbox(top,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scr,...))                                                                  #empty list box for active variables
  scr <-tkscrollbar(top,repeatinterval=5,command=function(...)tkyview(listdesc,...))  #scroll bar
  listdesc.nom<-NULL
  for (i in (1:ncol(donnee))) {                                                   #putting variables in the list box
      if (is.numeric(donnee[,i])) {
          tkinsert(listdesc,"end",vars[i])
          listdesc.nom<-c(listdesc.nom, vars[i])
      }
  }

  # création de tous les boutons d'options dans IlluFrame
  IlluFrame<- tkframe(top, borderwidth=2)                                         #new frame in the top window
  Reinitializ.but<-tkbutton(IlluFrame, text=gettextRcmdr("Restart"),width=18,command=Reinitializ.funct, borderwidth=3)                                                                   #"Reinitialize" button
       # mise en page de IlluFrame

  Fillu.funct(label=gettextRcmdr("Modify supplementary factors"), firstLabel=gettextRcmdr("Select supplementary factors"))                                                         #labels for the "select sup fact" button: firstlabel is the default label, label is the label which appears after the selection of sup fact(after clicking on OK)
  Dillu.funct(label=gettextRcmdr("Modify supplementary variables"), firstLabel=gettextRcmdr("Select supplementary variables"))                                                       #same for sup var
  Iillu.funct(label=gettextRcmdr("Modify supplementary individuals"), firstLabel=gettextRcmdr("Select supplementary individuals"))                                                     #same for sup ind
  PLOT.PCA(label=gettextRcmdr("Graphical options"), firstLabel=gettextRcmdr("Graphical options"))  
                                                                                  #same for graphical options
  Sortie.funct(label=gettextRcmdr("Outputs"), firstLabel=gettextRcmdr("Outputs")) #same for outputs
  tkgrid(FilluFrame, DilluFrame, IilluFrame, columnspan=7)                        #positionning buttons for supplementary elements
  tkgrid(tklabel(IlluFrame, text=""))                                             #blank line
  tkgrid(PlotFrame, SortieFrame, Reinitializ.but, columnspan=7)                   #buttons for graphical and output options and "reinitialize" button
  tkgrid.configure(FilluFrame, column=1, columnspan=1)                            #configuration of each button
  tkgrid.configure(PlotFrame, column=1, columnspan=1)
  tkgrid.configure(DilluFrame, column=3, columnspan=1)
  tkgrid.configure(SortieFrame, column=3, columnspan=1)
  tkgrid.configure(IilluFrame, column=5, columnspan=1)
  tkgrid.configure(Reinitializ.but, column=5, columnspan=1)
  tkgrid.columnconfigure(IlluFrame,0, minsize=25)                                 #configuration of columns
  tkgrid.columnconfigure(IlluFrame,7, minsize=25)
  tkgrid.columnconfigure(IlluFrame,2, minsize=40)
  tkgrid.columnconfigure(IlluFrame,4, minsize=40)

  # création des options dans OptionFrame                                         #options frame
  OptionFrame<-tkframe(top, borderwidth=2, relief="groove")                       #creation of a new frame
  resu.lab<-tklabel(OptionFrame,text=gettextRcmdr("Name of the result object: ")) #label for result's name
  resu.val<-tclVar("res")                                                         #default value for result's name
  resu<-tkentry(OptionFrame,width=10,textvariable=resu.val)                       #entry for result's name
  ncp.lab<-tklabel(OptionFrame,text=gettextRcmdr("Number of dimensions: "))       #label for the choice of the nb of dimensions
  ncp.val<-tclVar("5")                                                            #default value
  ncp<-tkentry(OptionFrame,width=5,textvariable=ncp.val)                          #entry
  reduit.lab<-tklabel(OptionFrame,text=gettextRcmdr("Scale the variables: "))     #label for scaling
  reduit.check <- tkcheckbutton(OptionFrame)                                      #checkbutton
  reduitValue <- tclVar("1")                                                      #default value
  tkconfigure(reduit.check,variable=reduitValue)                                  #configuration
  Axe.label<-tklabel(OptionFrame,text=gettextRcmdr("Graphical output: select the dimensions:")) #label for axes
  Axe1<-tclVar("1")                                                               #default value for first axes
  Axe2<-tclVar("2")                                                               #default value for second axes
  Axe1.entry <-tkentry(OptionFrame,width="5",textvariable=Axe1)                   #entry for first axes
  Axe2.entry <-tkentry(OptionFrame,width="5",textvariable=Axe2)                   #entry for second axes

    # mise en page de OptionFrame                                                 #settings
  tkgrid(tklabel(OptionFrame,text=gettextRcmdr("Main options"), fg = "darkred"), columnspan=8, sticky="we")                                                                             #text
  tkgrid(tklabel(OptionFrame,text=""))                                            #blank line
  tkgrid(resu.lab, resu)
  tkgrid(ncp.lab, ncp)
  tkgrid(reduit.lab,reduit.check)
  tkgrid(Axe.label,Axe1.entry , Axe2.entry, sticky="w")

  tkgrid.configure(ncp.lab, reduit.lab, resu.lab, Axe.label, column=1, columnspan=4, sticky="w")  #configuration
  tkgrid.configure(ncp, reduit.check, resu, column=6, columnspan=2, sticky="e")
  tkgrid.configure(Axe1.entry, column=6, columnspan=1, sticky="w")
  tkgrid.configure(Axe2.entry, column=7, columnspan=1, sticky="e")

  tkgrid.columnconfigure(OptionFrame,0, minsize=25)                              #columns' configuration
  tkgrid.columnconfigure(OptionFrame,5, minsize=40)
  tkgrid.columnconfigure(OptionFrame,8, minsize=25)

  #Frame pour HCPC                                                               #HCPC frame
  HcpcFrame<-tkframe(top, borderwidth=2)
  Hcpc.funct(label=gettextRcmdr("Perform Clustering after PCA"), firstLabel=gettextRcmdr("Perform Clustering after PCA"))
  tkgrid(Hcpc2Frame, columnspan=7)
  tkgrid.configure(Hcpc2Frame,column=4, columnspan=1)

  appliquer.but<-tkbutton(top, text=gettextRcmdr("Apply"),width=12,command=OnAppliquer, borderwidth=3, fg="#690f96")                                                                   #"Apply" button
  OKCancelHelp(helpSubject="PCA")                                                #OK, Cancel and Help button. Help leads to ?PCA

  #TOP                                                                           #settings of the main window
  tkgrid(tklabel(top, text=gettextRcmdr("Principal Components Analysis (PCA)"),font=fontheading),columnspan=3)                                                                   #title
  tkgrid(tklabel(top,text=""))                                                   #blank line
  tkgrid(tklabel(top, text = gettextRcmdr("Select active variables (by default all the variables are active)"),fg = "darkred"), column=1,columnspan=2, sticky = "w")                 #text
  tkgrid(listdesc, scr, sticky = "nw")                                           #list box with available continuous variables and scroll bar
  tkgrid.configure(scr, sticky = "ens",column=2)
  tkgrid.configure(listdesc, sticky = "ew", column=1, columnspan=2)
  tkgrid(tklabel(top,text=""))                                                   #blank line
  tkgrid(IlluFrame, column=1, columnspan=1)                                      #buttons for sup elements, graphical and output options and reinitialization
  tkgrid(tklabel(top,text=""))                                                   #blank line
  tkgrid(OptionFrame, column=1, columnspan=1)                                    #options frame
  tkgrid(tklabel(top,text=""))                                                   #blank line
  tkgrid(HcpcFrame, column=1, columnspan=1)                                      #HCPC frame
  tkgrid(tklabel(top,text=""))                                                   #blank line
  tkgrid(appliquer.but, column=1, columnspan=1)                                  #"Apply" button
  tkgrid(tklabel(top,text=""))                                                   #blank line
  tkgrid(buttonsFrame, column=1, columnspan=1, sticky="ew" )                     #OK, Cancel and Help buttons
  tkgrid(tklabel(top,text=""))                                                   #blank line

}

#############################FIN FONCTION FactoPCA #############################


################################################################################
#                              FONCTION FactoCA                                #
################################################################################

#! version JMA le 20/12/2006 16:33:58


FactoCA<-function()
{
  require(tcltk)
  require(FactoMineR)



################################################################################
#    Création des fonctions pour les options via nouvelle fenêtre graphique    #
################################################################################

 #! fonction pour le choix des variables colonnes supplémentaires 
  Cillu.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    variableColIllu<-NULL
    .CilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnCillu<-function()
    { 
      CilluWin<-tktoplevel()
      tkwm.title(CilluWin,gettextRcmdr("Select supplementary columns"))
      #création de la fonction COK.funct
      COK.funct<-function()
      {
        vsup.select<-listvar.nom[as.numeric(tkcurselection(listvar))+1]
        if(length(vsup.select)==0)
        {
          assign("variableColIllu", NULL, envir=env)
          tclvalue(.CilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Cillu.but, fg="black")
          tkdestroy(CilluWin)
          return()
        }
        assign("variableColIllu", vsup.select, envir=env)
#        tclvalue(.CilluLabel)<-paste(label, ": OK", sep=" ")
        tclvalue(.CilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Cillu.but, fg="blue")
        tkdestroy(CilluWin)
      }
      
      # création et mise en page de la fenetre Cillu
      listvar<-tklistbox(CilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrvar,...)) # Liste vide
      scrvar <-tkscrollbar(CilluWin,repeatinterval=5,command=function(...)tkyview(listvar,...)) 
      listvar.nom<-NULL
      indice<-0
      for (i in (1:ncol(donnee))){
        if (is.numeric(donnee[,i])) {
          tkinsert(listvar,"end",vars[i]) # On renseigne la liste
          listvar.nom<-c(listvar.nom,vars[i])
          if(vars[i] %in% variableColIllu) tkselection.set(listvar, indice)
          indice<-indice+1
        }
      }
  
      COK.but<-tkbutton(CilluWin, text="OK", width=16,command=COK.funct)

      tkgrid(tklabel(CilluWin, text=""))
        tkgrid(tklabel(CilluWin, text = gettextRcmdr("Select supplementary column(s)"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
        tkgrid(listvar, scrvar, sticky = "nw")
        tkgrid.configure(scrvar, sticky = "ens", columnspan=1)
        tkgrid.configure(listvar, sticky = "ew", column=1, columnspan=1)
        tkgrid(tklabel(CilluWin, text=""))
        tkgrid(COK.but, column=1,columnspan=1, sticky="ew")
        tkgrid(tklabel(CilluWin, text=""))
        tkgrid.columnconfigure(CilluWin,0, minsize=25)
      tkgrid.columnconfigure(CilluWin,2, minsize=25)
  }  

   CilluFrame<-tkframe(IlluFrame)
   Cillu.but<-tkbutton(CilluFrame, textvariable=.CilluLabel, command=OnCillu, borderwidth=3, width=35)
   tkgrid(Cillu.but, sticky="ew")
  })
  

  #! fonction pour le choix des variables lignes supplémentaires 
  Lillu.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    variableLigneIllu<-NULL
    .LilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnLillu<-function()
    {   
      LilluWin<-tktoplevel()
      tkwm.title(LilluWin,gettextRcmdr("Select supplementary rows"))
      #création de la fonction LOK.funct
      LOK.funct<-function()
      {
        Ligne.select<-rows[as.numeric(tkcurselection(listLigne))+1]
        if(length(Ligne.select)==0)
        {
          assign("individuillu", NULL, envir=env)
          tclvalue(.LilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Lillu.but, fg="black")
          tkdestroy(LilluWin)
          return()
        }
        assign("variableLigneIllu", Ligne.select, envir=env)
#        tclvalue(.LilluLabel)<-paste(label, ": OK", sep=" ")
        tclvalue(.LilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Lillu.but, fg="blue")
        tkdestroy(LilluWin)
      }
      
      # création et mise en page de la fenetre Lillu
      listLigne<-tklistbox(LilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrLigne,...)) # Liste vide
      scrLigne <-tkscrollbar(LilluWin,repeatinterval=5,command=function(...)tkyview(listLigne,...)) 
      indice<-0
      for (i in (1:nrow(donnee)))
      {
          tkinsert(listLigne,"end",rows[i]) # On renseigne la liste
          if(rows[i] %in% variableLigneIllu) tkselection.set(listLigne, indice)
          indice<-indice+1
        }
  
      LOK.but<-tkbutton(LilluWin, text="OK", width=16,command=LOK.funct)

      tkgrid(tklabel(LilluWin, text=""))
        tkgrid(tklabel(LilluWin, text = gettextRcmdr("Select supplementary row(s)"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
        tkgrid(listLigne, scrLigne, sticky = "nw")
        tkgrid.configure(scrLigne, sticky = "ens", columnspan=1)
        tkgrid.configure(listLigne, sticky = "ew", column=1, columnspan=1)
        tkgrid(tklabel(LilluWin, text=""))
        tkgrid(LOK.but, column=1,columnspan=1, sticky="ew")
        tkgrid(tklabel(LilluWin, text=""))
        tkgrid.columnconfigure(LilluWin,0, minsize=25)
      tkgrid.columnconfigure(LilluWin,2, minsize=25)
  }  

   LilluFrame<-tkframe(IlluFrame)
   Lillu.but<-tkbutton(LilluFrame, textvariable=.LilluLabel, command=OnLillu, borderwidth=3, width=35)
   tkgrid(Lillu.but, sticky="ew")
  })



  #! fonction pour la gestion des options graphiques 
  PLOT.CA<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.graph<-0
    #déclaration des variables
    Rchoix<-TRUE
    RTitle<-NULL
    Rlabel<-c("col", "col.sup", "row", "row.sup")
    Rcol.col<-Rcol.col.tmp<-"blue"
    Rcol.col.sup<-Rcol.col.sup.tmp<-"darkblue"
    Rcol.row<-Rcol.row.tmp<-"red"
    Rcol.row.sup<-Rcol.row.sup.tmp<-"darkred"
    Rinvis<-c("")
    RXlimInd<-NULL
    RYlimInd<-NULL
    
    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))
    
    OnPlot<-function()
    {
      PlotWin<-tktoplevel()
      tkwm.title(PlotWin,gettextRcmdr("Outputs"))
      tkwm.geometry(PlotWin, "-100+50")
      
      #création de la fonction onOKsub
      onOKsub<-function()
      {
        assign("compteur.graph", compteur.graph+1, envir=env)
#        if(compteur.graph>0) tclvalue(.PlotLabel)<-paste(label, ": Seen", sep=" ")
        if(compteur.graph>0) tclvalue(.PlotLabel)<-paste(label, "", sep=" ")
        tkconfigure(Plot.but, fg="blue")

        # gestion des entrées de la partie graphique
        if(tclvalue(ind.check.value)==1) assign("Rchoix", TRUE, envir=env)
        else assign("Rchoix", FALSE, envir=env)

        if(Rchoix)
        {
          if (tclvalue(Titre)==" ") assign("RTitle", NULL, envir=env)
          assign("RTitle", tclvalue(Titre), envir=env)

          label.tmp.col<-tclvalue(label.col.checkValue)
          label.tmp.col.sup<-tclvalue(label.col.sup.checkValue)
          label.tmp.row<-tclvalue(label.row.checkValue)
          label.tmp.row.sup<-tclvalue(label.row.sup.checkValue)
          assign("Rlabel", NULL, envir=env)
          if(label.tmp.col==1) assign("Rlabel", c(Rlabel, "col"), envir=env)
          if(label.tmp.col.sup==1) assign("Rlabel", c(Rlabel, "col.sup"), envir=env)
          if(label.tmp.row==1) assign("Rlabel", c(Rlabel, "row"), envir=env)
          if(label.tmp.row.sup==1) assign("Rlabel", c(Rlabel, "row.sup"), envir=env)
          
          invis.tmp.row<-tclvalue(invis.row.checkValue)
          invis.tmp.row.sup<-tclvalue(invis.row.sup.checkValue)
          invis.tmp.col.sup<-tclvalue(invis.col.sup.checkValue)
          invis.tmp.col<-tclvalue(invis.col.checkValue)
          assign("Rinvis", NULL, envir=env)
          if(invis.tmp.row==0) assign("Rinvis", c(Rinvis, "row"), envir=env)
          if(invis.tmp.row.sup==0) assign("Rinvis", c(Rinvis, "row.sup"), envir=env)
          if(invis.tmp.col.sup==0) assign("Rinvis", c(Rinvis, "col.sup"), envir=env)
          if(invis.tmp.col==0) assign("Rinvis", c(Rinvis, "col"), envir=env)

          assign("Rcol.col", Rcol.col.tmp, envir=env)
          assign("Rcol.col.sup", Rcol.col.sup.tmp, envir=env)
          assign("Rcol.row", Rcol.row.tmp, envir=env)
          assign("Rcol.row.sup", Rcol.row.sup.tmp, envir=env)

          if(tclvalue(XlimIndMin)=="" | tclvalue(XlimIndMax)=="") assign("RXlimInd", NULL, envir=env)
          else assign("RXlimInd", c(as.numeric(tclvalue(XlimIndMin)), as.numeric(tclvalue(XlimIndMax))), envir=env)
          if(tclvalue(YlimIndMin)=="" | tclvalue(YlimIndMax)=="") assign("RYlimInd", NULL, envir=env)
          else assign("RYlimInd", c(as.numeric(tclvalue(YlimIndMin)), as.numeric(tclvalue(YlimIndMax))), envir=env)
        }
        
         tkdestroy(PlotWin)
      }
        
      RchoixFrame<-tkframe(PlotWin,borderwidth=2)
      ind.check<-tkcheckbutton(RchoixFrame)
      if(Rchoix) ind.check.value<-tclVar("1")
      else ind.check.value<-tclVar("0")
      tkconfigure(ind.check, variable=ind.check.value)
      tkgrid(tklabel(RchoixFrame, text=gettextRcmdr("Graphical output"), font=font2),ind.check)
      tkgrid(tklabel(RchoixFrame, text="  "))
      
      RTitleFrame<-tkframe(PlotWin,borderwidth=2)
      if (is.null(RTitle)) Titre <- tclVar(" ")
      else Titre<-tclVar(RTitle)
      Titre.entry <-tkentry(RTitleFrame,width="40",textvariable=Titre)
      tkgrid(tklabel(RTitleFrame,text=gettextRcmdr("Title of the graph")),Titre.entry)
        
      RlabelFrame<-tkframe(PlotWin,borderwidth=2)
      tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("  ")),tklabel(RlabelFrame,text=gettextRcmdr("Plot")),tklabel(RlabelFrame, text=gettextRcmdr("Label")))
      label.row.check<-tkcheckbutton(RlabelFrame)
      if ("row" %in% Rlabel) label.row.checkValue<-tclVar("1")
      else label.row.checkValue<-tclVar("0") 
      tkconfigure(label.row.check, variable=label.row.checkValue)
      invis.row.check<-tkcheckbutton(RlabelFrame)
      if ("row" %in% Rinvis) invis.row.checkValue<-tclVar("0")
      else invis.row.checkValue<-tclVar("1") 
      tkconfigure(invis.row.check, variable=invis.row.checkValue)
      tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Active rows")),invis.row.check,label.row.check)
      label.row.sup.check<-tkcheckbutton(RlabelFrame)
      if ("row.sup" %in% Rlabel) label.row.sup.checkValue<-tclVar("1")
      else label.row.sup.checkValue<-tclVar("0")
      tkconfigure(label.row.sup.check, variable=label.row.sup.checkValue)
      invis.row.sup.check<-tkcheckbutton(RlabelFrame)
      if ("row.sup" %in% Rinvis) invis.row.sup.checkValue<-tclVar("0")
      else invis.row.sup.checkValue<-tclVar("1")
      tkconfigure(invis.row.sup.check, variable=invis.row.sup.checkValue)
      if(!is.null(variableLigneIllu)) tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Supplementary rows")), invis.row.sup.check, label.row.sup.check)
      label.col.check<-tkcheckbutton(RlabelFrame)
      if ("col" %in% Rlabel) label.col.checkValue<-tclVar("1")
      else label.col.checkValue<-tclVar("0") 
      tkconfigure(label.col.check, variable=label.col.checkValue)
      invis.col.check<-tkcheckbutton(RlabelFrame)
      if ("col" %in% Rinvis) invis.col.checkValue<-tclVar("0")
      else invis.col.checkValue<-tclVar("1") 
      tkconfigure(invis.col.check, variable=invis.col.checkValue)
      tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Active columns")), invis.col.check,label.col.check)
      label.col.sup.check<-tkcheckbutton(RlabelFrame)
      if ("col.sup" %in% Rlabel) label.col.sup.checkValue<-tclVar("1")
      else label.col.sup.checkValue<-tclVar("0")
      tkconfigure(label.col.sup.check, variable=label.col.sup.checkValue)
      invis.col.sup.check<-tkcheckbutton(RlabelFrame)
      if ("col.sup" %in% Rinvis) invis.col.sup.checkValue<-tclVar("0")
      else invis.col.sup.checkValue<-tclVar("1")
      tkconfigure(invis.col.sup.check, variable=invis.col.sup.checkValue)
      if(!is.null(variableColIllu)) tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Supplementary columns")),invis.col.sup.check,label.col.sup.check)
  
      RcolFrame<-tkframe(PlotWin, borderwidth=2)
      Rcol.row.value <- Rcol.row
      canvas.row <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.row.value)
      ChangeColor.row <- function()
      {
        Rcol.row.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.row.value,title="Select color"))
        if (nchar(Rcol.row.value)>0)
        {
          tkconfigure(canvas.row,bg=Rcol.row.value)
          assign("Rcol.row.tmp", Rcol.row.value, envir=env)
        }
      }
      ChangeColor.row.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.row)
      tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color for active rows")),canvas.row,ChangeColor.row.button, sticky="w")
      
      Rcol.row.sup.value<-Rcol.row.sup
      canvas.row.sup <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.row.sup.value)
      ChangeColor.row.sup <- function()
      {
        Rcol.row.sup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.row.sup.value,title="Select color"))
        if (nchar(Rcol.row.sup.value)>0)
        {
          tkconfigure(canvas.row.sup,bg=Rcol.row.sup.value)
          assign("Rcol.row.sup.tmp", Rcol.row.sup.value, envir=env)
        }
      }
      ChangeColor.row.sup.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.row.sup)
      if(!is.null(variableLigneIllu)) tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color for supplementary rows")),canvas.row.sup,ChangeColor.row.sup.button, sticky="w")
      Rcol.col.value <- Rcol.col
      canvas.col <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.col.value)
      ChangeColor.col <- function()
      {
        Rcol.col.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.col.value,title="Select color"))
        if (nchar(Rcol.col.value)>0)
        {
          tkconfigure(canvas.col,bg=Rcol.col.value)
          assign("Rcol.col.tmp", Rcol.col.value, envir=env)
        }
      }
      ChangeColor.col.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.col)
      tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color for active columns")),canvas.col,ChangeColor.col.button, sticky="w")
      
      Rcol.col.sup.value<-Rcol.col.sup
      canvas.col.sup <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.col.sup.value)
      ChangeColor.col.sup <- function()
      {
        Rcol.col.sup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.col.sup.value,title="Select color"))
        if (nchar(Rcol.col.sup.value)>0)
        {
          tkconfigure(canvas.col.sup,bg=Rcol.col.sup.value)
          assign("Rcol.col.sup.tmp", Rcol.col.sup.value, envir=env)
        }
      }
      ChangeColor.col.sup.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.col.sup)
      if(!is.null(variableColIllu)) tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color for supplementary columns")),canvas.col.sup,ChangeColor.col.sup.button, sticky="w")
                 
      RlimFrame<-tkframe(PlotWin, borderwidth=2)
      if(is.null(RXlimInd)) XlimIndMin<-tclVar("")
      else XlimIndMin<-tclVar(paste(RXlimInd[1]))
      XlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMin)
      if (is.null(RXlimInd)) XlimIndMax<- tclVar("")
      else XlimIndMax<-tclVar(paste(RXlimInd[1]))
      XlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMax)
      tkgrid(tklabel(RlimFrame,text=gettextRcmdr("x limits of the graph:")),XlimIndMin.entry, XlimIndMax.entry)
        if(is.null(RYlimInd)) YlimIndMin<- tclVar("")
      else YlimIndMin<-tclVar(paste(RYlimInd[1]))
      YlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMin)
      if (is.null(RYlimInd)) YlimIndMax<- tclVar("")
      else YlimIndMax<-tclVar(paste(RYlimInd[2]))
        YlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMax)
        tkgrid(tklabel(RlimFrame,text=gettextRcmdr("y limits of the graph:")),YlimIndMin.entry,YlimIndMax.entry)
  
      #mise en page des différents frames de PlotIndFrame
      tkgrid(RchoixFrame)
      tkgrid(tklabel(PlotWin, text=" "))
      tkgrid(RTitleFrame)
      tkgrid(tklabel(PlotWin, text=" "))
      tkgrid(RlabelFrame)
      tkgrid(tklabel(PlotWin, text=" "))
      tkgrid(RcolFrame)
      tkgrid(tklabel(PlotWin, text=" "))
      tkgrid(RlimFrame)
      tkgrid(tklabel(PlotWin, text=" "))
      
      subOKCancelHelp(PlotWin, "plot.CA")
      tkgrid(subButtonsFrame, sticky="ew")
    }
    PlotFrame<-tkframe(IlluFrame)
    Plot.but<-tkbutton(PlotFrame, textvariable=.PlotLabel, command=OnPlot, borderwidth=3, width=35)
    tkgrid(Plot.but, sticky="ew")
  })

    
     #! fonction pour le choix des éléments de sortie
  Sortie.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.sortie<-0
    #déclaration des variables
    Rpropre<-FALSE
    RFichier <- ""
    Rcol<-FALSE
    Rcolsup<-FALSE
    Rrow<-FALSE
    Rrowsup<-FALSE
    Rdescdim<-FALSE

    .SortieLabel<-tclVar(paste(firstLabel, "", sep=" "))

    OnSortie<-function()
    {
      SortieWin<-tktoplevel()
      tkwm.title(SortieWin,"Outputs options")

      #création de la fonction onOKsub
      onOK.sortie<-function()
      {
        assign("compteur.sortie", compteur.sortie+1, envir=env)
#        if(compteur.sortie>0) tclvalue(.SortieLabel)<-paste(label, ": Seen", sep=" ")
        if(compteur.sortie>0) tclvalue(.SortieLabel)<-paste(label, "", sep=" ")
        tkconfigure(Sortie.but, fg="blue")
        
        if(tclvalue(eigValue)=="1") assign("Rpropre", TRUE, envir=env)
        else assign("Rpropre", FALSE, envir=env)
        
        if(tclvalue(colValue)=="1") assign("Rcol", TRUE, envir=env)
        else assign("Rcol", FALSE, envir=env)
        
        if(tclvalue(colsupValue)=="1") assign("Rcolsup", TRUE, envir=env)
        else assign("Rcolsup", FALSE, envir=env)
        
        if(tclvalue(rowValue)=="1") assign("Rrow", TRUE, envir=env)
        else assign("Rrow", FALSE, envir=env)
        
        if(tclvalue(rowsupValue)=="1") assign("Rrowsup", TRUE, envir=env)
        else assign("Rrowsup", FALSE, envir=env)
        
        if(tclvalue(descdimValue)=="1") assign("Rdescdim", TRUE, envir=env)
        else assign("Rdescdim", FALSE, envir=env)

        if (tclvalue(Fichier)=="") assign("RFichier", NULL, envir=env)
        assign("RFichier", tclvalue(Fichier), envir=env)
        
        tkdestroy(SortieWin)
      
      }
      
      eig.lab <-tklabel(SortieWin, text=gettextRcmdr("Eigenvalues"))
        eig.check <- tkcheckbutton(SortieWin)
        if(Rpropre) eigValue <- tclVar("1")
        else eigValue <- tclVar("0")
        tkconfigure(eig.check,variable=eigValue)

      collab<-tklabel(SortieWin,text=gettextRcmdr("Results for column variables"))
        col.check <- tkcheckbutton(SortieWin)
        if(Rcol) colValue <- tclVar("1")
        else colValue <- tclVar("0")
        tkconfigure(col.check,variable=colValue)

      colsup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for column supplementary variables"))
        colsup.check <- tkcheckbutton(SortieWin)

        if(Rcolsup) colsupValue <- tclVar("1")
        else colsupValue <- tclVar("0")
        tkconfigure(colsup.check,variable=colsupValue)

      row.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for row variables"))
        row.check <- tkcheckbutton(SortieWin)
        if(Rrow) rowValue <- tclVar("1")
        else rowValue <- tclVar("0")
        tkconfigure(row.check,variable=rowValue)


      rowsup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for row supplementary variables"))
        rowsup.check <- tkcheckbutton(SortieWin)
        if(Rrowsup) rowsupValue <- tclVar("1")
        else rowsupValue <- tclVar("0")
        tkconfigure(rowsup.check,variable=rowsupValue)
        
        descdim.lab<-tklabel(SortieWin, text=gettextRcmdr("Description of the dimensions"))
      descdim.check<-tkcheckbutton(SortieWin)
      if(Rdescdim) descdimValue<-tclVar("1")
      else descdimValue<-tclVar("0")
      tkconfigure(descdim.check,variable=descdimValue)

      RFichierFrame<-tkframe(SortieWin,borderwidth=2)
      if (is.null(RFichier)) Fichier <- tclVar("")
      else Fichier<-tclVar(RFichier)
      Fichier.entry <-tkentry(RFichierFrame,width="40",textvariable=Fichier)
      tkgrid(tklabel(RFichierFrame,text=gettextRcmdr("Print results on a 'csv' file")),Fichier.entry)

      SortieOK.but<-tkbutton(SortieWin,text="OK",width=16,command=onOK.sortie)

        tkgrid(tklabel(SortieWin, text = gettextRcmdr("Select the outputs options"), fg ="blue"),  columnspan = 2, sticky = "w")
        tkgrid(tklabel(SortieWin, text = " "))
        tkgrid(eig.lab,eig.check,sticky="w")
        tkgrid(collab,col.check,sticky="w")
        if (!is.null(variableColIllu)) tkgrid(colsup.lab,colsup.check,sticky="w")
        tkgrid(row.lab,row.check,sticky="w")
        if (!is.null(variableLigneIllu)) tkgrid(rowsup.lab,rowsup.check,sticky="w")
        tkgrid(tklabel(SortieWin, text = " "))
        tkgrid(descdim.lab,descdim.check,sticky="w")
        tkgrid(RFichierFrame)
        tkgrid(SortieOK.but)
   }
    
    SortieFrame<-tkframe(IlluFrame)
    Sortie.but<-tkbutton(SortieFrame, textvariable=.SortieLabel, command=OnSortie, borderwidth=3, width=35)
    tkgrid(Sortie.but, sticky="ew")
  })


  #! fonction HCPC
  
  Hcpc.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    .HcpcLabel<-tclVar(paste(firstLabel, "", sep=" "))    
    compteur.hcpc<-0
    Rclassif<-0
    RclassifCA<-"rows"
    Rmeth <- -1
    Rconsolid<-0 
    Rgraphhcpc<-1  
    Rreshcpc<-0
    Rminhcpc<-3
    Rmaxhcpc<-10

    OnHCPC <- function()
    {

      HcpcWin<-tktoplevel()
      tkwm.title(HcpcWin, gettextRcmdr("HCPC options"))

      onOKHcpc <- function()
      {
        assign("compteur.hcpc", compteur.hcpc+1, envir=env) 
        if(compteur.hcpc>0) tclvalue(.HcpcLabel)<-paste(label, "", sep=" ")
        tkconfigure(Hcpc.but, fg="blue")
      
        if(tclvalue(classifCAValue)=="rows") assign("RclassifCA", "rows", envir=env)
        else assign("RclassifCA", "columns", envir=env)
      
        if(tclvalue(methValue)=="interactive") assign("Rmeth", 0, envir=env)
        else assign("Rmeth", -1, envir=env)
        
        if(tclvalue(consolidValue)=="1") assign("Rconsolid",TRUE, envir=env)
        else assign("Rconsolid",FALSE,envir=env)
        
        if(tclvalue(graphhcpcValue)=="1") assign("Rgraphhcpc",TRUE,envir=env)
        else assign("Rgraphhcpc",FALSE,envir=env)
        
        if(tclvalue(reshcpcValue)=="1") assign("Rreshcpc",TRUE,envir=env)
        else assign("Rreshcpc",FALSE,envir=env)        
        
        assign("Rminhcpc",as.numeric(tclvalue(minhcpc)),envir=env)
        assign("Rmaxhcpc",as.numeric(tclvalue(maxhcpc)),envir=env)
        
        assign("Rclassif",TRUE,envir=env)
        tkdestroy(HcpcWin)
      }
      OKHcpc.but<-tkbutton(HcpcWin, text="OK", width=8,command=onOKHcpc)

      onCancelHcpc <- function()
      {
        assign("Rclassif",FALSE,envir=env)
        tkdestroy(HcpcWin)
      }
      CancelHcpc.but<-tkbutton(HcpcWin, text="Cancel", width=8,command=onCancelHcpc)
 
      tkgrid(tklabel(HcpcWin, text=""))
      tkgrid(tklabel(HcpcWin, text = gettextRcmdr("Hierarchical Clustering on Principal Components"), fg = "darkred"), column=1, columnspan = 8, sticky = "ew")      

      lignes <- tkradiobutton (HcpcWin)
      lignes.lab <- tklabel(HcpcWin,text=gettextRcmdr("rows"))
      colonnes <- tkradiobutton (HcpcWin)
      colonnes.lab <- tklabel(HcpcWin,text=gettextRcmdr("columns"))
      classifCAValue <- tclVar("rows")
      classifCA.lab <- tklabel(HcpcWin,text=gettextRcmdr("Perform clustering on: "))
      tkconfigure(lignes,variable=classifCAValue,value="rows")
      tkconfigure(colonnes,variable=classifCAValue,value="columns")

      meth1 <- tkradiobutton (HcpcWin)
      meth1.lab <- tklabel(HcpcWin,text=gettextRcmdr("interactive"))
      meth2 <- tkradiobutton (HcpcWin)
      meth2.lab <- tklabel(HcpcWin,text=gettextRcmdr("automatic"))
      methValue <- tclVar("interactive")
      meth.lab <- tklabel(HcpcWin,text=gettextRcmdr("Choice of the number of clusters: "))
      tkconfigure(meth1,variable=methValue,value="interactive")
      tkconfigure(meth2,variable=methValue,value="automatic")

      minmaxhcpc.label<-tklabel(HcpcWin,text=gettextRcmdr("The optimal number of clusters is chosen between:"))

      minhcpc<-tclVar("3")
      maxhcpc<-tclVar("10")
      minhcpc.entry <-tkentry(HcpcWin,width="3",textvariable=minhcpc)
      maxhcpc.entry <-tkentry(HcpcWin,width="3",textvariable=maxhcpc)

      consolid.lab <- tklabel(HcpcWin,text=gettextRcmdr("Consolidate clusters "))
      consolid.check <- tkcheckbutton(HcpcWin)
      if(Rconsolid) consolidValue<-tclVar("1")
      else consolidValue<-tclVar("0")      
      tkconfigure(consolid.check,variable=consolidValue)
      
      graphhcpc.lab <- tklabel(HcpcWin,text=gettextRcmdr("Print graphs "))
      graphhcpc.check <- tkcheckbutton(HcpcWin)
      if(Rgraphhcpc) graphhcpcValue <- tclVar("1")
      else graphhcpcValue <- tclVar("0")
      tkconfigure(graphhcpc.check,variable=graphhcpcValue)   

      reshcpc.lab <- tklabel(HcpcWin,text=gettextRcmdr("Print results for clusters "))
      reshcpc.check <- tkcheckbutton(HcpcWin)
      if(Rreshcpc) reshcpcValue<-tclVar("1")
      else reshcpcValue <- tclVar("0")
      tkconfigure(reshcpc.check,variable=reshcpcValue)          
    
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr("Select options for the HCPC"), fg = "blue"), column=1, columnspan=8, sticky="we")
      tkgrid(tklabel(HcpcWin,text=""))
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr(paste('Clustering is performed on the first ', tclvalue(ncp.val), ' dimensions of the CA',sep=""))),column=1,columnspan=4,sticky="w")
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr("(Change your choice in the main options to change this number)")),column=1,columnspan=4,sticky="w")
      tkgrid(tklabel(HcpcWin,text=""))
      tkgrid(classifCA.lab,lignes.lab,lignes)  
      tkgrid(colonnes.lab,colonnes)
      tkgrid(tklabel(HcpcWin,text=""))
      tkgrid(meth.lab,meth1.lab,meth1)
      tkgrid(meth2.lab,meth2)
      tkgrid(tklabel(HcpcWin,text=""))
      tkgrid(minmaxhcpc.label,minhcpc.entry , maxhcpc.entry)
      tkgrid(tklabel(HcpcWin,text=""))      
      tkgrid(consolid.lab,consolid.check)
      tkgrid(graphhcpc.lab,graphhcpc.check)
      tkgrid(reshcpc.lab,reshcpc.check) 
      tkgrid(tklabel(HcpcWin,text=""))     
      tkgrid(OKHcpc.but, CancelHcpc.but)
      tkgrid(tklabel(HcpcWin, text=""))
      
      tkgrid.configure(minmaxhcpc.label,classifCA.lab,meth.lab,consolid.lab,graphhcpc.lab,reshcpc.lab,column=1,columnspan=4,sticky="w")
      tkgrid.configure(minhcpc.entry,column=7,columnspan=1,sticky="e")
      tkgrid.configure(maxhcpc.entry,column=8,columnspan=1,sticky="w")
      tkgrid.configure(lignes,colonnes,meth1,meth2,consolid.check,graphhcpc.check,reshcpc.check,column=8,sticky="e")
      tkgrid.configure(meth1.lab,lignes.lab,column=6,columnspan=2,sticky="w")
      tkgrid.configure(meth2.lab,colonnes.lab,column=6,columnspan=2,sticky="w") 
      tkgrid.configure(OKHcpc.but,column=2,columnspan=1,sticky="w")
      tkgrid.configure(CancelHcpc.but,column=6,columnspan=1,sticky="e")

      tkgrid.columnconfigure(HcpcWin,0, minsize=3)
      tkgrid.columnconfigure(HcpcWin,5, minsize=5)
      tkgrid.columnconfigure(HcpcWin,8, minsize=3) 

}      
    Hcpc2Frame<-tkframe(HcpcFrame)
    Hcpc.but<-tkbutton(Hcpc2Frame, textvariable=.HcpcLabel, command=OnHCPC, borderwidth=3)
    tkgrid(Hcpc.but, sticky="ew")
})   

  
  #! fonction pour la réinitialisation des paramètre
  Reinitializ.funct<-function()
  {
    tkdestroy(top)
    FactoCA()
  }



  
#! fonction associée au bouton Appliquer, execute sans détruire l'interface graphique
  OnAppliquer<-function()
  {

        # liste de toutes les variables interne créées      (** mise en forme incomplète)
      # sur la fenetre principale
#         listColonne      **
#         listLigne        **
#         resu.val         **
#         ncp.val          **
#         Axe1
#         Axe2
      # dans les boutons des fenêtres illustratives
#         variableColIllu    **
#         variableLigneIllu  **
      # dans le bouton Plot PCA
#         Rchoix
#         RTitle
#         Rlabel
#         Rcol.ind
#         Rcol.ind.sup
#         Rcol.quali
#         Rinvisible       
#         RXlimInd
#         RYlimInd
      # dans le bouton affichage sortie
#         Rpropre
#             Rcol
#           Rcolsup
#           Rrow
#           Rrowsup


    # récupération des paramètres de la fenêtre principale
    nom.res<-tclvalue(resu.val)
    if (length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0) justDoIt(paste('remove (',nom.res,')'))
    if(length(as.numeric(tkcurselection(listColonne)))<2) colActives<-listColonne.nom
    else colActives<-listColonne.nom[as.numeric(tkcurselection(listColonne))+1]
    colActives <- colActives[!(colActives%in%variableColIllu)]
    
    if(length(as.numeric(tkcurselection(listLigne)))<2) rowActives<-listLigne.nom
    else rowActives<-listLigne.nom[as.numeric(tkcurselection(listLigne))+1]
    rowActives <- rowActives[!(rowActives%in%variableLigneIllu)]
    
    Axe<-c(as.numeric(tclvalue(Axe1)), as.numeric(tclvalue(Axe2)))
    
    # gestion du tableau de données pour l'AFC
    if(!is.null(variableColIllu)) {
      if(!is.null(variableLigneIllu)) commande.data<-paste(activeDataSet(),'.CA', '<-', activeDataSet(), '[c("', paste(rowActives, collapse='", "'), '", "', paste(variableLigneIllu, collapse='", "'), '") ,c("', paste(colActives, collapse='", "'), '", "', paste(variableColIllu, collapse='", "'), '")]', sep="")
      else  commande.data<-paste(activeDataSet(),'.CA', '<-', activeDataSet(), '[c("', paste(rowActives, collapse='", "'), '") ,c("', paste(colActives, collapse='", "'), '", "', paste(variableColIllu, collapse='", "'), '")]', sep="")
    }
    else {
      if(!is.null(variableLigneIllu)) commande.data<-paste(activeDataSet(),'.CA', '<-', activeDataSet(), '[c("', paste(rowActives, collapse='", "'), '", "', paste(variableLigneIllu, collapse='", "'), '") ,c("', paste(colActives, collapse='", "'), '")]', sep="")
      else commande.data<-paste(activeDataSet(),'.CA', '<-', activeDataSet(), '[c("', paste(rowActives, collapse='", "'), '") ,c("', paste(colActives, collapse='", "'), '")]', sep="")
    }
    justDoIt(commande.data)
    logger(commande.data)
    donnee.depart<-activeDataSet()
    activeDataSet(paste(activeDataSet(),'.', 'CA', sep=""))
    
    # gestion de la commande réalisant l'AFC
    ncp<-as.numeric(tclvalue(ncp.val))
   
    if(!is.null(variableColIllu)) {
      if(!is.null(variableLigneIllu)) commande.ca<-paste(nom.res, '<-CA(', activeDataSet(), ', ncp=', ncp, ', row.sup=c(', nrow(get(.activeDataSet))-length(variableLigneIllu)+1, ': ', nrow(get(.activeDataSet)), '), col.sup=c(', ncol(get(.activeDataSet))-length(variableColIllu)+1, ': ', ncol(get(.activeDataSet)), '), graph = FALSE)', sep="")
      else commande.ca<-paste(nom.res, '<-CA(', activeDataSet(), ', ncp=', ncp, ', row.sup=NULL, col.sup=c(', ncol(get(.activeDataSet))-length(variableColIllu)+1, ': ', ncol(get(.activeDataSet)), '), graph = FALSE)', sep="")
    }
    else {
      if(!is.null(variableLigneIllu)) commande.ca<-paste(nom.res, '<-CA(', activeDataSet(), ', ncp=', ncp, ', row.sup=c(', nrow(get(.activeDataSet))-length(variableLigneIllu)+1, ': ', nrow(get(.activeDataSet)), '), col.sup=NULL, graph = FALSE)', sep="")
      else commande.ca<-paste(nom.res, '<-CA(', activeDataSet(), ', ncp=', ncp, ', row.sup=NULL, col.sup=NULL, graph = FALSE)', sep="")
    }
    justDoIt(commande.ca)
    logger(commande.ca)

    # gestion des graphiques
    if (length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0) {if (get(nom.res)$eig[1,2]==100) doItAndPrint(paste('"No graph can be plot: data are unidimensional"'))}
    if((Rchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)){
    if (get(nom.res)$eig[1,2]!=100) {
      commande.plot<-paste('plot.CA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), col.row="', Rcol.row, '", col.col="', Rcol.col, '", label=c("', paste(Rlabel, collapse='", "'), '")', sep="")
      if (!is.null(RXlimInd)) commande.plot<-paste(commande.plot, ', xlim=c(', paste(RXlimInd, collapse=", "), ')')
      if (!is.null(RYlimInd)) commande.plot<-paste(commande.plot, ', ylim=c(', paste(RYlimInd, collapse=", "), ')')
      if (!is.null(Rinvis)) commande.plot<-paste(commande.plot, ', invisible=c("', paste(Rinvis, collapse='", "'), '")',sep='')
      if(!is.null(variableLigneIllu)) commande.plot<-paste(commande.plot, ', col.row.sup="', Rcol.row.sup, '"',sep='')
      if(!is.null(variableColIllu))  commande.plot<-paste(commande.plot, ', col.col.sup="', Rcol.col.sup, '"',sep='')
      if (is.null(RTitle)) commande.plot <- paste(commande.plot,')', sep="")
      else {
        if (RTitle ==" ") commande.plot <- paste(commande.plot,')', sep="")
        else commande.plot <- paste(commande.plot,', title="', RTitle,'")', sep="")
      }
      justDoIt(commande.plot)
      logger(commande.plot)
    }}
    
    
    #Commande de la fonction HCPC

    if(Rclassif==TRUE){
      commande.hcpc<-paste(nom.res,'.hcpc', '<-HCPC(', nom.res, ' ,nb.clust=', Rmeth, ',consol=', Rconsolid,',min=', Rminhcpc,',max=',Rmaxhcpc,',cluster.CA="',RclassifCA,'",graph=', Rgraphhcpc, ')', sep="")
    justDoIt(commande.hcpc)
    logger(commande.hcpc)      
      if(Rreshcpc==TRUE){
        doItAndPrint(paste(nom.res,'.hcpc$data.clust[,ncol(res.hcpc$data.clust),drop=F]', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.var', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.axes', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.ind', sep=""))
      }        
    }    
    
    # gestion de l'édition de certains resultats
    if (RFichier==""){
      if(Rpropre) doItAndPrint(paste(nom.res, '$eig', sep=""))
      if(Rcol) doItAndPrint(paste(nom.res, '$col', sep=""))
      if(Rcolsup & !is.null(variableColIllu))doItAndPrint(paste(nom.res, '$col.sup', sep=""))
      if(Rrow) doItAndPrint(paste(nom.res, '$row', sep=""))
      if(Rrowsup & !is.null(variableLigneIllu)) doItAndPrint(paste(nom.res, '$row.sup', sep=""))
      if(Rdescdim) doItAndPrint(paste('dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '))', sep=""))
    }
    else {
      Fich = RFichier
      if (substr(Fich,1,1)!='"') Fich = paste('"',Fich,sep='')
      if (substr(Fich,nchar(Fich),nchar(Fich))!='"') Fich = paste(Fich,'"',sep='')
      append = FALSE
      if(Rpropre){
        doItAndPrint(paste('write.infile(', nom.res, '$eig, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rcol){
        doItAndPrint(paste('write.infile(', nom.res, '$col, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rcolsup){
        doItAndPrint(paste('write.infile(', nom.res, '$col.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rrow){
        doItAndPrint(paste('write.infile(', nom.res, '$row, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rrowsup){
        doItAndPrint(paste('write.infile(', nom.res, '$row.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rdescdim) doItAndPrint(paste('write.infile(dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), ')), file =',Fich,',append=',append,')', sep=""))
    }

    # Re-chargement du tableau de départ
    activeDataSet(donnee.depart)
    justDoIt(paste('remove(',activeDataSet(),'.CA)',sep=""))
    logger(paste('remove(',activeDataSet(),'.CA)',sep=""))    
  }
  
  #! fonction associée au bouton OK, execute et détruit l'interface graphique
  onOK<-function()
  {
    OnAppliquer()   
    # destuction de la fenêtre Top
    tkdestroy(top)
  
  }


################################################################################
#                   Création de la fenêtre top                                 #
################################################################################
  top<-tktoplevel(borderwidth=10)
  tkwm.title(top,gettextRcmdr("CA"))
  tkwm.geometry(top, "-100+50")
        
  # définition des polices
  font2<-tkfont.create(family="times",size=12,weight="bold")
  fontheading<-tkfont.create(family="times",size=18,weight="bold")

  # récupération du jeu de données actif
  donnee<-get(.activeDataSet)
  vars<-colnames(donnee)
  rows<-rownames(donnee)

  # création du frame contenant les listes colonnes et lignes
  ListeFrame<- tkframe(top, borderwidth=2)
    # liste des variables colonnes
  listColonne<-tklistbox(ListeFrame,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrColonne,...))
  scrColonne <-tkscrollbar(ListeFrame,repeatinterval=5,command=function(...)tkyview(listColonne,...))
  listColonne.nom<-NULL
  for (i in (1:ncol(donnee))) {
      if (is.numeric(donnee[,i])) {
          tkinsert(listColonne,"end",vars[i])
          listColonne.nom<-c(listColonne.nom, vars[i])
      }
  }
    # liste des variables lignes
  listLigne<-tklistbox(ListeFrame,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrLigne,...))
  scrLigne <-tkscrollbar(ListeFrame,repeatinterval=5,command=function(...)tkyview(listLigne,...))
  listLigne.nom<-NULL
  for (i in (1:nrow(donnee))) {
      tkinsert(listLigne,"end",rows[i])
        listLigne.nom<-c(listLigne.nom, rows[i])
  }
    # mise en forme de ListeFrame
  tkgrid(tklabel(ListeFrame, text = gettextRcmdr("Select the active rows and the active columns. \nBy default all rows and all columns are active"),fg = "darkred"), columnspan=5, sticky = "ew")
  tkgrid(listLigne, scrLigne, listColonne, scrColonne)
  tkgrid.configure(scrColonne, column=4, sticky="wns")
  tkgrid.configure(scrLigne, column=1, sticky="wns")
  tkgrid.configure(listLigne, sticky = "ew", column=0, columnspan=1)
  tkgrid.configure(listColonne, sticky = "ew", column=3, columnspan=1)
  tkgrid.columnconfigure(ListeFrame,2, minsize=95)
  
  
  # création de tous les boutons d'options dans IlluFrame
  IlluFrame<- tkframe(top, borderwidth=2)
  Reinitializ.but<-tkbutton(IlluFrame, text=gettextRcmdr("Restart"),width=35,command=Reinitializ.funct, borderwidth=3)
  Cillu.funct(label=gettextRcmdr("Modify supplementary column variables"), firstLabel=gettextRcmdr("Select supplementary column variables"))
  Lillu.funct(label=gettextRcmdr("Modify supplementary row variables"), firstLabel=gettextRcmdr("Select supplementary row variables"))    
  PLOT.CA(label=gettextRcmdr("Graphical options"), firstLabel=gettextRcmdr("Graphical options"))
  Sortie.funct(label=gettextRcmdr("Outputs"), firstLabel=gettextRcmdr("Outputs"))
  tkgrid(LilluFrame, CilluFrame)
  tkgrid(tklabel(IlluFrame, text=""))
  tkgrid(PlotFrame, SortieFrame)
  tkgrid(tklabel(IlluFrame, text=""))
  tkgrid(Reinitializ.but)  
  tkgrid(tklabel(IlluFrame, text=""))
  tkgrid.configure(LilluFrame, PlotFrame, Reinitializ.but, column=1, columnspan=2, sticky="ew")
  tkgrid.configure(CilluFrame, SortieFrame, column=4, columnspan=2, sticky="ew")
  tkgrid.columnconfigure(IlluFrame,0, minsize=25)
  tkgrid.columnconfigure(IlluFrame,6, minsize=25)
  tkgrid.columnconfigure(IlluFrame,3, minsize=35)  

  #Frame pour HCPC
  HcpcFrame<-tkframe(top, borderwidth=2)
  Hcpc.funct(label=gettextRcmdr("Perform Clustering after CA"), firstLabel=gettextRcmdr("Perform Clustering after CA")) 
  tkgrid(Hcpc2Frame, columnspan=7)
  tkgrid.configure(Hcpc2Frame,column=4, columnspan=1)

  # création des options dans OptionFrame  
  OptionFrame<-tkframe(top, borderwidth=2, relief="groove")
  resu.lab<-tklabel(OptionFrame,text=gettextRcmdr("Name of the result object: "))
  resu.val<-tclVar("res")
  resu<-tkentry(OptionFrame,width=10,textvariable=resu.val)
  ncp.lab<-tklabel(OptionFrame,text=gettextRcmdr("Number of dimensions: "))
  ncp.val<-tclVar("5") 
  ncp<-tkentry(OptionFrame,width=5,textvariable=ncp.val)
  Axe.label<-tklabel(OptionFrame,text=gettextRcmdr("Graphical output: select the dimensions:"))
  Axe1<-tclVar("1")
  Axe2<-tclVar("2")
  Axe1.entry <-tkentry(OptionFrame,width="5",textvariable=Axe1)
  Axe2.entry <-tkentry(OptionFrame,width="5",textvariable=Axe2)
  
  
    # mise en page de OptionFrame
  tkgrid(tklabel(OptionFrame,text=gettextRcmdr("Main options"), fg = "darkred"), columnspan=8, sticky="we") 
  tkgrid(tklabel(OptionFrame,text="")) # Ligne de blanc
  tkgrid(resu.lab, resu)
  tkgrid(ncp.lab, ncp)
  tkgrid(Axe.label,Axe1.entry , Axe2.entry, sticky="w")
  tkgrid.configure(ncp.lab, resu.lab, Axe.label, column=1, columnspan=4, sticky="w")
  tkgrid.configure(ncp, resu, column=6, columnspan=2, sticky="e")
  tkgrid.configure(Axe1.entry, column=6, columnspan=1, sticky="w")
  tkgrid.configure(Axe2.entry, column=7, columnspan=1, sticky="e")
  tkgrid.columnconfigure(OptionFrame,0, minsize=25)
  tkgrid.columnconfigure(OptionFrame,5, minsize=40)
  tkgrid.columnconfigure(OptionFrame,8, minsize=25)

  appliquer.but<-tkbutton(top, text="Apply",width=12,command=OnAppliquer, borderwidth=3, fg="#690f96")
  OKCancelHelp(helpSubject="CA")

  # Mise en page de top
  tkgrid(tklabel(top, text=gettextRcmdr("Correspondence Analysis (CA)"),font=fontheading), columnspan=3)
  tkgrid(tklabel(top,text=""))
  tkgrid(ListeFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text=""))
  tkgrid(IlluFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text=""))
  tkgrid(OptionFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc
  tkgrid(HcpcFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text=""))  
  tkgrid(appliquer.but, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc
  tkgrid(buttonsFrame, column=1, columnspan=1, sticky="ew" )
  tkgrid(tklabel(top,text="")) # Ligne de blanc
  
}  

#############################FIN FONCTION FactoCA ##############################
############################# FONCTION FactoMCA ##############################
FactoMCA<-function()
{
  require(tcltk)
  require(FactoMineR)


#    Création des fonctions pour les options via nouvelle fenêtre graphique


  #! fonction pour le choix des variables qualitatives supplémentaires
  Fillu.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    variablefact<-NULL
    .FilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    .factors<-Factors()
    OnFillu<-function()
    {
      if(length(.factors)==0) errorCondition(recall=NULL, message=gettextRcmdr("No Factor available"))

      FilluWin<-tktoplevel()
      tkwm.title(FilluWin,gettextRcmdr("Choice of supplementary factors"))
      #création de la fonction FOK.funct
      FOK.funct<-function()
      {
        fact.select<-listfact.nom[as.numeric(tkcurselection(listfact))+1]
        if(length(fact.select)==0)
        {
          assign("variablefact", NULL, envir=env)
          tclvalue(.FilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Fillu.but, fg="black")
          tkdestroy(FilluWin)
          return()
        }
        assign("variablefact", fact.select, envir=env)
        tclvalue(.FilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Fillu.but, fg="blue")
        tkdestroy(FilluWin)
      }

      # création et mise en page de la fenetre Fillu
      listfact<-tklistbox(FilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrfact,...)) # Liste vide
      scrfact <-tkscrollbar(FilluWin,repeatinterval=5,command=function(...)tkyview(listfact,...))
      listfact.nom<-NULL
      indice<-0
      for (i in (1:ncol(donnee))) {
          if (is.factor(donnee[,i])) {
            tkinsert(listfact,"end",vars[i]) # On renseigne la liste
            listfact.nom<-c(listfact.nom,vars[i])
            if(vars[i] %in% variablefact) tkselection.set(listfact, indice)
            indice<-indice+1
          }
      }

      FOK.but<-tkbutton(FilluWin, text="OK", width=16,command=FOK.funct)

      tkgrid(tklabel(FilluWin, text=""))
      tkgrid(tklabel(FilluWin, text = gettextRcmdr("Select supplementary factor(s)"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
      tkgrid(listfact, scrfact, sticky = "nw")
      tkgrid.configure(scrfact, sticky = "ens", columnspan=1)
      tkgrid.configure(listfact, sticky = "ew", column=1, columnspan=1)
      tkgrid(tklabel(FilluWin, text=""))
      tkgrid(FOK.but, column=1,columnspan=1, sticky="ew")
      tkgrid(tklabel(FilluWin, text=""))
      tkgrid.columnconfigure(FilluWin,0, minsize=25)
      tkgrid.columnconfigure(FilluWin,2, minsize=25)
  }

   FilluFrame<-tkframe(IlluFrame)
   Fillu.but<-tkbutton(FilluFrame, textvariable=.FilluLabel, command=OnFillu, borderwidth=3)
   tkgrid(Fillu.but, sticky="ew")
  })

  #! fonction pour le choix des variables quantitatives supplémentaires
  Dillu.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    variableillu<-NULL
    .DilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnDillu<-function()
    {
      DilluWin<-tktoplevel()
      tkwm.title(DilluWin,gettextRcmdr("Select supplementary variables"))
      #création de la fonction DOK.funct
      DOK.funct<-function()
      {
        vsup.select<-listvar.nom[as.numeric(tkcurselection(listvar))+1]
        if(length(vsup.select)==0)
        {
          assign("variableillu", NULL, envir=env)
          tclvalue(.DilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Dillu.but, fg="black")
          tkdestroy(DilluWin)
          return()
        }
        assign("variableillu", vsup.select, envir=env)
        tclvalue(.DilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Dillu.but, fg="blue")
        tkdestroy(DilluWin)
      }

      # création et mise en page de la fenetre Dillu
      listvar<-tklistbox(DilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrvar,...)) # Liste vide
      scrvar <-tkscrollbar(DilluWin,repeatinterval=5,command=function(...)tkyview(listvar,...))
      listvar.nom<-NULL
      indice<-0
      for (i in (1:ncol(donnee))) {
          if (is.numeric(donnee[,i])) {
            tkinsert(listvar,"end",vars[i]) # On renseigne la liste
            listvar.nom<-c(listvar.nom,vars[i])
            if(vars[i] %in% variableillu) tkselection.set(listvar, indice)
            indice<-indice+1
          }
      }

      DOK.but<-tkbutton(DilluWin, text="OK", width=16,command=DOK.funct)

      tkgrid(tklabel(DilluWin, text=""))
      tkgrid(tklabel(DilluWin, text = gettextRcmdr("Select supplementary variable(s)"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
      tkgrid(listvar, scrvar, sticky = "nw")
      tkgrid.configure(scrvar, sticky = "ens", columnspan=1)
      tkgrid.configure(listvar, sticky = "ew", column=1, columnspan=1)
      tkgrid(tklabel(DilluWin, text=""))
      tkgrid(DOK.but, column=1,columnspan=1, sticky="ew")
      tkgrid(tklabel(DilluWin, text=""))
      tkgrid.columnconfigure(DilluWin,0, minsize=25)
      tkgrid.columnconfigure(DilluWin,2, minsize=25)
  }

   DilluFrame<-tkframe(IlluFrame) 
   if(length(listNumeric())==0){
     Dillu.but<-tkbutton(DilluFrame, text=gettextRcmdr("No quantitative variable available"), borderwidth=3)
     tkconfigure(Dillu.but, fg="grey")
   }
   else Dillu.but<-tkbutton(DilluFrame, textvariable=.DilluLabel, command=OnDillu, borderwidth=3)
   tkgrid(Dillu.but, sticky="ew")
  })


  #! fonction pour le choix des individus supplémentaires
  Iillu.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    individuillu<-NULL
    .IilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnIillu<-function()
    {
      IilluWin<-tktoplevel()
      tkwm.title(IilluWin,gettextRcmdr("Select supplementary individuals"))
      #création de la fonction IOK.funct
      IOK.funct<-function()
      {
        ind.select<-rows[as.numeric(tkcurselection(listind))+1]
        if(length(ind.select)==0) {
          assign("individuillu", NULL, envir=env)
          tclvalue(.IilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Iillu.but, fg="black")
          tkdestroy(IilluWin)
          return()
        }
        assign("individuillu", ind.select, envir=env)
        tclvalue(.IilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Iillu.but, fg="blue")
        tkdestroy(IilluWin)
      }

      # création et mise en page de la fenetre Fillu
      listind<-tklistbox(IilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrind,...)) # Liste vide
      scrind <-tkscrollbar(IilluWin,repeatinterval=5,command=function(...)tkyview(listind,...))
      indice<-0
      for (i in (1:nrow(donnee))) {
          tkinsert(listind,"end",rows[i]) # On renseigne la liste
          if(rows[i] %in% individuillu) tkselection.set(listind,indice)
          indice<-indice+1
        }

      IOK.but<-tkbutton(IilluWin, text="OK", width=16,command=IOK.funct)

      tkgrid(tklabel(IilluWin, text=""))
        tkgrid(tklabel(IilluWin, text = gettextRcmdr("Select supplementary individual(s)"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
        tkgrid(listind, scrind, sticky = "nw")
        tkgrid.configure(scrind, sticky = "ens", columnspan=1)
        tkgrid.configure(listind, sticky = "ew", column=1, columnspan=1)
        tkgrid(tklabel(IilluWin, text=""))
        tkgrid(IOK.but, column=1,columnspan=1, sticky="ew")
        tkgrid(tklabel(IilluWin, text=""))
        tkgrid.columnconfigure(IilluWin,0, minsize=25)
      tkgrid.columnconfigure(IilluWin,2, minsize=25)
  }

   IilluFrame<-tkframe(IlluFrame)
   Iillu.but<-tkbutton(IilluFrame, textvariable=.IilluLabel, command=OnIillu, borderwidth=3)
   tkgrid(Iillu.but, sticky="ew")
  })


  #! fonction pour la gestion des options graphiques
  PLOT.MCA<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.graph<-0
    #déclaration des variables
    Rchoix<-TRUE
    RTitle<-NULL
    Rlabel<-c("ind", "ind.sup", "quali.sup","var","quanti.sup")
    Rinvis<- ""
    Rcol.ind<-Rcol.ind.tmp<-"black"
    Rcol.ind.sup<-Rcol.ind.sup.tmp<-"blue"
    Rcol.quali<-Rcol.quali.tmp<-"darkred"
    Rcol.qualisup<-Rcol.qualisup.tmp<-"darkgreen"
    RXlimInd<-NULL
    RYlimInd<-NULL

    Wchoix=TRUE
    WTitle<-NULL
    Wlabel<-c("quanti.sup")
    #Wlim.cos<-0.
    Wcol.quanti.sup<-Wcol.quanti.sup.tmp<-"blue"
    Wcol.var<-Wcol.var.tmp<-"black"
    WXlimVar<-NULL
    WYlimVar<-NULL

    Vchoix=TRUE
    VTitle<-NULL
    Vlabel<-c("var","quali.sup")
    Vinvis<- ""
    Vcol.quali.sup<-Vcol.quali.sup.tmp<-"darkgreen"
    Vcol.var<-Vcol.var.tmp<-"darkred"
    VXlimVar<-NULL
    VYlimVar<-NULL

    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))

    OnPlot<-function()
    {
      PlotWin<-tktoplevel()
      tkwm.title(PlotWin, gettextRcmdr("Select graphical options"))
      tkwm.geometry(PlotWin, "-100+50")

      #création de la fonction onOKsub
      onOKsub<-function()
      {
        assign("compteur.graph", compteur.graph+1, envir=env)
        if(compteur.graph>0) tclvalue(.PlotLabel)<-paste(label, "", sep=" ")
        tkconfigure(Plot.but, fg="blue")

        # gestion des entrées de la partie graphique des individus
        if(tclvalue(ind.check.value)==1) assign("Rchoix", TRUE, envir=env)
        else assign("Rchoix", FALSE, envir=env)

        if(Rchoix)
        {
          if (tclvalue(Titre)==" ") assign("RTitle", NULL, envir=env)
          assign("RTitle", tclvalue(Titre), envir=env)

          label.tmp.ind<-tclvalue(label.ind.checkValue)
          label.tmp.ind.sup<-tclvalue(label.ind.sup.checkValue)
          label.tmp.quali.sup<-tclvalue(label.quali.sup.checkValue)
          label.tmp.var<-tclvalue(label.var.checkValue)
          label.tmp.quanti.sup<-tclvalue(label.quanti.sup.checkValue)
          assign("Rlabel", NULL, envir=env)
          if(label.tmp.ind==1) assign("Rlabel", c(Rlabel, "ind"), envir=env)
          if(label.tmp.ind.sup==1) assign("Rlabel", c(Rlabel, "ind.sup"), envir=env)
          if(label.tmp.quali.sup==1) assign("Rlabel", c(Rlabel, "quali.sup"), envir=env)
          if(label.tmp.var==1) assign("Rlabel", c(Rlabel, "var"), envir=env)
          if(label.tmp.quanti.sup==1) assign("Wlabel", c(Rlabel, "quanti.sup"), envir=env)

          invis.tmp.ind<-tclvalue(invis.ind.checkValue)
          invis.tmp.ind.sup<-tclvalue(invis.ind.sup.checkValue)
          invis.tmp.quali.sup<-tclvalue(invis.quali.sup.checkValue)
          invis.tmp.var<-tclvalue(invis.var.checkValue)
          assign("Rinvis", NULL, envir=env)
          if(invis.tmp.ind==0) assign("Rinvis", c(Rinvis, "ind"), envir=env)
          if(invis.tmp.ind.sup==0) assign("Rinvis", c(Rinvis, "ind.sup"), envir=env)
          if(invis.tmp.quali.sup==0) assign("Rinvis", c(Rinvis, "quali.sup"), envir=env)
          if(invis.tmp.var==0) assign("Rinvis", c(Rinvis, "var"), envir=env)

          assign("Rcol.ind", Rcol.ind.tmp, envir=env)
          assign("Rcol.ind.sup", Rcol.ind.sup.tmp, envir=env)
          assign("Rcol.quali", Rcol.quali.tmp, envir=env)
          assign("Rcol.qualisup", Rcol.qualisup.tmp, envir=env)
          assign("Wcol.var", Wcol.var.tmp, envir=env)

          if(tclvalue(XlimIndMin)=="" | tclvalue(XlimIndMax)=="") assign("RXlimInd", NULL, envir=env)
          else assign("RXlimInd", c(as.numeric(tclvalue(XlimIndMin)), as.numeric(tclvalue(XlimIndMax))), envir=env)
          if(tclvalue(YlimIndMin)=="" | tclvalue(YlimIndMax)=="") assign("RYlimInd", NULL, envir=env)
          else assign("RYlimInd", c(as.numeric(tclvalue(YlimIndMin)), as.numeric(tclvalue(YlimIndMax))), envir=env)
        }

        if(tclvalue(quanti.var.check.value)==1) assign("Wchoix", TRUE, envir=env)
        else assign("Wchoix", FALSE, envir=env)

        if(Wchoix)
        {
          if (tclvalue(WTitre)==" ") assign("WTitle", NULL, envir=env)
          assign("WTitle", tclvalue(WTitre), envir=env)
          #assign("Wlim.cos", tclvalue(WlimCosValue), envir=env)
          label.tmp.quanti.sup<-tclvalue(label.quanti.sup.checkValue)
          assign("Wlabel", NULL, envir=env)
          if(label.tmp.quanti.sup==1) assign("Wlabel", c(Wlabel, "quanti.sup"), envir=env)
          assign("Wcol.quanti.sup", Wcol.quanti.sup.tmp, envir=env)
               
        }

        if(tclvalue(var.check.value)==1) assign("Vchoix", TRUE, envir=env)
        else assign("Vchoix", FALSE, envir=env)

        if(Vchoix)
        {
          if (tclvalue(VTitre)==" ") assign("VTitle", NULL, envir=env)
          assign("VTitle", tclvalue(VTitre), envir=env)

          labelV.tmp.quali.sup<-tclvalue(labelV.quali.sup.checkValue)
          labelV.tmp.var<-tclvalue(labelV.var.checkValue)
          assign("Vlabel", NULL, envir=env)
          if(labelV.tmp.quali.sup==1) assign("Vlabel", c(Vlabel, "quali.sup"), envir=env)
          if(labelV.tmp.var==1) assign("Vlabel", c(Vlabel, "var"), envir=env)

          invisV.tmp.quali.sup<-tclvalue(invisV.quali.sup.checkValue)
          invisV.tmp.var<-tclvalue(invisV.var.checkValue)
          assign("Vinvis", NULL, envir=env)
          if(invisV.tmp.quali.sup==0) assign("Vinvis", c(Vinvis, "quali.sup"), envir=env)
          if(invisV.tmp.var==0) assign("Vinvis", c(Vinvis, "var"), envir=env)

          assign("Vcol.quali.sup", Vcol.quali.sup.tmp, envir=env)
          assign("Vcol.var", Vcol.var.tmp, envir=env)
               
        }
        tkdestroy(PlotWin)
      }

    # construction de la partie graphique des individus
    PlotIndFrame<-tkframe(PlotWin, borderwidth=5, relief="groove")

    RchoixFrame<-tkframe(PlotIndFrame,borderwidth=2)
    ind.check<-tkcheckbutton(RchoixFrame)
    if(Rchoix) ind.check.value<-tclVar("1")
    else ind.check.value<-tclVar("0")
    tkconfigure(ind.check, variable=ind.check.value)
    tkgrid(tklabel(RchoixFrame, text=gettextRcmdr("Graphical output"), font=font2),ind.check)
    tkgrid(tklabel(RchoixFrame, text="  "))

    RTitleFrame<-tkframe(PlotIndFrame,borderwidth=2)
    if (is.null(RTitle)) Titre <- tclVar(" ")
    else Titre<-tclVar(RTitle)
    Titre.entry <-tkentry(RTitleFrame,width="40",textvariable=Titre)
    tkgrid(tklabel(RTitleFrame,text=gettextRcmdr("Title of the graph")),Titre.entry)

    RlabelFrame<-tkframe(PlotIndFrame,borderwidth=2)
    tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("  ")),tklabel(RlabelFrame,text=gettextRcmdr("Plot")),tklabel(RlabelFrame, text=gettextRcmdr("Label")))
    label.ind.check<-tkcheckbutton(RlabelFrame)
    if ("ind" %in% Rlabel) label.ind.checkValue<-tclVar("1")
    else label.ind.checkValue<-tclVar("0")
    invis.ind.check<-tkcheckbutton(RlabelFrame)
    if ("ind" %in% Rinvis) invis.ind.checkValue<-tclVar("0")
    else invis.ind.checkValue<-tclVar("1")
    tkconfigure(label.ind.check, variable=label.ind.checkValue)
    tkconfigure(invis.ind.check, variable=invis.ind.checkValue)
    tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Active individuals")), invis.ind.check,label.ind.check)
    label.ind.sup.check<-tkcheckbutton(RlabelFrame)
    if ("ind.sup" %in% Rlabel) label.ind.sup.checkValue<-tclVar("1")
    else label.ind.sup.checkValue<-tclVar("0")
    invis.ind.sup.check<-tkcheckbutton(RlabelFrame)
    if ("ind.sup" %in% Rinvis) invis.ind.sup.checkValue<-tclVar("0")
    else invis.ind.sup.checkValue<-tclVar("1")
    tkconfigure(label.ind.sup.check, variable=label.ind.sup.checkValue)
    tkconfigure(invis.ind.sup.check, variable=invis.ind.sup.checkValue)
    if(!is.null(individuillu)) tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Supplementary individuals")),invis.ind.sup.check,label.ind.sup.check)
    label.var.check<-tkcheckbutton(RlabelFrame)
    if ("var" %in% Rlabel) label.var.checkValue<-tclVar("1")
    else label.var.checkValue<-tclVar("0")
    invis.var.check<-tkcheckbutton(RlabelFrame)
    if ("var" %in% Rinvis) invis.var.checkValue<-tclVar("0")
    else invis.var.checkValue<-tclVar("1")
    tkconfigure(label.var.check, variable=label.var.checkValue)
    tkconfigure(invis.var.check, variable=invis.var.checkValue)
    tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Factors")),invis.var.check, label.var.check)
    label.quali.sup.check<-tkcheckbutton(RlabelFrame)
    if ("quali.sup" %in% Rlabel) label.quali.sup.checkValue<-tclVar("1")
    else label.quali.sup.checkValue<-tclVar("0")
    invis.quali.sup.check<-tkcheckbutton(RlabelFrame)
    if ("quali.sup" %in% Rinvis) invis.quali.sup.checkValue<-tclVar("0")
    else invis.quali.sup.checkValue<-tclVar("1")
    tkconfigure(label.quali.sup.check, variable=label.quali.sup.checkValue)
    tkconfigure(invis.quali.sup.check, variable=invis.quali.sup.checkValue)
    if(!is.null(variablefact)) tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Supplementary factors")),invis.quali.sup.check, label.quali.sup.check)
##    label.quanti.sup.check<-tkcheckbutton(RlabelFrame)
##    if ("quanti.sup" %in% Rlabel) label.quanti.sup.checkValue<-tclVar("1")
##    else label.quanti.sup.checkValue<-tclVar("0")
##    invis.quanti.sup.check<-tkcheckbutton(RlabelFrame)
##    if ("quanti.sup" %in% Rinvis) invis.quanti.sup.checkValue<-tclVar("0")
##    else invis.quanti.sup.checkValue<-tclVar("1")
##    tkconfigure(label.quanti.sup.check, variable=label.quanti.sup.checkValue)
##    tkconfigure(invis.quanti.sup.check, variable=invis.quanti.sup.checkValue)
##    tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Quantitative supplementary variables")),invis.quanti.sup.check, label.quanti.sup.check)

    RcolFrame<-tkframe(PlotIndFrame,borderwidth=2)
    Rcol.ind.value <- Rcol.ind
    canvas.ind <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.ind.value)
    ChangeColor.ind <- function()
    {
      Rcol.ind.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.ind.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Rcol.ind.value)>0)
      {
        tkconfigure(canvas.ind,bg=Rcol.ind.value)
        assign("Rcol.ind.tmp", Rcol.ind.value, envir=env)
      }
    }
    ChangeColor.ind.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.ind)
    tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color for active individuals")),canvas.ind,ChangeColor.ind.button)

    Rcol.ind.sup.value<-Rcol.ind.sup
    canvas.ind.sup <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.ind.sup.value)
    ChangeColor.ind.sup <- function()
    {
      Rcol.ind.sup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.ind.sup.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Rcol.ind.sup.value)>0)
      {
        tkconfigure(canvas.ind.sup,bg=Rcol.ind.sup.value)
        assign("Rcol.ind.sup.tmp", Rcol.ind.sup.value, envir=env)
      }
    }
    ChangeColor.ind.sup.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.ind.sup)
    if(!is.null(individuillu)) tkgrid(tklabel(RcolFrame, text=gettextRcmdr("color for supplementary individuals")),canvas.ind.sup,ChangeColor.ind.sup.button)

    Rcol.quali.value<- Rcol.quali
    canvas.quali <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.quali.value)
    ChangeColor.quali <- function()
    {
      Rcol.quali.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.quali.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Rcol.quali.value)>0)
      {
        tkconfigure(canvas.quali,bg=Rcol.quali.value)
        assign("Rcol.quali.tmp", Rcol.quali.value, envir=env)
      }
    }
    ChangeColor.quali.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.quali)
    tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color for factors")),canvas.quali,ChangeColor.quali.button)

    Rcol.qualisup.value<- Rcol.qualisup
    canvas.qualisup <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.qualisup.value)
    ChangeColor.qualisup <- function()
    {
      Rcol.qualisup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.qualisup.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Rcol.qualisup.value)>0)
      {
        tkconfigure(canvas.qualisup,bg=Rcol.qualisup.value)
        assign("Rcol.qualisup.tmp", Rcol.qualisup.value, envir=env)
      }
    }
    ChangeColor.qualisup.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.qualisup)
    if(!is.null(variablefact)) tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color for supplementary factors")),canvas.qualisup,ChangeColor.qualisup.button)

    RlimFrame<-tkframe(PlotIndFrame,borderwidth=2)
    if(is.null(RXlimInd)) XlimIndMin<-tclVar("")
    else XlimIndMin<-tclVar(paste(RXlimInd[1]))
    XlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMin)
    if (is.null(RXlimInd)) XlimIndMax<- tclVar("")
    else XlimIndMax<-tclVar(paste(RXlimInd[1]))
      XlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMax)
      tkgrid(tklabel(RlimFrame,text=gettextRcmdr("x limits of the graph:")),XlimIndMin.entry, XlimIndMax.entry)
    if(is.null(RYlimInd)) YlimIndMin<- tclVar("")
    else YlimIndMin<-tclVar(paste(RYlimInd[1]))
    YlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMin)
    if (is.null(RYlimInd)) YlimIndMax<- tclVar("")
    else YlimIndMax<-tclVar(paste(RYlimInd[2]))
    YlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMax)
    tkgrid(tklabel(RlimFrame,text=gettextRcmdr("y limits of the graph:")),YlimIndMin.entry,YlimIndMax.entry)

    #mise en page des différents frames de PlotIndFrame
    #tkgrid(tklabel(PlotIndFrame, text=gettextRcmdr("Individuals graph"), font=font2))
    #tkgrid(tklabel(PlotIndFrame, text=" "))
    tkgrid(RchoixFrame)
    tkgrid(RTitleFrame)
    tkgrid(tklabel(PlotIndFrame, text=" "))
    tkgrid(RlabelFrame)
    tkgrid(tklabel(PlotIndFrame, text=" "))
    tkgrid(RcolFrame)
    tkgrid(tklabel(PlotIndFrame, text=" "))
    tkgrid(RlimFrame)
    tkgrid(tklabel(PlotIndFrame, text=" "))

    PlotVarFrame<-tkframe(PlotWin, borderwidth=5, relief="groove")
    WchoixFrame<-tkframe(PlotVarFrame,borderwidth=2)
    quanti.var.check<-tkcheckbutton(WchoixFrame)
    if(Wchoix) quanti.var.check.value<-tclVar("1")
    else quanti.var.check.value<-tclVar("0")
    tkconfigure(quanti.var.check, variable=quanti.var.check.value)
    tkgrid(tklabel(WchoixFrame, text=gettextRcmdr("Plot supplementary variables graph"), font=font2),quanti.var.check)
    tkgrid(tklabel(WchoixFrame, text="  "))

    WTitleFrame<-tkframe(PlotVarFrame,borderwidth=2)
    if (is.null(WTitle)) WTitre <- tclVar(" ")
    else WTitre<-tclVar(WTitle)
    WTitre.entry <-tkentry(WTitleFrame,width="40",textvariable=WTitre)
    tkgrid(tklabel(WTitleFrame,text=gettextRcmdr("Title of the graph")),WTitre.entry)

    #WcosFrame<-tkframe(PlotVarFrame,borderwidth=2)
    #WlimCosValue<-tclVar(paste(Wlim.cos))
    #WlimCos.entry<-tkentry(WcosFrame, width=5, textvariable=WlimCosValue)
    #tkgrid(tklabel(WcosFrame,text=gettextRcmdr("Draw variables with a cos2 >:")),WlimCos.entry)

    WlabelFrame<-tkframe(PlotVarFrame,borderwidth=2)
    label.quanti.sup.check<-tkcheckbutton(WlabelFrame)
    if ("quanti.sup" %in% Wlabel) label.quanti.sup.checkValue<-tclVar("1")
    else label.quanti.sup.checkValue<-tclVar("0")
    tkconfigure(label.quanti.sup.check, variable=label.quanti.sup.checkValue)
    tkgrid(tklabel(WlabelFrame, text=gettextRcmdr("Labels for the supplementary quantitative variables")),label.quanti.sup.check)

    WcolFrame<-tkframe(PlotVarFrame,borderwidth=2)
    Wcol.quanti.sup.value<-Wcol.quanti.sup
    canvas.quanti.sup <- tkcanvas(WcolFrame,width="80",height="25",bg=Wcol.quanti.sup.value)
    ChangeColor.quanti.sup <- function()
    {
      Wcol.quanti.sup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Wcol.quanti.sup.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Wcol.quanti.sup.value)>0) {
        tkconfigure(canvas.quanti.sup,bg=Wcol.quanti.sup.value)
        assign("Wcol.quanti.sup.tmp", Wcol.quanti.sup.value, envir=env)
      }
    }
    ChangeColor.quanti.sup.button <- tkbutton(WcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.quanti.sup)
    tkgrid(tklabel(WcolFrame, text=gettextRcmdr("Color for supplementary variables")),canvas.quanti.sup,ChangeColor.quanti.sup.button)

    #mise en page des différents frames de PlotVarFrame
    tkgrid(WchoixFrame)
    tkgrid(WTitleFrame)
    #tkgrid(WcosFrame)
    tkgrid(WlabelFrame)
    tkgrid(tklabel(PlotVarFrame, text=" "))
    tkgrid(WcolFrame)
    tkgrid(tklabel(PlotVarFrame, text=" "))
    
################Début à changer
    PlotVVarFrame<-tkframe(PlotWin, borderwidth=5, relief="groove")
    VchoixFrame<-tkframe(PlotVVarFrame,borderwidth=2)
    var.check<-tkcheckbutton(VchoixFrame)
    if(Vchoix) var.check.value<-tclVar("1")
    else var.check.value<-tclVar("0")
    tkconfigure(var.check, variable=var.check.value)
    tkgrid(tklabel(VchoixFrame, text=gettextRcmdr("Plot variables graph"), font=font2),var.check)
    tkgrid(tklabel(VchoixFrame, text="  "))

    VTitleFrame<-tkframe(PlotVVarFrame,borderwidth=2)
    if (is.null(VTitle)) VTitre <- tclVar(" ")
    else VTitre<-tclVar(VTitle)
    VTitre.entry <-tkentry(VTitleFrame,width="40",textvariable=VTitre)
    tkgrid(tklabel(VTitleFrame,text=gettextRcmdr("Title of the graph")),VTitre.entry)

    VlabelFrame<-tkframe(PlotVVarFrame,borderwidth=2)
    tkgrid(tklabel(VlabelFrame, text=gettextRcmdr("  ")),tklabel(VlabelFrame,text=gettextRcmdr("Plot")),tklabel(VlabelFrame, text=gettextRcmdr("Label")))
    labelV.var.check<-tkcheckbutton(VlabelFrame)
##    label.quali.sup.check<-tkcheckbutton(VlabelFrame)
##    if ("quali.sup" %in% Vlabel) label.quali.sup.checkValue<-tclVar("1")
##    else label.quali.sup.checkValue<-tclVar("0")
##    tkconfigure(label.quali.sup.check, variable=label.quali.sup.checkValue)
##    tkgrid(tklabel(VlabelFrame, text=gettextRcmdr("Labels for the supplementary variables")),label.quali.sup.check)
    if ("var" %in% Vlabel) labelV.var.checkValue<-tclVar("1")
    else labelV.var.checkValue<-tclVar("0")
    invisV.var.check<-tkcheckbutton(VlabelFrame)
    if ("var" %in% Vinvis) invisV.var.checkValue<-tclVar("0")
    else invisV.var.checkValue<-tclVar("1")
    tkconfigure(labelV.var.check, variable=labelV.var.checkValue)
    tkconfigure(invisV.var.check, variable=invisV.var.checkValue)
    tkgrid(tklabel(VlabelFrame, text=gettextRcmdr("Active variables")),invisV.var.check, labelV.var.check)
    labelV.quali.sup.check<-tkcheckbutton(VlabelFrame)
    if ("quali.sup" %in% Vlabel) labelV.quali.sup.checkValue<-tclVar("1")
    else labelV.quali.sup.checkValue<-tclVar("0")
    invisV.quali.sup.check<-tkcheckbutton(VlabelFrame)
    if ("quali.sup" %in% Vinvis) invisV.quali.sup.checkValue<-tclVar("0")
    else invisV.quali.sup.checkValue<-tclVar("1")
    tkconfigure(labelV.quali.sup.check, variable=labelV.quali.sup.checkValue)
    tkconfigure(invisV.quali.sup.check, variable=invisV.quali.sup.checkValue)
    if(!is.null(variablefact)) tkgrid(tklabel(VlabelFrame, text=gettextRcmdr("Supplementary factors")),invisV.quali.sup.check, labelV.quali.sup.check)

    VcolFrame<-tkframe(PlotVVarFrame,borderwidth=2)
    Vcol.var.value<-Vcol.var
    canvas.varV <- tkcanvas(VcolFrame,width="80",height="25",bg=Vcol.var.value)
    ChangeColor.varV <- function()
    {
      Vcol.var.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Vcol.var.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Vcol.var.value)>0) {
        tkconfigure(canvas.varV,bg=Vcol.var.value)
        assign("Vcol.var.tmp", Vcol.var.value, envir=env)
      }
    }
    ChangeColor.varV.button <- tkbutton(VcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.varV)
    tkgrid(tklabel(VcolFrame, text=gettextRcmdr("Color for active variables")),canvas.varV,ChangeColor.varV.button)
###
    Vcol.quali.sup.value<-Vcol.quali.sup
    canvas.quali.sup <- tkcanvas(VcolFrame,width="80",height="25",bg=Vcol.quali.sup.value)
    ChangeColor.quali.sup <- function()
    {
      Vcol.quali.sup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Vcol.quali.sup.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Vcol.quali.sup.value)>0) {
        tkconfigure(canvas.quali.sup,bg=Vcol.quali.sup.value)
        assign("Vcol.quali.sup.tmp", Vcol.quali.sup.value, envir=env)
      }
    }
    ChangeColor.quali.sup.button <- tkbutton(VcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.quali.sup)
    tkgrid(tklabel(VcolFrame, text=gettextRcmdr("Color for supplementary variables")),canvas.quali.sup,ChangeColor.quali.sup.button)

    #mise en page des différents frames de PlotVarFrame
    tkgrid(VchoixFrame)
    tkgrid(VTitleFrame)
    tkgrid(VlabelFrame)
    tkgrid(tklabel(PlotVVarFrame, text=" "))
    tkgrid(VcolFrame)
    tkgrid(tklabel(PlotVVarFrame, text=" "))
    
################Fin à changer

    # construction de la partie graphique des variables

    subOKCancelHelp(PlotWin, "plot.MCA")
    tkgrid(PlotIndFrame,PlotVVarFrame)
    tkgrid.configure(PlotVVarFrame,sticky="ne")
    if (length(variableillu)>0) {
      tkgrid(PlotVarFrame)
      tkgrid.configure(PlotVarFrame, sticky="se")
    }
    tkgrid(subButtonsFrame, sticky="ew", columnspan=2)

    }
    PlotFrame<-tkframe(IlluFrame)
    Plot.but<-tkbutton(PlotFrame, textvariable=.PlotLabel, command=OnPlot, borderwidth=3)
    tkgrid(Plot.but, sticky="ew")
  })


  #! fonction pour la réinitialisation des paramètre
  Reinitializ.funct<-function()
  {
    tkdestroy(top)
        FactoMCA()
  }


  #! fonction pour le choix des éléments de sortie
  Sortie.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.sortie<-0
    #déclaration des variables
    Rpropre<-FALSE
    RFichier <- ""
        Rvariable<-FALSE
        Rindividu<-FALSE
        Rindsup<-FALSE
        Rquantisup<-FALSE
        Rqualisup<-FALSE
        Rdescdim<-FALSE

    .SortieLabel<-tclVar(paste(firstLabel, "", sep=" "))

    OnSortie<-function()
    {
      SortieWin<-tktoplevel()
      tkwm.title(SortieWin,"Displayed outputs")

      #création de la fonction onOKsub
      onOK.sortie<-function()
      {
        assign("compteur.sortie", compteur.sortie+1, envir=env)
        if(compteur.sortie>0) tclvalue(.SortieLabel)<-paste(label, "", sep=" ")
        tkconfigure(Sortie.but, fg="blue")

        if(tclvalue(eigValue)=="1") assign("Rpropre", TRUE, envir=env)
        else assign("Rpropre", FALSE, envir=env)

        if(tclvalue(varValue)=="1") assign("Rvariable", TRUE, envir=env)
        else assign("Rvariable", FALSE, envir=env)

        if(tclvalue(indValue)=="1") assign("Rindividu", TRUE, envir=env)
        else assign("Rindividu", FALSE, envir=env)

        if(tclvalue(ind.sup.Value)=="1") assign("Rindsup", TRUE, envir=env)
        else assign("Rindsup", FALSE, envir=env)

        if(tclvalue(quanti.sup.Value)=="1") assign("Rquantisup", TRUE, envir=env)
        else assign("Rquantisup", FALSE, envir=env)

        if(tclvalue(quali.sup.Value)=="1") assign("Rqualisup", TRUE, envir=env)
        else assign("Rqualisup", FALSE, envir=env)

        if(tclvalue(descdimValue)=="1") assign("Rdescdim", TRUE, envir=env)
        else assign("Rdescdim", FALSE, envir=env)

        if (tclvalue(Fichier)=="") assign("RFichier", NULL, envir=env)
        assign("RFichier", tclvalue(Fichier), envir=env)

        tkdestroy(SortieWin)

      }

      eig.lab <-tklabel(SortieWin, text=gettextRcmdr("Eigenvalues"))
        eig.check <- tkcheckbutton(SortieWin)
        if(Rpropre) eigValue <- tclVar("1")
        else eigValue <- tclVar("0")
        tkconfigure(eig.check,variable=eigValue)

      var.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for active variables"))
        var.check <- tkcheckbutton(SortieWin)
        if(Rvariable) varValue <- tclVar("1")
        else varValue <- tclVar("0")
        tkconfigure(var.check,variable=varValue)

      ind.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for active individuals"))
        ind.check <- tkcheckbutton(SortieWin)
        if(Rindividu) indValue <- tclVar("1")
        else indValue <- tclVar("0")
        tkconfigure(ind.check,variable=indValue)

      ind.sup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for supplementary individuals"))
        ind.sup.check <- tkcheckbutton(SortieWin)
        if(Rindsup) ind.sup.Value <- tclVar("1")
        else ind.sup.Value <- tclVar("0")
        tkconfigure(ind.sup.check,variable=ind.sup.Value)

      quanti.sup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for supplementary variables"))
        quanti.sup.check <- tkcheckbutton(SortieWin)
        if(Rquantisup) quanti.sup.Value <- tclVar("1")
        else quanti.sup.Value <- tclVar("0")
        tkconfigure(quanti.sup.check,variable=quanti.sup.Value)

      quali.sup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for supplementary factors"))
        quali.sup.check <- tkcheckbutton(SortieWin)
      if(Rqualisup) quali.sup.Value <- tclVar("1")
        else quali.sup.Value <- tclVar("0")
        tkconfigure(quali.sup.check,variable=quali.sup.Value)

        descdim.lab<-tklabel(SortieWin, text=gettextRcmdr("Description of the dimensions"))
      descdim.check<-tkcheckbutton(SortieWin)
      if(Rdescdim) descdimValue<-tclVar("1")
      else descdimValue<-tclVar("0")
      tkconfigure(descdim.check,variable=descdimValue)

      RFichierFrame<-tkframe(SortieWin,borderwidth=2)
      if (is.null(RFichier)) Fichier <- tclVar("")
      else Fichier<-tclVar(RFichier)
      Fichier.entry <-tkentry(RFichierFrame,width="40",textvariable=Fichier)
      tkgrid(tklabel(RFichierFrame,text=gettextRcmdr("Print results on a 'csv' file")),Fichier.entry)

      SortieOK.but<-tkbutton(SortieWin,text="OK",width=16,command=onOK.sortie)

        tkgrid(tklabel(SortieWin, text = gettextRcmdr("Select output options"), fg ="blue"),  columnspan = 2, sticky = "w")
        tkgrid(tklabel(SortieWin, text = " "))
        tkgrid(eig.lab,eig.check,sticky="w")
        tkgrid(var.lab,var.check,sticky="w")
        tkgrid(ind.lab,ind.check,sticky="w")
        if (!is.null(individuillu)) tkgrid(ind.sup.lab,ind.sup.check,sticky="w")
        if (!is.null(variableillu)) tkgrid(quanti.sup.lab,quanti.sup.check,sticky="w")
        if (!is.null(variablefact)) tkgrid(quali.sup.lab,quali.sup.check,sticky="w")
        tkgrid(descdim.lab,descdim.check,sticky="w")
        tkgrid(tklabel(SortieWin, text = " "))
        tkgrid(RFichierFrame)
        tkgrid(SortieOK.but)
   }

    SortieFrame<-tkframe(IlluFrame)
    Sortie.but<-tkbutton(SortieFrame, textvariable=.SortieLabel, command=OnSortie, borderwidth=3)
    tkgrid(Sortie.but, sticky="ew")
  })

  #! fonction HCPC
  
  Hcpc.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    .HcpcLabel<-tclVar(paste(firstLabel, "", sep=" "))    
    compteur.hcpc<-0
    Rclassif<-0
    Rmeth <- -1
    Rconsolid<-0 
    Rgraphhcpc<-1  
    Rreshcpc<-0
    Rminhcpc<-3
    Rmaxhcpc<-10

    OnHCPC <- function()
    {

      HcpcWin<-tktoplevel()
      tkwm.title(HcpcWin, gettextRcmdr("HCPC options"))

      onOKHcpc <- function()
      {
        assign("compteur.hcpc", compteur.hcpc+1, envir=env) 
        if(compteur.hcpc>0) tclvalue(.HcpcLabel)<-paste(label, "", sep=" ")
        tkconfigure(Hcpc.but, fg="blue")
      
        if(tclvalue(methValue)=="interactive") assign("Rmeth", 0, envir=env)
        else assign("Rmeth", -1, envir=env)
        
        if(tclvalue(consolidValue)=="1") assign("Rconsolid",TRUE, envir=env)
        else assign("Rconsolid",FALSE,envir=env)
        
        if(tclvalue(graphhcpcValue)=="1") assign("Rgraphhcpc",TRUE,envir=env)
        else assign("Rgraphhcpc",FALSE,envir=env)
        
        if(tclvalue(reshcpcValue)=="1") assign("Rreshcpc",TRUE,envir=env)
        else assign("Rreshcpc",FALSE,envir=env)        
        
        assign("Rminhcpc",as.numeric(tclvalue(minhcpc)),envir=env)
        assign("Rmaxhcpc",as.numeric(tclvalue(maxhcpc)),envir=env)
        
        assign("Rclassif",TRUE,envir=env)
        tkdestroy(HcpcWin)
      }
      OKHcpc.but<-tkbutton(HcpcWin, text="OK", width=8,command=onOKHcpc)

      onCancelHcpc <- function()
      {
        assign("Rclassif",FALSE,envir=env)
        tkdestroy(HcpcWin)
      }
      CancelHcpc.but<-tkbutton(HcpcWin, text="Cancel", width=8,command=onCancelHcpc)
 
      tkgrid(tklabel(HcpcWin, text=""))
      tkgrid(tklabel(HcpcWin, text = gettextRcmdr("Hierarchical Clustering on Principal Components"), fg = "darkred"), column=1, columnspan = 8, sticky = "ew")      

      meth1 <- tkradiobutton (HcpcWin)
      meth1.lab <- tklabel(HcpcWin,text=gettextRcmdr("interactive"))
      meth2 <- tkradiobutton (HcpcWin)
      meth2.lab <- tklabel(HcpcWin,text=gettextRcmdr("automatic"))
      methValue <- tclVar("interactive")
      meth.lab <- tklabel(HcpcWin,text=gettextRcmdr("Choice of the number of clusters: "))
      tkconfigure(meth1,variable=methValue,value="interactive")
      tkconfigure(meth2,variable=methValue,value="automatic")

      minmaxhcpc.label<-tklabel(HcpcWin,text=gettextRcmdr("The optimal number of clusters is chosen between:"))

      minhcpc<-tclVar("3")
      maxhcpc<-tclVar("10")
      minhcpc.entry <-tkentry(HcpcWin,width="3",textvariable=minhcpc)
      maxhcpc.entry <-tkentry(HcpcWin,width="3",textvariable=maxhcpc)

      consolid.lab <- tklabel(HcpcWin,text=gettextRcmdr("Consolidate clusters "))
      consolid.check <- tkcheckbutton(HcpcWin)
      if(Rconsolid) consolidValue<-tclVar("1")
      else consolidValue<-tclVar("0")      
      tkconfigure(consolid.check,variable=consolidValue)
      
      graphhcpc.lab <- tklabel(HcpcWin,text=gettextRcmdr("Print graphs "))
      graphhcpc.check <- tkcheckbutton(HcpcWin)
      if(Rgraphhcpc) graphhcpcValue <- tclVar("1")
      else graphhcpcValue <- tclVar("0")
      tkconfigure(graphhcpc.check,variable=graphhcpcValue)   

      reshcpc.lab <- tklabel(HcpcWin,text=gettextRcmdr("Print results for clusters "))
      reshcpc.check <- tkcheckbutton(HcpcWin)
      if(Rreshcpc) reshcpcValue<-tclVar("1")
      else reshcpcValue <- tclVar("0")
      tkconfigure(reshcpc.check,variable=reshcpcValue)          
    
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr("Select options for the HCPC"), fg = "blue"), column=1, columnspan=8, sticky="we")
      tkgrid(tklabel(HcpcWin,text=""))
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr(paste('Clustering is performed on the first ', tclvalue(ncp.val), ' dimensions of the MCA',sep=""))),column=1,columnspan=4,sticky="w")
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr("(Change your choice in the main options to change this number)")),column=1,columnspan=4,sticky="w")
      tkgrid(tklabel(HcpcWin,text=""))  
      tkgrid(meth.lab,meth1.lab,meth1)
      tkgrid(meth2.lab,meth2)
      tkgrid(tklabel(HcpcWin,text=""))
      tkgrid(minmaxhcpc.label,minhcpc.entry , maxhcpc.entry)
      tkgrid(tklabel(HcpcWin,text=""))      
      tkgrid(consolid.lab,consolid.check)
      tkgrid(graphhcpc.lab,graphhcpc.check)
      tkgrid(reshcpc.lab,reshcpc.check) 
      tkgrid(tklabel(HcpcWin,text=""))     
      tkgrid(OKHcpc.but, CancelHcpc.but)
      tkgrid(tklabel(HcpcWin, text=""))
      
      tkgrid.configure(minmaxhcpc.label,meth.lab,consolid.lab,graphhcpc.lab,reshcpc.lab,column=1,columnspan=4,sticky="w")
      tkgrid.configure(minhcpc.entry,column=7,columnspan=1,sticky="e")
      tkgrid.configure(maxhcpc.entry,column=8,columnspan=1,sticky="w")
      tkgrid.configure(meth1,meth2,consolid.check,graphhcpc.check,reshcpc.check,column=8,sticky="e")
      tkgrid.configure(meth1.lab,column=6,columnspan=2,sticky="w")
      tkgrid.configure(meth2.lab,column=6,columnspan=2,sticky="w") 
      tkgrid.configure(OKHcpc.but,column=2,columnspan=1,sticky="w")
      tkgrid.configure(CancelHcpc.but,column=6,columnspan=1,sticky="e")

      tkgrid.columnconfigure(HcpcWin,0, minsize=3)
      tkgrid.columnconfigure(HcpcWin,5, minsize=5)
      tkgrid.columnconfigure(HcpcWin,8, minsize=3) 

}      
    Hcpc2Frame<-tkframe(HcpcFrame)
    Hcpc.but<-tkbutton(Hcpc2Frame, textvariable=.HcpcLabel, command=OnHCPC, borderwidth=3)
    tkgrid(Hcpc.but, sticky="ew")
}) 

  #! fonction associée au bouton OK, execute et détruit l'interface graphique
onOK <- function(){
  done = OnAppliquer()
  tkdestroy(top)
}

  #! fonction associer au bouton Appliquer, execute sans détruire la fenêtre top
  OnAppliquer<-function()
  {
    # liste de toutes les variables interne créées      (** mise en forme incomplète)
      # sur la fenetre principale
#         listdesc         **
#         resu.val         **
#         ncp.val          **
      # dans les boutons des fenêtres illustratives
#         variablefact     **
#         variableillu     **
#         individuillu     **
      # dans le bouton Plot MCA
#         Rchoix
#         RTitle
#         Rlabel
#         Rcol.ind
#         Rcol.ind.sup
#         Rcol.quali
#         Rhabillage       **
#         RXlimInd
#         RYlimInd
#         Wcol.var
      # dans le bouton affichage sortie
#         Rpropre
#             Rvariable
#           Rindividu
#           Rindsup
#           Rquantisup
#           Rqualisup


    # récupération des paramètres de la fenêtre principale
    nom.res<-tclvalue(resu.val)
    if (length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0) justDoIt(paste('remove (',nom.res,')'))
    if(length(as.numeric(tkcurselection(listdesc)))<2) varActives<-listdesc.nom
    else varActives<-listdesc.nom[as.numeric(tkcurselection(listdesc))+1]
    varActives <- varActives[!(varActives%in%variablefact)]
    ncp<-as.numeric(tclvalue(ncp.val))
    Axe<-c(as.numeric(tclvalue(Axe1)), as.numeric(tclvalue(Axe2)))

    # gestion du tableau de données pour l'ACM
    if(!is.null(individuillu)) {
      ind.actif<-rows[-which(rows %in% individuillu)]
      if(!is.null(variableillu)) {
        if(!is.null(variablefact)) commande.data<-paste(activeDataSet(),'.', 'MCA', '<-', activeDataSet(), '[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '") ,c("', paste(varActives, collapse='", "'), '", "', paste(variableillu, collapse='", "'), '", "', paste(variablefact, collapse='", "'), '")]', sep="")
        else commande.data<-paste(activeDataSet(),'.', 'MCA', '<-', activeDataSet(), '[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '") ,c("', paste(varActives, collapse='", "'), '", "', paste(variableillu, collapse='", "'), '")]', sep="")
      }
      else {
        if(!is.null(variablefact)) commande.data<-paste(activeDataSet(),'.', 'MCA', '<-', activeDataSet(), '[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '") ,c("', paste(varActives, collapse='", "'), '", "', paste(variablefact, collapse='", "'), '")]', sep="")
        else commande.data<-paste(activeDataSet(),'.', 'MCA', '<-', activeDataSet(), '[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '") ,c("', paste(varActives, collapse='", "'), '")]', sep="")
      }
    }
    else {
      if(!is.null(variableillu)) {
        if(!is.null(variablefact)) commande.data<-paste(activeDataSet(),'.', 'MCA', '<-', activeDataSet(), '[, c("', paste(varActives, collapse='", "'), '", "', paste(variableillu, collapse='", "'), '", "', paste(variablefact, collapse='", "'), '")]', sep="")
        else commande.data<-paste(activeDataSet(),'.', 'MCA', '<-', activeDataSet(), '[, c("', paste(varActives, collapse='", "'), '", "', paste(variableillu, collapse='", "'), '")]', sep="")
      }
      else {
        if(!is.null(variablefact)) commande.data<-paste(activeDataSet(),'.', 'MCA', '<-', activeDataSet(), '[, c("', paste(varActives, collapse='", "'), '", "', paste(variablefact, collapse='", "'), '")]', sep="")
        else commande.data<-paste(activeDataSet(),'.', 'MCA', '<-', activeDataSet(), '[, c("', paste(varActives, collapse='", "'), '")]', sep="")
      }
    }
    justDoIt(commande.data)
    logger(commande.data)
    donnee.depart<-activeDataSet()
    activeDataSet(paste(activeDataSet(),'.', 'MCA', sep=""))

    # gestion de la commande réalisant l'ACM
    if(!is.null(individuillu)) {
      ind.actif<-rows[-which(rows %in% individuillu)]
      if(!is.null(variableillu)) {
        if(!is.null(variablefact))    commande.acm<-paste(nom.res, '<-MCA(', activeDataSet(), ', ncp=', ncp, ', ind.sup=', nrow(get(.activeDataSet))-length(individuillu)+1, ': ', nrow(get(.activeDataSet)), ', quanti.sup=', length(varActives)+1, ': ', length(varActives)+ length(variableillu), ', quali.sup=', length(varActives)+length(variableillu)+1, ': ', length(varActives)+length(variableillu)+length(variablefact), ', graph = FALSE)', sep="")
        else commande.acm<-paste(nom.res, '<-MCA(', activeDataSet(), ', ncp=', ncp, ', ind.sup=', nrow(get(.activeDataSet))-length(individuillu)+1, ': ', nrow(get(.activeDataSet)), ', quanti.sup=', length(varActives)+1, ': ', length(varActives)+ length(variableillu), ', graph = FALSE)', sep="")
      }
      else {
        if(!is.null(variablefact))  commande.acm<-paste(nom.res, '<-MCA(', activeDataSet(), ', ncp=', ncp, ', ind.sup=', nrow(get(.activeDataSet))-length(individuillu)+1, ': ', nrow(get(.activeDataSet)), ', quali.sup=', length(varActives)+1, ': ', length(varActives)+length(variablefact), ', graph = FALSE)', sep="")
        else commande.acm<-paste(nom.res, '<-MCA(', activeDataSet(), ', ncp=', ncp, ', ind.sup=c(', nrow(get(.activeDataSet))-length(individuillu)+1, ': ', nrow(get(.activeDataSet)), '), graph = FALSE)', sep="")
      }
    }
    else
    {
      if(!is.null(variableillu)) {
        if(!is.null(variablefact))  commande.acm<-paste(nom.res, '<-MCA(', activeDataSet(), ', ncp=', ncp, ', ind.sup=NULL, quanti.sup=', length(varActives)+1, ': ', length(varActives)+ length(variableillu), ', quali.sup=', length(varActives)+length(variableillu)+1, ': ', length(varActives)+length(variableillu)+length(variablefact), ', graph = FALSE)', sep="")
        else commande.acm<-paste(nom.res, '<-MCA(', activeDataSet(), ', ncp=', ncp, ', ind.sup=NULL, quanti.sup=', length(varActives)+1, ': ', length(varActives)+ length(variableillu), ', graph = FALSE)', sep="")
      }
      else
      {
        if(!is.null(variablefact)) commande.acm<-paste(nom.res, '<-MCA(', activeDataSet(), ', ncp=', ncp, ', quali.sup=', length(varActives)+1, ': ', length(varActives)+length(variablefact), ', graph = FALSE)', sep="")
        else commande.acm<-paste(nom.res, '<-MCA(', activeDataSet(), ', ncp=', ncp, ', graph = FALSE)', sep="")
      }
    }
    justDoIt(commande.acm)
    logger(commande.acm)

    #Commande de la fonction HCPC

    if(Rclassif==TRUE){
      commande.hcpc<-paste(nom.res,'.hcpc', '<-HCPC(', nom.res, ' ,nb.clust=', Rmeth, ',consol=', Rconsolid,',min=', Rminhcpc,',max=',Rmaxhcpc,',graph=', Rgraphhcpc, ')', sep="")
    justDoIt(commande.hcpc)
    logger(commande.hcpc)      
      if(Rreshcpc==TRUE){
        doItAndPrint(paste(nom.res,'.hcpc$data.clust[,ncol(res.hcpc$data.clust),drop=F]', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.var', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.axes', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.ind', sep=""))
      }        
    }


    # gestion des graphiques

    if (length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0) {if (get(nom.res)$eig[1,2]==100) doItAndPrint(paste('"No graph can be plot: data are unidimensional"'))}
    if((Rchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)){
    if (get(nom.res)$eig[1,2]!=100) {
      commande.plotInd<-paste('plot.MCA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), col.ind="', Rcol.ind, '", col.ind.sup="', Rcol.ind.sup, '", col.var="', Rcol.quali, '", col.quali.sup="', Rcol.qualisup, '", label=c("', paste(Rlabel, collapse='", "'), '"), invisible=c("', paste(Rinvis, collapse='", "'), '")', sep="")
      if (!is.null(RTitle)) {
        if (RTitle !=" ") commande.plotInd <- paste(commande.plotInd,', title="', RTitle,'"', sep="")
      }
      if (!is.null(RXlimInd)) commande.plotInd <- paste(commande.plotInd,', xlim=c(', paste(RXlimInd, collapse=", "), ')', sep="")
      if (!is.null(RYlimInd)) commande.plotInd <- paste(commande.plotInd,', ylim=c(', paste(RYlimInd, collapse=", "), ')', sep="")
      commande.plotInd <- paste(commande.plotInd,')', sep="")
      justDoIt(commande.plotInd)
      logger(commande.plotInd)
    }}

    if((Vchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)){
    if (get(nom.res)$eig[1,2]!=100) {
      commande.plotInd<-paste('plot.MCA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="var", col.var="',Vcol.var,'", col.quali.sup="',Vcol.quali.sup,'", label=c("', paste(Vlabel, collapse='", "'), '"), invisible=c("', paste(Vinvis, collapse='", "'), '")', sep="")
      if (!is.null(VTitle)) {
        if (VTitle !=" ") commande.plotInd <- paste(commande.plotInd,', title="', VTitle,'"', sep="")
      }
      commande.plotInd <- paste(commande.plotInd,')', sep="")
      justDoIt(commande.plotInd) 
      logger(commande.plotInd)
    }}

    if((Wchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)){
    if (get(nom.res)$eig[1,2]!=100) {
      commande.plotInd<-paste('plot.MCA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="quanti.sup", col.quanti.sup="',Wcol.quanti.sup,'"',', label=c("', paste(Wlabel, collapse='", "'),'")', sep="")
      if (!is.null(WTitle)) {
        if (WTitle !=" ") commande.plotInd <- paste(commande.plotInd,', title="', WTitle,'"', sep="")
      }
#      if ("quanti.sup"%in%Wlabel) commande.plotInd <- paste(commande.plotInd, ',label=c("quanti.sup")',sep='') 
      commande.plotInd <- paste(commande.plotInd,')', sep="")
      justDoIt(commande.plotInd) 
      logger(commande.plotInd)
    }}

    # gestion de l'édition de certains resultats
    if (RFichier==""){
      if(Rpropre) doItAndPrint(paste( nom.res, '$eig', sep=""))
      if(Rvariable) doItAndPrint(paste( nom.res, '$var', sep=""))
      if(Rindividu) doItAndPrint(paste( nom.res, '$ind', sep=""))
      if(Rindsup & !is.null(individuillu)) doItAndPrint(paste( nom.res, '$ind.sup', sep=""))
      if(Rquantisup & !is.null(variableillu)) doItAndPrint(paste( nom.res, '$quanti.sup', sep=""))
      if(Rqualisup & !is.null(variablefact)) doItAndPrint(paste( nom.res, '$quali.sup', sep=""))
      if(Rdescdim) doItAndPrint(paste('dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '))', sep=""))
    }
    else {
      Fich = RFichier
      if (substr(Fich,1,1)!='"') Fich = paste('"',Fich,sep='')
      if (substr(Fich,nchar(Fich),nchar(Fich))!='"') Fich = paste(Fich,'"',sep='')
      append = FALSE
      if(Rpropre){
        doItAndPrint(paste('write.infile(', nom.res, '$eig, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rvariable){
        doItAndPrint(paste('write.infile(', nom.res, '$var, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rindividu){
        doItAndPrint(paste('write.infile(', nom.res, '$ind, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rindsup){
        doItAndPrint(paste('write.infile(', nom.res, '$ind.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rquantisup){
        doItAndPrint(paste('write.infile(', nom.res, '$quanti.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rqualisup){
        doItAndPrint(paste('write.infile(', nom.res, '$quali.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rdescdim) doItAndPrint(paste('write.infile(dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), ')), file =',Fich,',append=',append,')', sep=""))
    }

    # Re-chargement du tableau de départ et supression du tableau temporaire
    activeDataSet(donnee.depart)
    justDoIt(paste('remove(',activeDataSet(),'.MCA)',sep=""))
    logger(paste('remove(',activeDataSet(),'.MCA)',sep=""))
  }


################################################################################
#                   Création de la fenêtre top                                 #
################################################################################
  top<-tktoplevel(borderwidth=10)
  tkwm.title(top, gettextRcmdr("MCA"))
  tkwm.geometry(top, "-100+50")

  # définition des polices
  font2<-tkfont.create(family="times",size=12,weight="bold")
  fontheading<-tkfont.create(family="times",size=18,weight="bold")

  # récupération du jeu de données actif
  donnee<-get(.activeDataSet)
  vars<-colnames(donnee)
  rows<-rownames(donnee)

  # création de la liste pour le choix des variables acives
  listdesc<-tklistbox(top,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scr,...))
  scr <-tkscrollbar(top,repeatinterval=5,command=function(...)tkyview(listdesc,...))
  listdesc.nom<-NULL
  for (i in (1:ncol(donnee))) {
      if (is.factor(donnee[,i])) {
          tkinsert(listdesc,"end",vars[i])
          listdesc.nom<-c(listdesc.nom, vars[i])
      }
  }

  # création de tous les boutons d'options dans IlluFrame
  IlluFrame<- tkframe(top, borderwidth=2)
  Reinitializ.but<-tkbutton(IlluFrame, text=gettextRcmdr("Restart"),width=18,command=Reinitializ.funct, borderwidth=3)
       # mise en page de IlluFrame

  Fillu.funct(label=gettextRcmdr("Select supplementary factors"), firstLabel=gettextRcmdr("Select supplementary factors"))
  Dillu.funct(label=gettextRcmdr("Select supplementary quantitative variables"), firstLabel=gettextRcmdr("Select supplementary quantitative variables"))
  Iillu.funct(label=gettextRcmdr("Select supplementary individuals"), firstLabel=gettextRcmdr("Select supplementary individuals"))
  PLOT.MCA(label=gettextRcmdr("Graphical options"), firstLabel=gettextRcmdr("Graphical options"))
  Sortie.funct(label=gettextRcmdr("Outputs"), firstLabel=gettextRcmdr("Outputs"))
  tkgrid(FilluFrame, DilluFrame, IilluFrame, columnspan=7)
  tkgrid(tklabel(IlluFrame, text=""))
  tkgrid(PlotFrame, SortieFrame, Reinitializ.but, columnspan=7)
  tkgrid.configure(FilluFrame, column=1, columnspan=1)
  tkgrid.configure(PlotFrame, column=1, columnspan=1)
  tkgrid.configure(DilluFrame, column=3, columnspan=1)
  tkgrid.configure(SortieFrame, column=3, columnspan=1)
  tkgrid.configure(IilluFrame, column=5, columnspan=1)
  tkgrid.configure(Reinitializ.but, column=5, columnspan=1)
  tkgrid.columnconfigure(IlluFrame,0, minsize=25)
  tkgrid.columnconfigure(IlluFrame,7, minsize=25)
  tkgrid.columnconfigure(IlluFrame,2, minsize=40)
  tkgrid.columnconfigure(IlluFrame,4, minsize=40)

  # création des options dans OptionFrame
  OptionFrame<-tkframe(top, borderwidth=2, relief="groove")
  resu.lab<-tklabel(OptionFrame,text=gettextRcmdr("Name of the result object: "))
  resu.val<-tclVar("res")
  resu<-tkentry(OptionFrame,width=10,textvariable=resu.val)
  ncp.lab<-tklabel(OptionFrame,text=gettextRcmdr("Number of dimensions: "))
  ncp.val<-tclVar("5")
  ncp<-tkentry(OptionFrame,width=5,textvariable=ncp.val)
  Axe.label<-tklabel(OptionFrame,text=gettextRcmdr("Graphical output: select the dimensions:"))
  Axe1<-tclVar("1")
  Axe2<-tclVar("2")
  Axe1.entry <-tkentry(OptionFrame,width="5",textvariable=Axe1)
  Axe2.entry <-tkentry(OptionFrame,width="5",textvariable=Axe2)
    # mise en page de OptionFrame
  tkgrid(tklabel(OptionFrame,text=gettextRcmdr("Main options"), fg = "darkred"), columnspan=8, sticky="we")
  tkgrid(tklabel(OptionFrame,text="")) # Ligne de blanc
  tkgrid(ncp.lab, ncp)
  tkgrid(Axe.label,Axe1.entry , Axe2.entry, sticky="w")
  tkgrid(resu.lab, resu)
  tkgrid(tklabel(OptionFrame,text="")) # Ligne de blanc
  tkgrid.configure(ncp.lab, resu.lab, Axe.label, column=1, columnspan=4, sticky="w")
  tkgrid.configure(ncp, resu, column=6, columnspan=2, sticky="e")
  tkgrid.configure(Axe1.entry, column=6, columnspan=1, sticky="w")
  tkgrid.configure(Axe2.entry, column=7, columnspan=1, sticky="e")
  tkgrid.columnconfigure(OptionFrame,0, minsize=25)
  tkgrid.columnconfigure(OptionFrame,5, minsize=40)
  tkgrid.columnconfigure(OptionFrame,8, minsize=25)

  #Frame pour HCPC
  HcpcFrame<-tkframe(top, borderwidth=2)
  Hcpc.funct(label=gettextRcmdr("Perform Clustering after MCA"), firstLabel=gettextRcmdr("Perform Clustering after MCA")) 
  tkgrid(Hcpc2Frame, columnspan=7)
  tkgrid.configure(Hcpc2Frame,column=4, columnspan=1)
  

  appliquer.but<-tkbutton(top, text=gettextRcmdr("Apply"),width=12,command=OnAppliquer, borderwidth=3, fg="#690f96")
  OKCancelHelp(helpSubject="MCA")

  #TOP
  tkgrid(tklabel(top, text=gettextRcmdr("Multiple Correspondence Analysis (MCA)"),font=fontheading),columnspan=3)
  tkgrid(tklabel(top,text=""))
  tkgrid(tklabel(top, text = gettextRcmdr("Select active variables (by default all the variables are active)"),fg = "darkred"), column=1,columnspan=2, sticky = "w")
  tkgrid(listdesc, scr, sticky = "nw")
  tkgrid.configure(scr, sticky = "ens",column=2)
  tkgrid.configure(listdesc, sticky = "ew", column=1, columnspan=2)
  tkgrid(tklabel(top,text="")) # Ligne de blanc
  tkgrid(IlluFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc
  tkgrid(OptionFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc
  tkgrid(HcpcFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc 
  tkgrid(appliquer.but, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc
  tkgrid(buttonsFrame, column=1, columnspan=1, sticky="ew" )
  tkgrid(tklabel(top,text="")) # Ligne de blanc

}

#############################FIN FONCTION FactoMCA #############################

################################################################################
#                              FONCTION FactoMFA                               #
################################################################################
#! version JMA du 27/12/2006 17:10:39 

FactoMFA<-function()
{
  require(tcltk)
  require(FactoMineR)

# Fonction pour la gestion des noms ############################################

  nom.correct<-function(text, liste=NULL)
  {
    text<-chartr("^\ ", "...", text)
    if(!is.null(liste)) {
      while(text %in% liste) text<-paste(text, ".bis", sep="")
    }
    return(text)  
  }
################################################################################

#    Création des fonctions pour les options via nouvelle fenêtre graphique   


  #! suppression de groupes quantitatif
  supprimeQuanti.funct<-defmacro(label, expr=
  {
    env<-environment()
    OnSGQ<-function()
    {
      grpeActSupprime<-listQuantiAct.nom[as.numeric(tkcurselection(listQuantiAct))+1]
      if(length(grpeActSupprime)>=1)
      { 
        listQuantiAct.nom.tmp<-listQuantiAct.nom[-which(listQuantiAct.nom %in% grpeActSupprime)]
        assign("listQuantiAct.nom",listQuantiAct.nom.tmp, envir=env)
        tkdelete(listQuantiAct,"0","end")
        if(length(listQuantiAct.nom)>=1) {
          for (grpe in listQuantiAct.nom) tkinsert(listQuantiAct, "end", grpe)
        }  
      }
      grpeIlluSupprime<-listQuantiIllu.nom[as.numeric(tkcurselection(listQuantiIllu))+1]
      if(length(grpeIlluSupprime)>=1)
      { 
        listQuantiIllu.nom.tmp<-listQuantiIllu.nom[-which(listQuantiIllu.nom %in% grpeIlluSupprime)]
        assign("listQuantiIllu.nom",listQuantiIllu.nom.tmp, envir=env)
        tkdelete(listQuantiIllu,"0","end")
        if(length(listQuantiIllu.nom)>=1) {
          for (grpe in listQuantiIllu.nom) tkinsert(listQuantiIllu, "end", grpe)
        }  
      }
      nb.grpe<-length(listQuantiAct.nom) + length(listQuantiIllu.nom)
      if (nb.grpe>=1) {
        #tclvalue(.AjoutQuantiLabel)<-paste(nb.grpe, "Add quanti group(s)", sep=" ")
        #tkconfigure(AjoutGpeQuanti.but, fg="blue")
        tclvalue(label.quantiFrame.var)<-paste(nb.grpe, gettextRcmdr("quantitative groups"), sep=" ")
        tkconfigure(label.quantiFrame)
      }  
      else {
        #tclvalue(.AjoutQuantiLabel)<-paste("Add quanti group", "!", sep=" ")
        #tkconfigure(AjoutGpeQuanti.but, fg="black")
        tclvalue(label.quantiFrame.var)<-paste("0", gettextRcmdr("quantitative group"), sep=" ")
        tkconfigure(label.quantiFrame)
      }  
      
    }
    
    SupGpeQuantiFrame<-tkframe(ListeQuantiFrame)
    SupGpeQuanti.but<-tkbutton(SupGpeQuantiFrame, textvariable=tclVar(label), command=OnSGQ, borderwidth=3)
   tkgrid(SupGpeQuanti.but, sticky="ew")  
  })


  #! Ajout d'un groupe quantitatif
  ajoutQuanti.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.GQ<-1
    .AjoutQuantiLabel<-tclVar(paste(firstLabel, "!", sep=" "))
    OnAGQ<-function()
    {
      AjoutGpeQuantiWin<-tktoplevel()
      tkwm.title(AjoutGpeQuantiWin,gettextRcmdr("Definition of a quantitatif group"))
      
      #création de la fonction AGA.OK
      AGQ.OK<-function()
      {
        assign("compteur.GQ", compteur.GQ+1, envir=env)
        nom.groupe<-nom.correct(tclvalue(nomGrpeQuanti.val), liste=c(listQuantiAct.nom,listQuantiIllu.nom))
        if (nom.groupe=="") tkmessageBox(message=gettextRcmdr("Name for the group"), icon="warning", type="ok") 
        else {
          varGroupe<-listVarQuanti.nom[as.numeric(tkcurselection(listVarQuanti))+1]
          if (length(varGroupe)>=1) {
            if(tclvalue(norm.Value)=="ok") assign(paste(nom.groupe,".var", sep=""), c("s", varGroupe), envir=env)
            if(tclvalue(norm.Value)=="nok") assign(paste(nom.groupe,".var", sep=""),c("c", varGroupe), envir=env)
            if(tclvalue(etat.Value)=="actif") {  
              tkinsert(listQuantiAct,"end",nom.groupe)
              assign("listQuantiAct.nom", c(listQuantiAct.nom, nom.groupe),envir=env)
            }  
            if(tclvalue(etat.Value)=="illu") {
              tkinsert(listQuantiIllu,"end",nom.groupe)
              assign("listQuantiIllu.nom",c(listQuantiIllu.nom, nom.groupe),envir=env)
              
            }
            #tclvalue(.AjoutQuantiLabel)<-paste(length(listQuantiAct.nom) + length(listQuantiIllu.nom), label, sep=" ")
            #tkconfigure(AjoutGpeQuanti.but, fg="blue")
            tclvalue(label.quantiFrame.var)<-paste(length(listQuantiAct.nom) + length(listQuantiIllu.nom), gettextRcmdr("quantitative groups"), sep=" ")
            tkconfigure(label.quantiFrame)  
            tkdestroy(AjoutGpeQuantiWin)
          }
        }  
      }
      
      
      # choix du nom du groupe
      nomGrpeQuanti.lab<-tklabel(AjoutGpeQuantiWin,text=gettextRcmdr("Name of the group: "))
      nomGrpeQuanti.val<-tclVar(paste("Gc", compteur.GQ, sep=""))
      nomGrpeQuanti<-tkentry(AjoutGpeQuantiWin,width=15,textvariable=nomGrpeQuanti.val)
      
      # choix de l'état actif ou illustratif
      etat.actif.check<-tkradiobutton(AjoutGpeQuantiWin)
      etat.illu.check<-tkradiobutton(AjoutGpeQuantiWin)
      etat.Value<-tclVar("actif")
      tkconfigure(etat.actif.check,variable=etat.Value,value="actif")
      tkconfigure(etat.illu.check,variable=etat.Value, value="illu")
      
      # choix de la normalisation ou non
      norm.ok.check<-tkradiobutton(AjoutGpeQuantiWin)
      norm.nok.check<-tkradiobutton(AjoutGpeQuantiWin)
      norm.Value<-tclVar("ok")
      tkconfigure(norm.ok.check,variable=norm.Value,value="ok")
      tkconfigure(norm.nok.check,variable=norm.Value, value="nok")
            
      # création de la liste pour le choix des variables acives
      listVarQuanti<-tklistbox(AjoutGpeQuantiWin, selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrVarQuanti,...))
      scrVarQuanti<-tkscrollbar(AjoutGpeQuantiWin,repeatinterval=5,command=function(...)tkyview(listVarQuanti,...))
      listVarQuanti.nom<-NULL
      for (i in (1:ncol(donnee))) {
        if (is.numeric(donnee[,i])) {
          tkinsert(listVarQuanti,"end",vars[i])
          listVarQuanti.nom<-c(listVarQuanti.nom, vars[i])
        }
      }
      
      AGQ.but<-tkbutton(AjoutGpeQuantiWin, text="OK", width=16, command=AGQ.OK)
      
      tkgrid(nomGrpeQuanti.lab, nomGrpeQuanti)
      tkgrid.configure(nomGrpeQuanti.lab, column=0, columnspan=2, sticky="w")
      tkgrid.configure(nomGrpeQuanti, column=2, columnspan=3)      
      tkgrid(tklabel(AjoutGpeQuantiWin, text=""))      
      tkgrid(tklabel(AjoutGpeQuantiWin, text=gettextRcmdr("Status of the group:")), tklabel(AjoutGpeQuantiWin, text="Active"),etat.actif.check, tklabel(AjoutGpeQuantiWin, text=gettextRcmdr("Supplementary")),etat.illu.check, sticky="w")
      tkgrid(tklabel(AjoutGpeQuantiWin, text=""))      
      tkgrid(tklabel(AjoutGpeQuantiWin, text=gettextRcmdr("Scaled the variable of the group:")), tklabel(AjoutGpeQuantiWin, text=gettextRcmdr("Yes")),norm.ok.check, tklabel(AjoutGpeQuantiWin, text=gettextRcmdr("No")),norm.nok.check, sticky="w")
      tkgrid(tklabel(AjoutGpeQuantiWin, text=""))
      tkgrid(tklabel(AjoutGpeQuantiWin, text = gettextRcmdr("Select the variables for the group"), fg = "blue"), column=0, columnspan = 5, sticky = "w")
      tkgrid(listVarQuanti, scrVarQuanti, sticky = "nw")
      tkgrid.configure(scrVarQuanti, sticky = "wns", column=4,columnspan=1)
      tkgrid.configure(listVarQuanti, sticky = "ew", column=0, columnspan=4)
      tkgrid(tklabel(AjoutGpeQuantiWin, text=""))
      tkgrid(AGQ.but, column=2,columnspan=1, sticky="ew")
      tkgrid.columnconfigure(AjoutGpeQuantiWin,0, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQuantiWin,1, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQuantiWin,2, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQuantiWin,3, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQuantiWin,4, minsize=55)             
   }
   GpeQuantiFrame<-tkframe(ListeQuantiFrame)
   AjoutGpeQuanti.but<-tkbutton(GpeQuantiFrame, textvariable=.AjoutQuantiLabel, command=OnAGQ, borderwidth=3)
   tkgrid(AjoutGpeQuanti.but, sticky="ew")
  })    
  


  #! Modification d'un groupe quantitatif
  modifQuanti.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    .ModifQuantiLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnMGQ<-function()
    {
      ModifGpeQuantiWin<-tktoplevel()
      tkwm.title(ModifGpeQuantiWin,gettextRcmdr("Modification of a quantitative group"))
      
      #création de la fonction AGA.OK
      MGQ.OK<-function()
      {
        nom.groupe<-nom.correct(tclvalue(nomModifGrpeQuanti.val), liste=c(listQuantiAct.nom,listQuantiIllu.nom))
        if (nom.groupe=="") tkmessageBox(message=gettextRcmdr("Name for the group"), icon="warning", type="ok") 
        else {
          if(etat=="actif") {
            listQuantiAct.nom.tmp<-listQuantiAct.nom[-which(listQuantiAct.nom== grpeAModifier)]
            assign("listQuantiAct.nom",listQuantiAct.nom.tmp, envir=env)
            tkdelete(listQuantiAct,"0","end")
            for (grpe in listQuantiAct.nom) tkinsert(listQuantiAct, "end", grpe)
          }
          
          if(etat=="illu")  {
            listQuantiIllu.nom.tmp<-listQuantiIllu.nom[-which(listQuantiIllu.nom== grpeAModifier)]
            assign("listQuantiIllu.nom",listQuantiIllu.nom.tmp, envir=env)
            tkdelete(listQuantiIllu,"0","end")
            for (grpe in listQuantiIllu.nom) tkinsert(listQuantiIllu, "end", grpe)
          }
          
          varGroupe<-listModifVarQuanti.nom[as.numeric(tkcurselection(listModifVarQuanti))+1]
          if (length(varGroupe)>=1) {
            if(tclvalue(normModif.Value)=="ok") assign(paste(nom.groupe,".var", sep=""), c("s", varGroupe), envir=env)
            if(tclvalue(normModif.Value)=="nok") assign(paste(nom.groupe,".var", sep=""),c("c", varGroupe), envir=env)
            if(tclvalue(etatModif.Value)=="actif") {  
              tkinsert(listQuantiAct,"end",nom.groupe)
              assign("listQuantiAct.nom", c(listQuantiAct.nom, nom.groupe),envir=env)
            }  
            if(tclvalue(etatModif.Value)=="illu") {
              tkinsert(listQuantiIllu,"end",nom.groupe)
              assign("listQuantiIllu.nom",c(listQuantiIllu.nom, nom.groupe),envir=env)              
            }
              
            tkdestroy(ModifGpeQuantiWin)
          }
        }  
      }
            
      if(length(as.numeric(tkcurselection(listQuantiAct)))>=1) {
        grpeAModifier<-listQuantiAct.nom[as.numeric(tkcurselection(listQuantiAct))+1][1]
        etat<-"actif"
      }
      else if (length(as.numeric(tkcurselection(listQuantiIllu)))>=1) {
        grpeAModifier<-listQuantiIllu.nom[as.numeric(tkcurselection(listQuantiIllu))+1][1]
        etat<-"illu"
      }
      else  {
        tkdestroy(ModifGpeQuantiWin)
        return()
      }  
        
      eval(parse(text=paste("grpeAModifier.var<-",paste(grpeAModifier,".var", sep=""),sep="")))
      # choix du nom du groupe
      nomModifGrpeQuanti.lab<-tklabel(ModifGpeQuantiWin,text=gettextRcmdr("Name of the group: "))
      nomModifGrpeQuanti.val<-tclVar(grpeAModifier)
      nomModifGrpeQuanti<-tkentry(ModifGpeQuantiWin,width=15,textvariable=nomModifGrpeQuanti.val)
      
      # choix de l'état actif ou illustratif
      etatModif.actif.check<-tkradiobutton(ModifGpeQuantiWin)
      etatModif.illu.check<-tkradiobutton(ModifGpeQuantiWin)
      etatModif.Value<-tclVar(etat)
      tkconfigure(etatModif.actif.check,variable=etatModif.Value,value="actif")
        tkconfigure(etatModif.illu.check,variable=etatModif.Value, value="illu")
      
      # choix de la normalisation ou non
      normModif.ok.check<-tkradiobutton(ModifGpeQuantiWin)
      normModif.nok.check<-tkradiobutton(ModifGpeQuantiWin)
      if(grpeAModifier.var[1]=="s") normModif.Value<-tclVar("ok")
      else normModif.Value<-tclVar("nok")
      tkconfigure(normModif.ok.check,variable=normModif.Value,value="ok")
        tkconfigure(normModif.nok.check,variable=normModif.Value, value="nok")
            
      # création de la liste pour le choix des variables acives
      listModifVarQuanti<-tklistbox(ModifGpeQuantiWin, selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrModifVarQuanti,...))
      scrModifVarQuanti<-tkscrollbar(ModifGpeQuantiWin,repeatinterval=5,command=function(...)tkyview(listModifVarQuanti,...))
      listModifVarQuanti.nom<-NULL
      indice.num<-0
      for (i in (1:ncol(donnee))) {
        if (is.numeric(donnee[,i])) {
            tkinsert(listModifVarQuanti,"end",vars[i])
            listModifVarQuanti.nom<-c(listModifVarQuanti.nom, vars[i])
            if(vars[i] %in% grpeAModifier.var[-1]) tkselection.set(listModifVarQuanti, indice.num)
            indice.num<-indice.num+1
        }
      }
      
      MGQ.but<-tkbutton(ModifGpeQuantiWin, text="OK", width=16, command=MGQ.OK)
      
      tkgrid(nomModifGrpeQuanti.lab, nomModifGrpeQuanti)
      tkgrid.configure(nomModifGrpeQuanti.lab, column=0, columnspan=2, sticky="w")
      tkgrid.configure(nomModifGrpeQuanti, column=2, columnspan=3)      
      tkgrid(tklabel(ModifGpeQuantiWin, text=""))      
      tkgrid(tklabel(ModifGpeQuantiWin, text=gettextRcmdr("Status of the group:")), tklabel(ModifGpeQuantiWin, text=gettextRcmdr("Active")),etatModif.actif.check, tklabel(ModifGpeQuantiWin, text=gettextRcmdr("Supplementary")),etatModif.illu.check, sticky="w")
      tkgrid(tklabel(ModifGpeQuantiWin, text=""))      
      tkgrid(tklabel(ModifGpeQuantiWin, text=gettextRcmdr("Scaled the variables of the group:")), tklabel(ModifGpeQuantiWin, text="Yes"),normModif.ok.check, tklabel(ModifGpeQuantiWin, text=gettextRcmdr("No")),normModif.nok.check, sticky="w")
      tkgrid(tklabel(ModifGpeQuantiWin, text=""))
      tkgrid(tklabel(ModifGpeQuantiWin, text = gettextRcmdr("Select the variables of the group"), fg = "blue"), column=0, columnspan = 5, sticky = "w")
      tkgrid(listModifVarQuanti, scrModifVarQuanti, sticky = "nw")
      tkgrid.configure(scrModifVarQuanti, sticky = "wns", column=4,columnspan=1)
      tkgrid.configure(listModifVarQuanti, sticky = "ew", column=0, columnspan=4)
      tkgrid(tklabel(ModifGpeQuantiWin, text=""))
      tkgrid(MGQ.but, column=2,columnspan=1, sticky="ew")
      tkgrid.columnconfigure(ModifGpeQuantiWin,0, minsize=55)
      tkgrid.columnconfigure(ModifGpeQuantiWin,1, minsize=55)
      tkgrid.columnconfigure(ModifGpeQuantiWin,2, minsize=55)
      tkgrid.columnconfigure(ModifGpeQuantiWin,3, minsize=55)
      tkgrid.columnconfigure(ModifGpeQuantiWin,4, minsize=55)             
   }
   ModifGpeQuantiFrame<-tkframe(ListeQuantiFrame)
   ModifGpeQuanti.but<-tkbutton(ModifGpeQuantiFrame, textvariable=.ModifQuantiLabel, command=OnMGQ, borderwidth=3)
   tkgrid(ModifGpeQuanti.but, sticky="ew")
  })    
  



  #! Suppression de groupes qualitatifs
  supprimeQuali.funct<-defmacro(label, expr=
  {
    env<-environment()
    OnSGQl<-function()
    {
      grpeActSupprime<-listQualiAct.nom[as.numeric(tkcurselection(listQualiAct))+1]
      if(length(grpeActSupprime)>=1) { 
        listQualiAct.nom.tmp<-listQualiAct.nom[-which(listQualiAct.nom %in% grpeActSupprime)]
        assign("listQualiAct.nom",listQualiAct.nom.tmp, envir=env)
        tkdelete(listQualiAct,"0","end")
        if(length(listQualiAct.nom)>=1) {
          for (grpe in listQualiAct.nom) tkinsert(listQualiAct, "end", grpe)
        }  
      }
      grpeIlluSupprime<-listQualiIllu.nom[as.numeric(tkcurselection(listQualiIllu))+1]
      if(length(grpeIlluSupprime)>=1) { 
        listQualiIllu.nom.tmp<-listQualiIllu.nom[-which(listQualiIllu.nom %in% grpeIlluSupprime)]
        assign("listQualiIllu.nom",listQualiIllu.nom.tmp, envir=env)
        tkdelete(listQualiIllu,"0","end")
        if(length(listQualiIllu.nom)>=1) {
          for (grpe in listQualiIllu.nom) tkinsert(listQualiIllu, "end", grpe)
        }  
      }
      nb.grpe<-length(listQualiAct.nom) + length(listQualiIllu.nom)
      if (nb.grpe>=1) {
        tclvalue(label.qualiFrame.var)<-paste(nb.grpe, gettextRcmdr("qualitative groups"), sep=" ")
        tkconfigure(label.qualiFrame)
      }  
      else {
        tclvalue(label.qualiFrame.var)<-paste("0", gettextRcmdr("qualitative group"), sep=" ")
        tkconfigure(label.qualiFrame)
      }  
    }
    
    SupGpeQualiFrame<-tkframe(ListeQualiFrame)
    SupGpeQuali.but<-tkbutton(SupGpeQualiFrame, textvariable=tclVar(label), command=OnSGQl, borderwidth=3)
   tkgrid(SupGpeQuali.but, sticky="ew")  
  })

  
  
  
  
  #! Ajout d'un groupe qualitatif 
  ajoutQuali.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.GQl<-1
    nbGrpeQualiAct<-0
    nbGrpeQualiIllu<-0
    .AjoutQualiLabel<-tclVar(paste(firstLabel, "!", sep=" "))
    OnAGQl<-function()
    {
      AjoutGpeQualiWin<-tktoplevel()
      tkwm.title(AjoutGpeQualiWin,gettextRcmdr("Construction of a qualitative group"))
      
      #création de la fonction AGA.OK
      AGQl.OK<-function()
      {
        assign("compteur.GQl", compteur.GQl+1, envir=env)
        nom.groupe<-nom.correct(tclvalue(nomGrpeQuali.val), liste=c(listQualiAct.nom,listQualiIllu.nom))
        if (nom.groupe=="") tkmessageBox(message=gettextRcmdr("Name for the group"), icon="warning", type="ok") 
        else
        {
          varGroupe<-listVarQuali.nom[as.numeric(tkcurselection(listVarQuali))+1]
          if (length(varGroupe)>=1) {
            assign(paste(nom.groupe,".var", sep=""), c("n",varGroupe), envir=env)
            if(tclvalue(etat.Value)=="actif") {  
              tkinsert(listQualiAct,"end",nom.groupe)
              assign("listQualiAct.nom", c(listQualiAct.nom, nom.groupe),envir=env)
            }  
            if(tclvalue(etat.Value)=="illu") {
              tkinsert(listQualiIllu,"end",nom.groupe)
              assign("listQualiIllu.nom",c(listQualiIllu.nom, nom.groupe),envir=env)
            }  
            tclvalue(label.qualiFrame.var)<-paste(length(listQualiAct.nom) + length(listQualiIllu.nom), "qualitative groups", sep=" ")
            tkconfigure(label.qualiFrame) 
            tkdestroy(AjoutGpeQualiWin)
          }
        }  
      }
      
      
      # choix du nom du groupe
      nomGrpeQuali.lab<-tklabel(AjoutGpeQualiWin,text=gettextRcmdr("Name of the group: "))
      nomGrpeQuali.val<-tclVar(paste("Gq", compteur.GQl, sep=""))
      nomGrpeQuali<-tkentry(AjoutGpeQualiWin,width=15,textvariable=nomGrpeQuali.val)
      
      # choix de l'état actif ou illustratif
      etat.actif.check<-tkradiobutton(AjoutGpeQualiWin)
      etat.illu.check<-tkradiobutton(AjoutGpeQualiWin)
      etat.Value<-tclVar("actif")
      tkconfigure(etat.actif.check,variable=etat.Value,value="actif")
      tkconfigure(etat.illu.check,variable=etat.Value, value="illu")
      
      # création de la liste pour le choix des variables acives
      listVarQuali<-tklistbox(AjoutGpeQualiWin, selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrVarQuali,...))
      scrVarQuali<-tkscrollbar(AjoutGpeQualiWin,repeatinterval=5,command=function(...)tkyview(listVarQuali,...))
      listVarQuali.nom<-NULL
      for (i in (1:ncol(donnee))) {
        if (is.factor(donnee[,i])) {
          tkinsert(listVarQuali,"end",vars[i])
          listVarQuali.nom<-c(listVarQuali.nom, vars[i])
        }
      }
      
      AGQl.but<-tkbutton(AjoutGpeQualiWin, text="OK", width=16, command=AGQl.OK)
      
      tkgrid(nomGrpeQuali.lab, nomGrpeQuali)
      tkgrid.configure(nomGrpeQuali.lab, column=0, columnspan=2, sticky="w")
      tkgrid.configure(nomGrpeQuali, column=2, columnspan=3)      
      tkgrid(tklabel(AjoutGpeQualiWin, text=""))      
      tkgrid(tklabel(AjoutGpeQualiWin, text=gettextRcmdr("Status of the group:")), tklabel(AjoutGpeQualiWin, text=gettextRcmdr("Active")),etat.actif.check, tklabel(AjoutGpeQualiWin, text=gettextRcmdr("Supplementary")),etat.illu.check, sticky="w")
      tkgrid(tklabel(AjoutGpeQualiWin, text=""))
      tkgrid(tklabel(AjoutGpeQualiWin, text = gettextRcmdr("Select the variables of the group"), fg = "blue"), column=0, columnspan = 5, sticky = "w")
      tkgrid(listVarQuali, scrVarQuali, sticky = "nw")
      tkgrid.configure(scrVarQuali, sticky = "wns", column=4,columnspan=1)
      tkgrid.configure(listVarQuali, sticky = "ew", column=0, columnspan=4)
      tkgrid(tklabel(AjoutGpeQualiWin, text=""))
      tkgrid(AGQl.but, column=2,columnspan=1, sticky="ew")
      tkgrid.columnconfigure(AjoutGpeQualiWin,0, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQualiWin,1, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQualiWin,2, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQualiWin,3, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQualiWin,4, minsize=55)              
   }
   GpeQualiFrame<-tkframe(ListeQualiFrame)
   AjoutGpeQuali.but<-tkbutton(GpeQualiFrame, textvariable=.AjoutQualiLabel, command=OnAGQl, borderwidth=3)
   tkgrid(AjoutGpeQuali.but, sticky="ew")
  })    


    #! Modification d'un groupe qualitatif
  modifQuali.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    .ModifQualiLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnMGQl<-function()
    {
      ModifGpeQualiWin<-tktoplevel()
      tkwm.title(ModifGpeQualiWin,gettextRcmdr("Modification of a qualitative group"))
      
      #création de la fonction AGA.OK
      MGQl.OK<-function()
      {
        nom.groupe<-nom.correct(tclvalue(nomModifGrpeQuali.val), liste=c(listQualiAct.nom,listQualiIllu.nom))
        if (nom.groupe=="") tkmessageBox(message=gettextRcmdr("Give a name for the group"), icon="warning", type="ok") 
        else {
          if(etat=="actif") {
            listQualiAct.nom.tmp<-listQualiAct.nom[-which(listQualiAct.nom== grpeAModifier)]
            assign("listQualiAct.nom",listQualiAct.nom.tmp, envir=env)
            tkdelete(listQualiAct,"0","end")
            for (grpe in listQualiAct.nom) tkinsert(listQualiAct, "end", grpe)
          }          
          if(etat=="illu") {
            listQualiIllu.nom.tmp<-listQualiIllu.nom[-which(listQualiIllu.nom== grpeAModifier)]
            assign("listQualiIllu.nom",listQualiIllu.nom.tmp, envir=env)
            tkdelete(listQualiIllu,"0","end")
            for (grpe in listQualiIllu.nom) tkinsert(listQualiIllu, "end", grpe)
          }          
          varGroupe<-listModifVarQuali.nom[as.numeric(tkcurselection(listModifVarQuali))+1]
          if (length(varGroupe)>=1) {
            assign(paste(nom.groupe,".var", sep=""), c("n", varGroupe), envir=env)
            if(tclvalue(etatModif.Value)=="actif") {  
              tkinsert(listQualiAct,"end",nom.groupe)
              assign("listQualiAct.nom", c(listQualiAct.nom, nom.groupe),envir=env)
            }  
            if(tclvalue(etatModif.Value)=="illu") {
              tkinsert(listQualiIllu,"end",nom.groupe)
              assign("listQualiIllu.nom",c(listQualiIllu.nom, nom.groupe),envir=env)              
            }
              
            tkdestroy(ModifGpeQualiWin)
          }
        }  
      }
      
      
      if(length(as.numeric(tkcurselection(listQualiAct)))>=1) {
        grpeAModifier<-listQualiAct.nom[as.numeric(tkcurselection(listQualiAct))+1][1]
        etat<-"actif"
      }
      else if (length(as.numeric(tkcurselection(listQualiIllu)))>=1) {
        grpeAModifier<-listQualiIllu.nom[as.numeric(tkcurselection(listQualiIllu))+1][1]
        etat<-"illu"
      }
      else  {
        tkdestroy(ModifGpeQualiWin)
        return()
      }  
        
       eval(parse(text=paste("grpeAModifier.var<-",paste(grpeAModifier,".var", sep=""),sep="")))
      # choix du nom du groupe
      nomModifGrpeQuali.lab<-tklabel(ModifGpeQualiWin,text=gettextRcmdr("Name of the group: "))
      nomModifGrpeQuali.val<-tclVar(grpeAModifier)
      nomModifGrpeQuali<-tkentry(ModifGpeQualiWin,width=15,textvariable=nomModifGrpeQuali.val)
      
      # choix de l'état actif ou illustratif
      etatModif.actif.check<-tkradiobutton(ModifGpeQualiWin)
      etatModif.illu.check<-tkradiobutton(ModifGpeQualiWin)
      etatModif.Value<-tclVar(etat)
      tkconfigure(etatModif.actif.check,variable=etatModif.Value,value="actif")
      tkconfigure(etatModif.illu.check,variable=etatModif.Value, value="illu")
      
      # création de la liste pour le choix des variables actives
      listModifVarQuali<-tklistbox(ModifGpeQualiWin, selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrModifVarQuali,...))
      scrModifVarQuali<-tkscrollbar(ModifGpeQualiWin,repeatinterval=5,command=function(...)tkyview(listModifVarQuali,...))
      listModifVarQuali.nom<-NULL
      indice.num<-0
      for (i in (1:ncol(donnee))) {
        if (is.factor(donnee[,i])) {
          tkinsert(listModifVarQuali,"end",vars[i])
          listModifVarQuali.nom<-c(listModifVarQuali.nom, vars[i])
          if(vars[i] %in% grpeAModifier.var[-1]) tkselection.set(listModifVarQuali, indice.num)
          indice.num<-indice.num+1
        }
      }
      
      MGQl.but<-tkbutton(ModifGpeQualiWin, text="OK", width=16, command=MGQl.OK)
      
      tkgrid(nomModifGrpeQuali.lab, nomModifGrpeQuali)
      tkgrid.configure(nomModifGrpeQuali.lab, column=0, columnspan=2, sticky="w")
      tkgrid.configure(nomModifGrpeQuali, column=2, columnspan=3)      
      tkgrid(tklabel(ModifGpeQualiWin, text=""))      
      tkgrid(tklabel(ModifGpeQualiWin, text=gettextRcmdr("Status of the group:")), tklabel(ModifGpeQualiWin, text=gettextRcmdr("Active")),etatModif.actif.check, tklabel(ModifGpeQualiWin, text=gettextRcmdr("Supplementary")),etatModif.illu.check, sticky="w")
      tkgrid(tklabel(ModifGpeQualiWin, text=""))      
      tkgrid(tklabel(ModifGpeQualiWin, text = gettextRcmdr("Select the variables of the group"), fg = "blue"), column=0, columnspan = 5, sticky = "w")
      tkgrid(listModifVarQuali, scrModifVarQuali, sticky = "nw")
      tkgrid.configure(scrModifVarQuali, sticky = "wns", column=4,columnspan=1)
      tkgrid.configure(listModifVarQuali, sticky = "ew", column=0, columnspan=4)
      tkgrid(tklabel(ModifGpeQualiWin, text=""))
      tkgrid(MGQl.but, column=2,columnspan=1, sticky="ew")
      tkgrid.columnconfigure(ModifGpeQualiWin,0, minsize=55)
      tkgrid.columnconfigure(ModifGpeQualiWin,1, minsize=55)
      tkgrid.columnconfigure(ModifGpeQualiWin,2, minsize=55)
      tkgrid.columnconfigure(ModifGpeQualiWin,3, minsize=55)
      tkgrid.columnconfigure(ModifGpeQualiWin,4, minsize=55)              
   }
   ModifGpeQualiFrame<-tkframe(ListeQualiFrame)
   ModifGpeQuali.but<-tkbutton(ModifGpeQualiFrame, textvariable=.ModifQualiLabel, command=OnMGQl, borderwidth=3)
   tkgrid(ModifGpeQuali.but, sticky="ew")
  })    
  

  #! fonction pour le choix des individus supplémentaires 
  Iillu.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    individuillu<-NULL
    .IilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnIillu<-function()
    {   
      IilluWin<-tktoplevel()
      tkwm.title(IilluWin,gettextRcmdr("Select supplementary individuals"))
      #création de la fonction IOK.funct
      IOK.funct<-function()
      {
        ind.select<-rows[as.numeric(tkcurselection(listind))+1]
        if(length(ind.select)==0) {
          assign("individuillu", NULL, envir=env)
          tclvalue(.IilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Iillu.but, fg="black")
          tkdestroy(IilluWin)
          return()
        }
        assign("individuillu", ind.select, envir=env)
        tclvalue(.IilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Iillu.but, fg="blue")
        tkdestroy(IilluWin)
      }
      
      # création et mise en page de la fenetre Fillu
      listind<-tklistbox(IilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrind,...)) # Liste vide
      scrind <-tkscrollbar(IilluWin,repeatinterval=5,command=function(...)tkyview(listind,...)) 
      indice<-0
      for (i in (1:nrow(donnee))) {
        tkinsert(listind,"end",rows[i]) # On renseigne la liste
        if(rows[i] %in% individuillu) tkselection.set(listind, indice)
        indice<-indice+1
      }
  
      IOK.but<-tkbutton(IilluWin, text="OK", width=16,command=IOK.funct)

      tkgrid(tklabel(IilluWin, text=""))
      tkgrid(tklabel(IilluWin, text = gettextRcmdr("Select supplementary individuals"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
      tkgrid(listind, scrind, sticky = "nw")
      tkgrid.configure(scrind, sticky = "ens", columnspan=1)
      tkgrid.configure(listind, sticky = "ew", column=1, columnspan=1)
      tkgrid(tklabel(IilluWin, text=""))
      tkgrid(IOK.but, column=1,columnspan=1, sticky="ew")
      tkgrid(tklabel(IilluWin, text=""))
      tkgrid.columnconfigure(IilluWin,0, minsize=25)
      tkgrid.columnconfigure(IilluWin,2, minsize=25)
  }  

   IilluFrame<-tkframe(IlluFrame)
   Iillu.but<-tkbutton(IilluFrame, textvariable=.IilluLabel, command=OnIillu, borderwidth=3)
   tkgrid(Iillu.but, sticky="ew")
  })
    
  
    #! fonction pour la réinitialisation des paramètres
  Reinitializ.funct<-function()
  {
    tkdestroy(top)
    FactoMFA()
  }


  #! fonction pour le choix des éléments de sortie
  Sortie.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.sortie<-0
    #déclaration des variables
    Rpropre<-FALSE
    RFichier <- ""
    Rgroupe<-FALSE
    Rinertie<-FALSE
    Rindividu<-FALSE
    Rindsup<-FALSE
    Rquantisummary<-FALSE
    Rquanti<-FALSE
    Rquantisup<-FALSE    
    Rqualisummary<-FALSE
    Rquali<-FALSE
    Rqualisup<-FALSE
    Raxepartiel<-FALSE          
    Rdescdim<-FALSE

    .SortieLabel<-tclVar(paste(firstLabel, "", sep=" "))

    OnSortie<-function()
    {
      SortieWin<-tktoplevel()
      tkwm.title(SortieWin,gettextRcmdr("Output options"))

      #création de la fonction onOKsub
      onOK.sortie<-function()
      {
        assign("compteur.sortie", compteur.sortie+1, envir=env)
#        if(compteur.sortie>0) tclvalue(.SortieLabel)<-paste(label, ": Seen", sep=" ")
        if(compteur.sortie>0) tclvalue(.SortieLabel)<-paste(label, "", sep=" ")
        tkconfigure(Sortie.but, fg="blue")
        
        if(tclvalue(eigValue)=="1") assign("Rpropre", TRUE, envir=env)
        else assign("Rpropre", FALSE, envir=env)
        
        if(tclvalue(groupeValue)=="1") assign("Rgroupe", TRUE, envir=env)
        else assign("Rgroupe", FALSE, envir=env)
        
        if(tclvalue(inertieValue)=="1") assign("Rinertie", TRUE, envir=env)
        else assign("Rinertie", FALSE, envir=env)
        
        if(tclvalue(indValue)=="1") assign("Rindividu", TRUE, envir=env)
        else assign("Rindividu", FALSE, envir=env)
        
        if(tclvalue(ind.sup.Value)=="1") assign("Rindsup", TRUE, envir=env)
        else assign("Rindsup", FALSE, envir=env)
        
        if(tclvalue(quantiSummaryValue)=="1") assign("Rquantisummary", TRUE, envir=env)
        else assign("Rquantisummary", FALSE, envir=env)
        
        if(tclvalue(quantiValue)=="1") assign("Rquanti", TRUE, envir=env)
        else assign("Rquanti", FALSE, envir=env)
        
        if(tclvalue(quantisupValue)=="1") assign("Rquantisup", TRUE, envir=env)
        else assign("Rquantisup", FALSE, envir=env)
        
        if(tclvalue(qualiSummaryValue)=="1") assign("Rqualisummary", TRUE, envir=env)
        else assign("Rqualisummary", FALSE, envir=env)
        
        if(tclvalue(qualiValue)=="1") assign("Rquali", TRUE, envir=env)
        else assign("Rquali", FALSE, envir=env)
        
        if(tclvalue(qualisupValue)=="1") assign("Rqualisup", TRUE, envir=env)
        else assign("Rqualisup", FALSE, envir=env)
        
        if(tclvalue(axepartielValue)=="1") assign("Raxepartiel", TRUE, envir=env)
        else assign("Raxepartiel", FALSE, envir=env)
        
        if(tclvalue(descdimValue)=="1") assign("Rdescdim", TRUE, envir=env)
        else assign("Rdescdim", FALSE, envir=env)
        
        if (tclvalue(Fichier)=="") assign("RFichier", NULL, envir=env)
        assign("RFichier", tclvalue(Fichier), envir=env)

        tkdestroy(SortieWin)
      
      }
      
      eig.lab <-tklabel(SortieWin, text=gettextRcmdr("Eigenvalues"))
        eig.check <- tkcheckbutton(SortieWin)
        if(Rpropre) eigValue <- tclVar("1")
        else eigValue <- tclVar("0")
        tkconfigure(eig.check,variable=eigValue)
        
        groupe.lab <-tklabel(SortieWin, text=gettextRcmdr("Results for the groups"))
        groupe.check <- tkcheckbutton(SortieWin)
        if(Rgroupe) groupeValue <- tclVar("1")
        else groupeValue <- tclVar("0")
        tkconfigure(groupe.check,variable=groupeValue)
        
        inertie.lab <-tklabel(SortieWin, text=gettextRcmdr("Inertia"))
        inertie.check <- tkcheckbutton(SortieWin)
        if(Rinertie) inertieValue <- tclVar("1")
        else inertieValue <- tclVar("0")
        tkconfigure(inertie.check,variable=inertieValue)

      ind.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for the active individuals"))
        ind.check <- tkcheckbutton(SortieWin)
        if(Rindividu) indValue <- tclVar("1")
        else indValue <- tclVar("0")
        tkconfigure(ind.check,variable=indValue)

      ind.sup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for the supplementary individuals"))
        ind.sup.check <- tkcheckbutton(SortieWin)
        if(Rindsup) ind.sup.Value <- tclVar("1")
        else ind.sup.Value <- tclVar("0")
        tkconfigure(ind.sup.check,variable=ind.sup.Value)
        
      quantiSummary.lab<-tklabel(SortieWin,text=gettextRcmdr("Summary of the quantitative variables"))
        quantiSummary.check <- tkcheckbutton(SortieWin)
        if(Rquantisummary) quantiSummaryValue <- tclVar("1")
        else quantiSummaryValue <- tclVar("0")
        tkconfigure(quantiSummary.check,variable=quantiSummaryValue)
      
      quanti.lab<-tklabel(SortieWin,text=gettextRcmdr("Results of the quantitative variables"))
        quanti.check <- tkcheckbutton(SortieWin)
        if(Rquanti) quantiValue <- tclVar("1")
        else quantiValue <- tclVar("0")
        tkconfigure(quanti.check,variable=quantiValue)

      quantisup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results of the supplementary quantitative variables"))
        quantisup.check <- tkcheckbutton(SortieWin)
        if(Rquantisup) quantisupValue <- tclVar("1")
        else quantisupValue <- tclVar("0")
        tkconfigure(quantisup.check,variable=quantisupValue)
      
      qualiSummary.lab<-tklabel(SortieWin,text=gettextRcmdr("Summary of the qualitative variables"))
        qualiSummary.check <- tkcheckbutton(SortieWin)
        if(Rqualisummary) qualiSummaryValue <- tclVar("1")
        else qualiSummaryValue <- tclVar("0")
        tkconfigure(qualiSummary.check,variable=qualiSummaryValue)
      
      quali.lab<-tklabel(SortieWin,text=gettextRcmdr("Results of the qualitative variables"))
        quali.check <- tkcheckbutton(SortieWin)
        if(Rquali) qualiValue <- tclVar("1")
        else qualiValue <- tclVar("0")
        tkconfigure(quali.check,variable=qualiValue)

      qualisup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results of the supplementary qualitative variables"))
        qualisup.check <- tkcheckbutton(SortieWin)
        if(Rqualisup) qualisupValue <- tclVar("1")
        else qualisupValue <- tclVar("0")
        tkconfigure(qualisup.check,variable=qualisupValue)
      
      axepartiel.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for the partial axes"))
        axepartiel.check <- tkcheckbutton(SortieWin)
        if(Raxepartiel) axepartielValue <- tclVar("1")
        else axepartielValue <- tclVar("0")
        tkconfigure(axepartiel.check,variable=axepartielValue)
        
        descdim.lab<-tklabel(SortieWin, text=gettextRcmdr("Description of the dimensions"))
      descdim.check<-tkcheckbutton(SortieWin)
      if(Rdescdim) descdimValue<-tclVar("1")
      else descdimValue<-tclVar("0")
      tkconfigure(descdim.check,variable=descdimValue)

      RFichierFrame<-tkframe(SortieWin,borderwidth=2)
      if (is.null(RFichier)) Fichier <- tclVar("")
      else Fichier<-tclVar(RFichier)
      Fichier.entry <-tkentry(RFichierFrame,width="40",textvariable=Fichier)
      tkgrid(tklabel(RFichierFrame,text=gettextRcmdr("Print results on a 'csv' file")),Fichier.entry)

      SortieOK.but<-tkbutton(SortieWin,text="OK",width=16,command=onOK.sortie)

      tkgrid(tklabel(SortieWin, text = gettextRcmdr("Select output options"), fg ="blue"),  columnspan = 2, sticky = "w")
      tkgrid(tklabel(SortieWin, text = " "))
      tkgrid(eig.lab,eig.check,sticky="w")
      tkgrid(groupe.lab,groupe.check,sticky="w")
      tkgrid(inertie.lab,inertie.check,sticky="w")
      tkgrid(ind.lab,ind.check,sticky="w")
    nb.GQA<-length(listQuantiAct.nom)
    nb.GQI<-length(listQuantiIllu.nom)
    nb.GQlA<-length(listQualiAct.nom)
    nb.GQlI<-length(listQualiIllu.nom)
      if (!is.null(individuillu))tkgrid(ind.sup.lab,ind.sup.check,sticky="w")
      if (nb.GQA+ nb.GQI>0) tkgrid(quantiSummary.lab,quantiSummary.check,sticky="w")
      if (nb.GQA>0) tkgrid(quanti.lab,quanti.check,sticky="w")      
      if (nb.GQI>0) tkgrid(quantisup.lab,quantisup.check,sticky="w")
      if (nb.GQlA+ nb.GQlI>0) tkgrid(qualiSummary.lab,qualiSummary.check,sticky="w")
      if (nb.GQlA>0) tkgrid(quali.lab,quali.check,sticky="w")        
      if (nb.GQlI>0) tkgrid(qualisup.lab,qualisup.check,sticky="w")
      tkgrid(axepartiel.lab,axepartiel.check,sticky="w")      
      tkgrid(descdim.lab,descdim.check,sticky="w")
      tkgrid(tklabel(SortieWin, text = " "))
      tkgrid(RFichierFrame)
      tkgrid(SortieOK.but)
   }
    
    SortieFrame<-tkframe(IlluFrame)
    Sortie.but<-tkbutton(SortieFrame, textvariable=.SortieLabel, command=OnSortie, borderwidth=3)
    tkgrid(Sortie.but, sticky="ew")
  })



  #! fonction pour la gestion des options graphiques 
  PLOT.MFA<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.graph<-0
    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))
    #déclaration des variables
    Gchoix<-TRUE
    GTitle<-NULL
    GAxeGrpe<-c(1,2)
    Glabel<-TRUE
        
    Rchoix<-TRUE
    RTitle<-NULL
    Rlabel.indMoy<-TRUE
    Rlabel.indPar<-TRUE
    Rlabel.quali<-TRUE    
    Rhabillage<-"group"
    Rinvisible<-NULL
    Rpartial<-NULL
    RpartialSouris<-FALSE
    Rchrono<-FALSE
    RXlimInd<-NULL
    RYlimInd<-NULL
    
    Wchoix<-TRUE
    WTitle<-NULL
    WAxeVar<-c(1,2)
    Wlabel.var<-TRUE
    Whabillage<-"group"
    Winvisible<-NULL
    Wlim.cos<-0.
    
    Achoix<-TRUE
    ATitle<-NULL
    AAxeAxe<-c(1,2)
    Ahabillage<-"group"
        
    OnPlot<-function()
    {
      PlotWin<-tktoplevel()
      tkwm.title(PlotWin,gettextRcmdr("Graphical options"))
      tkwm.geometry(PlotWin, "-100+50")
      PlotWin2<-tkframe(PlotWin)

      #création de la fonction onOKsub
      onOKsub<-function()
      {
        assign("compteur.graph", compteur.graph+1, envir=env)
        if(compteur.graph>0) tclvalue(.PlotLabel)<-paste(label, gettextRcmdr(""), sep=" ")
        tkconfigure(Plot.but, fg="blue")

        # gestion des entrées de la partie graphique des Groupes
        if(tclvalue(grpe.check.value)==1) assign("Gchoix", TRUE, envir=env)
        else assign("Gchoix", FALSE, envir=env)

        if(Gchoix) {
          if (tclvalue(GTitre)==" ") assign("GTitle", NULL, envir=env)
          assign("GTitle", tclvalue(GTitre), envir=env)

          #assign("GAxeGrpe", c(as.numeric(tclvalue(AxeGrpe1)), as.numeric(tclvalue(AxeGrpe2))), envir=env)
          
          label.tmp.grpe<-tclvalue(label.grpe.checkValue)
          if(label.tmp.grpe==1) assign("Glabel", TRUE, envir=env)
          else assign("Glabel", FALSE, envir=env)
        }
        
        # gestion des entrées de la partie graphique des Groupes
        if(tclvalue(axe.check.value)==1) assign("Achoix", TRUE, envir=env)
        else assign("Achoix", FALSE, envir=env)

        if(Achoix) {
          if (tclvalue(ATitre)==" ") assign("ATitle", NULL, envir=env)
          assign("ATitle", tclvalue(ATitre), envir=env)

          habillage.tmp.axe<-tclvalue(Ahabillage.checkValue)
          if(habillage.tmp.axe==1) assign("Ahabillage", "group", envir=env)
          else assign("Ahabillage", "none", envir=env)
        }
            
        # gestion des entrées de la partie graphique des variables
        if(tclvalue(var.check.value)==1) assign("Wchoix", TRUE, envir=env)
        else assign("Wchoix", FALSE, envir=env)

        if(Wchoix) {
          if (tclvalue(WTitre)==" ") assign("WTitle", NULL, envir=env)
          else assign("WTitle", tclvalue(WTitre), envir=env)

          assign("Wlim.cos", tclvalue(WlimCosValue), envir=env)

          if(tclvalue(label.var.checkValue)==1) assign("Wlabel.var", TRUE, envir=env)
          else assign("Wlabel.var", FALSE, envir=env)

          if(tclvalue(Whabillage.checkValue)==1) assign("Whabillage", "group", envir=env)
          else assign("Whabillage", "none", envir=env)
          
          if(tclvalue(inv.Value)=="aucun")  assign("Winvisible", NULL, envir=env)
          else assign("Winvisible", tclvalue(inv.Value), envir=env)
        }

        # gestion des entrées de la partie graphique des individus
        if(tclvalue(ind.check.value)==1) assign("Rchoix", TRUE, envir=env)
        else assign("Rchoix", FALSE, envir=env)

        if(Rchoix) {
          if (tclvalue(Titre)==" ") assign("RTitle", NULL, envir=env)
          assign("RTitle", tclvalue(Titre), envir=env)

          label.tmp.indMoy<-tclvalue(label.indMoy.checkValue)
          label.tmp.indPar<-tclvalue(label.indPar.checkValue)
          label.tmp.quali<-tclvalue(label.quali.checkValue)
          if(label.tmp.indMoy==1) assign("Rlabel.indMoy", TRUE, envir=env)
          else assign("Rlabel.indMoy", FALSE, envir=env)
          if(label.tmp.indPar==1) assign("Rlabel.indPar", TRUE, envir=env)
          else assign("Rlabel.indPar", FALSE, envir=env)
          if(label.tmp.quali==1) assign("Rlabel.quali", TRUE, envir=env)
          else assign("Rlabel.quali", FALSE, envir=env)
          
          habillage.tmp<-listgraph.nom[as.numeric(tkcurselection(listgraph))+1]
          if(length(habillage.tmp)==0) assign("Rhabillage","none", envir=env)
          else assign("Rhabillage", habillage.tmp, envir=env)

          if(tclvalue(XlimIndMin)=="" | tclvalue(XlimIndMax)=="") assign("RXlimInd", NULL, envir=env)
          else assign("RXlimInd", c(as.numeric(tclvalue(XlimIndMin)), as.numeric(tclvalue(XlimIndMax))), envir=env)
          if(tclvalue(YlimIndMin)=="" | tclvalue(YlimIndMax)=="") assign("RYlimInd", NULL, envir=env)
          else assign("RYlimInd", c(as.numeric(tclvalue(YlimIndMin)), as.numeric(tclvalue(YlimIndMax))), envir=env)
          
          partial.tmp<-listpartial.nom[as.numeric(tkcurselection(listpartial))+1]
          if(length(partial.tmp)==0) assign("Rpartial",NULL, envir=env)
          else assign("Rpartial", partial.tmp, envir=env)
          
          chrono.tmp<-tclvalue(partial.chrono.checkValue)
          if(chrono.tmp=="1") assign("Rchrono", TRUE, envir=env)
          else assign("Rchrono", FALSE, envir=env)
          
          souris.tmp<-tclvalue(partial.souris.checkValue)
          if(souris.tmp=="1") assign("RpartialSouris", TRUE, envir=env)
          else assign("RpartialSouris", FALSE, envir=env)
          
          inv.ind.tmp<-tclvalue(inv.ind.checkValue)
          inv.ind.sup.tmp<-tclvalue(inv.ind.sup.checkValue)
          inv.quali.tmp<-tclvalue(inv.quali.checkValue)
          assign("Rinvisible", NULL, envir=env)
          if(inv.ind.tmp=="1") assign("Rinvisible", c(Rinvisible, "ind"), envir=env)
          if(inv.ind.sup.tmp=="1") assign("Rinvisible", c(Rinvisible, "ind.sup"), envir=env)          
          if(inv.quali.tmp=="1") assign("Rinvisible", c(Rinvisible, "quali"), envir=env)          
        }
        tkdestroy(PlotWin)
      }
    
      # création l'interface "options graphiques"
    
      ##########################
      # construction de la partie graphique des Groupes
      PlotGrpeFrame<-tkframe(PlotWin2, borderwidth=5, relief="groove")
  
      GchoixFrame<-tkframe(PlotGrpeFrame,borderwidth=2)
      grpe.check<-tkcheckbutton(GchoixFrame)
      if(Gchoix) grpe.check.value<-tclVar("1")
      else grpe.check.value<-tclVar("0")
      tkconfigure(grpe.check, variable=grpe.check.value)
      tkgrid(tklabel(GchoixFrame, text=gettextRcmdr("Graph of the groups"), font=font2),grpe.check)
      tkgrid(tklabel(GchoixFrame, text="  "))
  
      GTitleFrame<-tkframe(PlotGrpeFrame,borderwidth=2)
      if (is.null(GTitle)) GTitre <- tclVar(" ")
      else GTitre<-tclVar(GTitle)
      GTitre.entry <-tkentry(GTitleFrame,width="40",textvariable=GTitre)
      tkgrid(tklabel(GTitleFrame,text=gettextRcmdr("Title of the graph")),GTitre.entry)
    
      GlabelFrame<-tkframe(PlotGrpeFrame,borderwidth=2)
      label.grpe.check<-tkcheckbutton(GlabelFrame)
      if (Glabel) label.grpe.checkValue<-tclVar("1")
      else label.grpe.checkValue<-tclVar("0")
      tkconfigure(label.grpe.check, variable=label.grpe.checkValue)
      tkgrid(tklabel(GlabelFrame, text=gettextRcmdr("Draw labels for the groups")),label.grpe.check)
  
     
      #mise en page des différents frames de PlotGrpeFrame
      tkgrid(GchoixFrame)
      tkgrid(GTitleFrame)
      tkgrid(GlabelFrame)
      tkgrid(tklabel(PlotGrpeFrame, text=" "))
      
      
      ##########################
      # construction de la partie graphique des Axes partiels
      PlotAxeFrame<-tkframe(PlotWin2, borderwidth=5, relief="groove")
  
      AchoixFrame<-tkframe(PlotAxeFrame,borderwidth=2)
      axe.check<-tkcheckbutton(AchoixFrame)
      if(Achoix) axe.check.value<-tclVar("1")
      else axe.check.value<-tclVar("0")
      tkconfigure(axe.check, variable=axe.check.value)
      tkgrid(tklabel(AchoixFrame, text=gettextRcmdr("Graph of the partial axes"), font=font2),axe.check)
      tkgrid(tklabel(AchoixFrame, text="  "))
  
      ATitleFrame<-tkframe(PlotAxeFrame,borderwidth=2)
      if (is.null(ATitle)) ATitre <- tclVar(" ")
      else ATitre<-tclVar(ATitle)
      ATitre.entry <-tkentry(ATitleFrame,width="40",textvariable=ATitre)
      tkgrid(tklabel(ATitleFrame,text=gettextRcmdr("Title of the graph")),ATitre.entry)
  
      AhabillageFrame<-tkframe(PlotAxeFrame,borderwidth=2)
      Ahabillage.check<-tkcheckbutton(AhabillageFrame)
      if (Ahabillage=="group") Ahabillage.checkValue<-tclVar("1")
      else Ahabillage.checkValue<-tclVar("0")
      tkconfigure(Ahabillage.check, variable=Ahabillage.checkValue)
      tkgrid(tklabel(AhabillageFrame, text=gettextRcmdr("Color the partial axes by group")),Ahabillage.check)
           
      #mise en page des différents frames de PlotGrpeFrame
      tkgrid(AchoixFrame)
      tkgrid(ATitleFrame)
      tkgrid(AhabillageFrame)
      tkgrid(tklabel(PlotAxeFrame, text=" "))
      
      ########################

      # construction de la partie graphique des variables
      PlotVarFrame<-tkframe(PlotWin2, borderwidth=5, relief="groove")
  
      WchoixFrame<-tkframe(PlotVarFrame,borderwidth=2)
      var.check<-tkcheckbutton(WchoixFrame)
      if(Wchoix) var.check.value<-tclVar("1")
      else var.check.value<-tclVar("0")
      tkconfigure(var.check, variable=var.check.value)
      tkgrid(tklabel(WchoixFrame, text=gettextRcmdr("Graph of the variables"), font=font2),var.check)
      tkgrid(tklabel(WchoixFrame, text="  "))
  
      WTitleFrame<-tkframe(PlotVarFrame,borderwidth=2)
      if(is.null(WTitle)) WTitre <- tclVar(" ")
      else WTitre<-tclVar(WTitle)
      WTitre.entry <-tkentry(WTitleFrame,width="40",textvariable=WTitre) 
      tkgrid(tklabel(WTitleFrame,text=gettextRcmdr("Title of the graph")),WTitre.entry)
  
      WcosFrame<-tkframe(PlotVarFrame,borderwidth=2)
      WlimCosValue<-tclVar(paste(Wlim.cos))
      WlimCos.entry<-tkentry(WcosFrame, width=5, textvariable=WlimCosValue)
      tkgrid(tklabel(WcosFrame,text=gettextRcmdr("Draw variables with a cos2 >:")),WlimCos.entry)
  
      WlabelFrame<-tkframe(PlotVarFrame,borderwidth=2)
      label.var.check<-tkcheckbutton(WlabelFrame)
      if (Wlabel.var) label.var.checkValue<-tclVar("1")
      else label.var.checkValue<-tclVar("0")
      tkconfigure(label.var.check, variable=label.var.checkValue)
      tkgrid(tklabel(WlabelFrame, text=gettextRcmdr("Draw the labels of the variables")),label.var.check)
      
      WhabillageFrame<-tkframe(PlotVarFrame,borderwidth=2)
      Whabillage.check<-tkcheckbutton(WhabillageFrame)
      if (Whabillage=="group") Whabillage.checkValue<-tclVar("1")
      else Whabillage.checkValue<-tclVar("0")
      tkconfigure(Whabillage.check, variable=Whabillage.checkValue)
      tkgrid(tklabel(WhabillageFrame, text=gettextRcmdr("Color the variables by group")),Whabillage.check)
      
      WinvisibleFrame<-tkframe(PlotVarFrame,borderwidth=2)
      inv.aucun.check<-tkradiobutton(WinvisibleFrame)
      inv.act.check<-tkradiobutton(WinvisibleFrame)
      inv.sup.check<-tkradiobutton(WinvisibleFrame)
      if(is.null(Winvisible)) inv.Value<-tclVar("aucun")
      else inv.Value<-tclVar(Winvisible)
      tkconfigure(inv.aucun.check,variable=inv.Value,value="aucun")
      tkconfigure(inv.act.check,variable=inv.Value, value="actif")
      tkconfigure(inv.sup.check,variable=inv.Value, value="sup")
      tkgrid(tklabel(WinvisibleFrame, text=gettextRcmdr("Hide some elements:")), columnspan=6, sticky="w")
      tkgrid(tklabel(WinvisibleFrame, text="None"),inv.aucun.check, tklabel(WinvisibleFrame, text=gettextRcmdr("active variables")),inv.act.check, tklabel(WinvisibleFrame, text=gettextRcmdr("supplementary variables")),inv.sup.check, sticky="w")   
        
      #mise en page des différents frames de PlotVarFrame
      tkgrid(WchoixFrame)
      tkgrid(WTitleFrame)
      tkgrid(WcosFrame)
      tkgrid(WlabelFrame)
      tkgrid(WhabillageFrame)
      tkgrid(WinvisibleFrame)            
      tkgrid(tklabel(PlotVarFrame, text=" "))        
        
      ##########################  
      # construction de la partie graphique des individus
      PlotIndFrame<-tkframe(PlotWin, borderwidth=5, relief="groove")
      
      RchoixFrame<-tkframe(PlotIndFrame,borderwidth=2)
      ind.check<-tkcheckbutton(RchoixFrame)
      if(Rchoix) ind.check.value<-tclVar("1")
      else ind.check.value<-tclVar("0")
      tkconfigure(ind.check, variable=ind.check.value)
      tkgrid(tklabel(RchoixFrame, text=gettextRcmdr("Graph of the individuals"), font=font2),ind.check)
      tkgrid(tklabel(RchoixFrame, text=" "))
      
      RTitleFrame<-tkframe(PlotIndFrame,borderwidth=2)
      if (is.null(RTitle)) Titre <- tclVar(" ")
      else Titre<-tclVar(RTitle)
      Titre.entry <-tkentry(RTitleFrame,width="40",textvariable=Titre)
      tkgrid(tklabel(RTitleFrame,text=gettextRcmdr("Title of the graph")),Titre.entry)
      
      RlabelFrame<-tkframe(PlotIndFrame,borderwidth=2)
      label.indMoy.check<-tkcheckbutton(RlabelFrame)
      if (Rlabel.indMoy) label.indMoy.checkValue<-tclVar("1")
      else label.indMoy.checkValue<-tclVar("0") 
      tkconfigure(label.indMoy.check, variable=label.indMoy.checkValue)
      tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Draw labels for the mean individuals")),label.indMoy.check)
      label.indPar.check<-tkcheckbutton(RlabelFrame)
      if (Rlabel.indPar) label.indPar.checkValue<-tclVar("1")
      else label.indPar.checkValue<-tclVar("0")
      tkconfigure(label.indPar.check, variable=label.indPar.checkValue)
      tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Draw labels for the partial individuals")),label.indPar.check)
      label.quali.check<-tkcheckbutton(RlabelFrame)
      if (Rlabel.quali) label.quali.checkValue<-tclVar("1")
      else label.quali.checkValue<-tclVar("0")
      tkconfigure(label.quali.check, variable=label.quali.checkValue)
      tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Draw labels for the qualitative variables")), label.quali.check)
  
        
      RhabillageFrame<-tkframe(PlotIndFrame,borderwidth=2)
      listgraph<-tklistbox(RhabillageFrame,height=4, selectmode="single",exportselection="FALSE",yscrollcommand=function(...) tkset(scrgraph,...))
      scrgraph <-tkscrollbar(RhabillageFrame,repeatinterval=5,command=function(...)tkyview(listgraph,...))
      listgraph.nom<-c("group","ind")
      tkinsert(listgraph,"end","by.group")
      tkinsert(listgraph,"end","by.individual")
      if(Rhabillage=="group") tkselection.set(listgraph,0)
      if(Rhabillage=="ind") tkselection.set(listgraph,1)
      indice<-2      
##      for (i in 1:ncol(donnee)) {
##          if(is.factor(donnee[,i])) {
##          tkinsert(listgraph,"end",vars[i])
##          listgraph.nom<-c(listgraph.nom,vars[i])
##          if(Rhabillage==vars[i]) tkselection.set(listgraph, indice)
##          indice<-indice+1
##        }
##      }
    if(length(listQualiAct.nom)+length(listQualiIllu.nom)>=1) {
      var.aux = NULL
      if(length(listQualiAct.nom)>=1) {
        for(i in 1:length(listQualiAct.nom)) {
          eval(parse(text=paste("liste.aux.GQlA<-", listQualiAct.nom[i], ".var", sep="")))
          var.aux<-c(var.aux, liste.aux.GQlA[-1])
        }
      }
      if(length(listQualiIllu.nom)>=1) {
        for(i in 1:length(listQualiIllu.nom)) {
          eval(parse(text=paste("liste.aux.GQlI<-", listQualiIllu.nom[i], ".var", sep="")))
          var.aux<-c(var.aux, liste.aux.GQlI[-1])
        }
      }
      for (j in 1:ncol(donnee)){
        if(vars[j] %in% var.aux){
          tkinsert(listgraph,"end",vars[j])
          listgraph.nom<-c(listgraph.nom,vars[j])
          if(Rhabillage==vars[j]) tkselection.set(listgraph, indice)
          indice<-indice+1
        }
      }
    }
      tkgrid(tklabel(RhabillageFrame, text=gettextRcmdr("Select drawing for the individuals")))
      tkgrid(listgraph, scrgraph, sticky = "nw")
      tkgrid.configure(scrgraph, sticky = "wns")
      tkgrid.configure(listgraph, sticky = "ew")
      
      
      RinvisibleFrame<-tkframe(PlotIndFrame,borderwidth=2)
      inv.ind.check<-tkcheckbutton(RinvisibleFrame)
      if ("ind" %in% Rinvisible) inv.ind.checkValue<-tclVar("1")
      else inv.ind.checkValue<-tclVar("0") 
      inv.ind.sup.check<-tkcheckbutton(RinvisibleFrame)
      if ("ind.sup" %in% Rinvisible) inv.ind.sup.checkValue<-tclVar("1")
      else inv.ind.sup.checkValue<-tclVar("0") 
      inv.quali.check<-tkcheckbutton(RinvisibleFrame)      
      if ("quali" %in% Rinvisible) inv.quali.checkValue<-tclVar("1")
      else inv.quali.checkValue<-tclVar("0") 
      tkconfigure(inv.ind.check, variable=inv.ind.checkValue)
      tkconfigure(inv.ind.sup.check, variable=inv.ind.sup.checkValue)
      tkconfigure(inv.quali.check, variable=inv.quali.checkValue)            
      tkgrid(tklabel(RinvisibleFrame, text=gettextRcmdr("Hide some elements:")), columnspan=6, sticky="w")
      tkgrid(tklabel(RinvisibleFrame, text="ind"),inv.ind.check, tklabel(RinvisibleFrame, text="ind sup"),inv.ind.sup.check, tklabel(RinvisibleFrame, text="quali"),inv.quali.check, sticky="w")
      
            
      RlimFrame<-tkframe(PlotIndFrame,borderwidth=2)
      if(is.null(RXlimInd)) XlimIndMin<-tclVar("")
      else XlimIndMin<-tclVar(paste(RXlimInd[1]))
      XlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMin)
      if (is.null(RXlimInd)) XlimIndMax<- tclVar("")
      else XlimIndMax<-tclVar(paste(RXlimInd[1]))
      XlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMax)
      tkgrid(tklabel(RlimFrame,text=gettextRcmdr("x limits of the graph:")),XlimIndMin.entry, XlimIndMax.entry)
      if(is.null(RYlimInd)) YlimIndMin<- tclVar("")
      else YlimIndMin<-tclVar(paste(RYlimInd[1]))
      YlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMin)
      if (is.null(RYlimInd)) YlimIndMax<- tclVar("")
      else YlimIndMax<-tclVar(paste(RYlimInd[2]))
      YlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMax)
      tkgrid(tklabel(RlimFrame,text=gettextRcmdr("y limits of the graph:")),YlimIndMin.entry,YlimIndMax.entry)
  
      RpartialFrame<-tkframe(PlotIndFrame,borderwidth=2)
      listpartial<-tklistbox(RpartialFrame,height=7, selectmode="extended",exportselection="FALSE",yscrollcommand=function(...) tkset(scrpartial,...))
      scrpartial<-tkscrollbar(RpartialFrame,repeatinterval=5,command=function(...)tkyview(listpartial,...))
      listpartial.nom<-NULL
      indice<-0
      for (i in 1:nrow(donnee)) {
        tkinsert(listpartial,"end",rows[i])
        listpartial.nom<-c(listpartial.nom,rows[i])
        if(rows[i] %in% Rpartial) tkselection.set(listpartial, indice)
        indice<-indice+1 
      }
      partial.souris.check<-tkcheckbutton(RpartialFrame)
      if (RpartialSouris) partial.souris.checkValue<-tclVar("1")
      else partial.souris.checkValue<-tclVar("0") 
      partial.chrono.check<-tkcheckbutton(RpartialFrame)
      if (Rchrono) partial.chrono.checkValue<-tclVar("1")
      else partial.chrono.checkValue<-tclVar("0")
      tkconfigure(partial.souris.check, variable=partial.souris.checkValue)
      tkconfigure(partial.chrono.check, variable=partial.chrono.checkValue)      
      tkgrid(tklabel(RpartialFrame, text=gettextRcmdr("Select the individuals for which partial points are drawn")))
      tkgrid(listpartial, scrpartial, sticky = "nw")
      tkgrid.configure(scrpartial, sticky = "wns")
      tkgrid.configure(listpartial, sticky = "ew")
      tkgrid(tklabel(RpartialFrame, text=gettextRcmdr("Interactive selection of the individuals")), partial.souris.check)
      tkgrid(tklabel(RpartialFrame, text=gettextRcmdr("Chronologic representation of the partial points")), partial.chrono.check)
  
  
      #mise en page des différents frames de PlotIndFrame
      tkgrid(RchoixFrame)
      tkgrid(RTitleFrame)
      tkgrid(RlabelFrame)
      tkgrid(RinvisibleFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))
      tkgrid(RhabillageFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))
      tkgrid(RpartialFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))      
      tkgrid(RlimFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))
      
              
      #mise en page de plotWin
      subOKCancelHelp(PlotWin, "plot.MFA")
      tkgrid(PlotGrpeFrame)
      tkgrid(PlotAxeFrame)
      if(length(listQuantiAct.nom)+length(listQuantiIllu.nom)>=1) {
        tkgrid(PlotVarFrame)
        Wchoix = TRUE
      }
      tkgrid(PlotIndFrame, PlotWin2, sticky="ns")
      tkgrid(subButtonsFrame, sticky="ew", columnspan=2)
    }

    PlotFrame<-tkframe(IlluFrame)
    Plot.but<-tkbutton(PlotFrame, textvariable=.PlotLabel, command=OnPlot, borderwidth=3)
    tkgrid(Plot.but, sticky="ew")
  }) 


  #! fonction HCPC
  
  Hcpc.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    .HcpcLabel<-tclVar(paste(firstLabel, "", sep=" "))    
    compteur.hcpc<-0
    Rclassif<-0
    Rmeth <- -1
    Rconsolid<-0 
    Rgraphhcpc<-1  
    Rreshcpc<-0
    Rminhcpc<-3
    Rmaxhcpc<-10

    OnHCPC <- function()
    {

      HcpcWin<-tktoplevel()
      tkwm.title(HcpcWin, gettextRcmdr("HCPC options"))

      onOKHcpc <- function()
      {
        assign("compteur.hcpc", compteur.hcpc+1, envir=env) 
        if(compteur.hcpc>0) tclvalue(.HcpcLabel)<-paste(label, "", sep=" ")
        tkconfigure(Hcpc.but, fg="blue")
      
        if(tclvalue(methValue)=="interactive") assign("Rmeth", 0, envir=env)
        else assign("Rmeth", -1, envir=env)
        
        if(tclvalue(consolidValue)=="1") assign("Rconsolid",TRUE, envir=env)
        else assign("Rconsolid",FALSE,envir=env)
        
        if(tclvalue(graphhcpcValue)=="1") assign("Rgraphhcpc",TRUE,envir=env)
        else assign("Rgraphhcpc",FALSE,envir=env)
        
        if(tclvalue(reshcpcValue)=="1") assign("Rreshcpc",TRUE,envir=env)
        else assign("Rreshcpc",FALSE,envir=env)        
        
        assign("Rminhcpc",as.numeric(tclvalue(minhcpc)),envir=env)
        assign("Rmaxhcpc",as.numeric(tclvalue(maxhcpc)),envir=env)
        
        assign("Rclassif",TRUE,envir=env)
        tkdestroy(HcpcWin)
      }
      OKHcpc.but<-tkbutton(HcpcWin, text="OK", width=8,command=onOKHcpc)

      onCancelHcpc <- function()
      {
        assign("Rclassif",FALSE,envir=env)
        tkdestroy(HcpcWin)
      }
      CancelHcpc.but<-tkbutton(HcpcWin, text="Cancel", width=8,command=onCancelHcpc)
 
      tkgrid(tklabel(HcpcWin, text=""))
      tkgrid(tklabel(HcpcWin, text = gettextRcmdr("Hierarchical Clustering on Principal Components"), fg = "darkred"), column=1, columnspan = 8, sticky = "ew")      

      meth1 <- tkradiobutton (HcpcWin)
      meth1.lab <- tklabel(HcpcWin,text=gettextRcmdr("interactive"))
      meth2 <- tkradiobutton (HcpcWin)
      meth2.lab <- tklabel(HcpcWin,text=gettextRcmdr("automatic"))
      methValue <- tclVar("interactive")
      meth.lab <- tklabel(HcpcWin,text=gettextRcmdr("Choice of the number of clusters: "))
      tkconfigure(meth1,variable=methValue,value="interactive")
      tkconfigure(meth2,variable=methValue,value="automatic")

      minmaxhcpc.label<-tklabel(HcpcWin,text=gettextRcmdr("The optimal number of clusters is chosen between:"))

      minhcpc<-tclVar("3")
      maxhcpc<-tclVar("10")
      minhcpc.entry <-tkentry(HcpcWin,width="3",textvariable=minhcpc)
      maxhcpc.entry <-tkentry(HcpcWin,width="3",textvariable=maxhcpc)

      consolid.lab <- tklabel(HcpcWin,text=gettextRcmdr("Consolidate clusters "))
      consolid.check <- tkcheckbutton(HcpcWin)
      if(Rconsolid) consolidValue<-tclVar("1")
      else consolidValue<-tclVar("0")      
      tkconfigure(consolid.check,variable=consolidValue)
      
      graphhcpc.lab <- tklabel(HcpcWin,text=gettextRcmdr("Print graphs "))
      graphhcpc.check <- tkcheckbutton(HcpcWin)
      if(Rgraphhcpc) graphhcpcValue <- tclVar("1")
      else graphhcpcValue <- tclVar("0")
      tkconfigure(graphhcpc.check,variable=graphhcpcValue)   

      reshcpc.lab <- tklabel(HcpcWin,text=gettextRcmdr("Print results for clusters "))
      reshcpc.check <- tkcheckbutton(HcpcWin)
      if(Rreshcpc) reshcpcValue<-tclVar("1")
      else reshcpcValue <- tclVar("0")
      tkconfigure(reshcpc.check,variable=reshcpcValue)          
    
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr("Select options for the HCPC"), fg = "blue"), column=1, columnspan=8, sticky="we")
      tkgrid(tklabel(HcpcWin,text=""))
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr(paste('Clustering is performed on the first ', tclvalue(ncp.val), ' dimensions of the MFA',sep=""))),column=1,columnspan=4,sticky="w")
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr("(Change your choice in the main options to change this number)")),column=1,columnspan=4,sticky="w")
      tkgrid(tklabel(HcpcWin,text=""))  
      tkgrid(meth.lab,meth1.lab,meth1)
      tkgrid(meth2.lab,meth2)
      tkgrid(tklabel(HcpcWin,text=""))
      tkgrid(minmaxhcpc.label,minhcpc.entry , maxhcpc.entry)
      tkgrid(tklabel(HcpcWin,text=""))      
      tkgrid(consolid.lab,consolid.check)
      tkgrid(graphhcpc.lab,graphhcpc.check)
      tkgrid(reshcpc.lab,reshcpc.check) 
      tkgrid(tklabel(HcpcWin,text=""))     
      tkgrid(OKHcpc.but, CancelHcpc.but)
      tkgrid(tklabel(HcpcWin, text=""))
      
      tkgrid.configure(minmaxhcpc.label,meth.lab,consolid.lab,graphhcpc.lab,reshcpc.lab,column=1,columnspan=4,sticky="w")
      tkgrid.configure(minhcpc.entry,column=7,columnspan=1,sticky="e")
      tkgrid.configure(maxhcpc.entry,column=8,columnspan=1,sticky="w")
      tkgrid.configure(meth1,meth2,consolid.check,graphhcpc.check,reshcpc.check,column=8,sticky="e")
      tkgrid.configure(meth1.lab,column=6,columnspan=2,sticky="w")
      tkgrid.configure(meth2.lab,column=6,columnspan=2,sticky="w") 
      tkgrid.configure(OKHcpc.but,column=2,columnspan=1,sticky="w")
      tkgrid.configure(CancelHcpc.but,column=6,columnspan=1,sticky="e")

      tkgrid.columnconfigure(HcpcWin,0, minsize=3)
      tkgrid.columnconfigure(HcpcWin,5, minsize=5)
      tkgrid.columnconfigure(HcpcWin,8, minsize=3) 

}      
    Hcpc2Frame<-tkframe(HcpcFrame)
    Hcpc.but<-tkbutton(Hcpc2Frame, textvariable=.HcpcLabel, command=OnHCPC, borderwidth=3)
    tkgrid(Hcpc.but, sticky="ew")
})   


    #! fonction associée au bouton Appliquer, execute sans détruire l'interface graphique
  OnAppliquer<-function()
  {
      #liste de l'ensemble des variables créées
      #sur la fenêtre top
#      listQuantiAct
#      listQuantiIllu
#      listQualiAct
#      listQualiIllu
#      resu.val
#      ncp.val
      #pour les individus illustratifs
#      individuillu
      #pour l'affichage
#      Rpropre
#          Rgroupe
#          Rinertie
#          Rindividu
#        Rindsup
#      Rquantisummary
#      Rquanti
#      Rquantisup  
#          Rqualisummary
#          Rquali
#          Rqualisup
#      Raxepartiel      
#          Rdescdim
      # pour les graphiques
#      Gchoix
#      GTitle
#      GAxeGrpe
#      Glabel
#        
#      Rchoix
#      RTitle
#      Rlabel.indMoy
#      Rlabel.indPar
#      Rlabel.quali  
#      Rhabillage
#      Rinvisible
#      Rpartial
#      RpartialSouris
#      Rchrono
#      RXlimInd
#      RYlimInd
#    
#      Wchoix
#      WTitle
#      WAxeVar
#      Wlabel.var
#      Whabillage
#      Winvisible
#      Wlim.cos
#    
#      Achoix
#      ATitle
#      AAxeAxe
#      Ahabillage
#
#      Axe 
  
    # récupération des paramètres de la fenêtre principale
    nom.res<-tclvalue(resu.val)
    if (length(ls(pat=nom.res))>0) justDoIt(paste('remove (',nom.res,')'))
    ncp<-as.numeric(tclvalue(ncp.val))
    Axe<-c(as.numeric(tclvalue(Axe1)), as.numeric(tclvalue(Axe2)))
    
    # gestion du tableau de données pour l'AFM
    group<-NULL
    type<-NULL
    name.group<-NULL
    num.group.sup<-NULL
    variables<-NULL
    indice.grpe<-1
      #récupération des groupes quanti actif
    nb.GQA<-length(listQuantiAct.nom)
    if(nb.GQA>=1) {
      name.group<-c(name.group, listQuantiAct.nom)
      for(i in 1:nb.GQA) {
        eval(parse(text=paste("liste.var.GQA<-", listQuantiAct.nom[i], ".var", sep="")))
        group<-c(group, length(liste.var.GQA)-1)
        type<-c(type,liste.var.GQA[1])
        variables<-c(variables, liste.var.GQA[-1])
        indice.grpe<-indice.grpe+1
      }
    }
      #récupération des groupes quanti illustratif
    nb.GQI<-length(listQuantiIllu.nom)
    if(nb.GQI>=1) {
      name.group<-c(name.group, listQuantiIllu.nom)
      for(i in 1:nb.GQI) {
         eval(parse(text=paste("liste.var.GQI<-", listQuantiIllu.nom[i], ".var", sep="")))
        group<-c(group, length(liste.var.GQI)-1)
        type<-c(type,liste.var.GQI[1])
        variables<-c(variables, liste.var.GQI[-1])
        num.group.sup<-c(num.group.sup,indice.grpe)
        indice.grpe<-indice.grpe+1
      }
    }
      #récupération des groupes quali actif
    nb.GQlA<-length(listQualiAct.nom)
    if(nb.GQlA>=1) {
      name.group<-c(name.group, listQualiAct.nom)
      for(i in 1:nb.GQlA) {
        eval(parse(text=paste("liste.var.GQlA<-", listQualiAct.nom[i], ".var", sep="")))
        group<-c(group, length(liste.var.GQlA)-1)
        type<-c(type,liste.var.GQlA[1])
        variables<-c(variables, liste.var.GQlA[-1])
        indice.grpe<-indice.grpe+1
      }
    }
      #récupération des groupes quali illustratif
    nb.GQlI<-length(listQualiIllu.nom)
    if(nb.GQlI>=1) {
      name.group<-c(name.group, listQualiIllu.nom)
      for(i in 1:nb.GQlI) {
        eval(parse(text=paste("liste.var.GQlI<-", listQualiIllu.nom[i], ".var", sep="")))
        group<-c(group, length(liste.var.GQlI)-1)
        type<-c(type,liste.var.GQlI[1])
        variables<-c(variables, liste.var.GQlI[-1])
        num.group.sup<-c(num.group.sup,indice.grpe)
        indice.grpe<-indice.grpe+1
      }
    }
    
      #construction du tableau de données.MFA
      if(!is.null(individuillu)) {
        ind.actif<-rows[-which(rows %in% individuillu)]
        commande.data<-paste(activeDataSet(),'.MFA', '<-', activeDataSet(),'[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '"), c("',paste(variables, collapse='", "'), '")]', sep="")
      }
      else commande.data<-paste(activeDataSet(),'.MFA', '<-', activeDataSet(),'[, c("',paste(variables, collapse='", "'), '")]', sep="")
      
      justDoIt(commande.data)
      logger(commande.data)
      donnee.depart<-activeDataSet()
      activeDataSet(paste(activeDataSet(),'.MFA', sep=""))

      # gestion de la commande réalisant l'AFM     
      if(!is.null(individuillu)) {
        ind.actif<-rows[-which(rows %in% individuillu)]
        commande.MFA<-paste(nom.res, '<-MFA(', activeDataSet(), ', group=c(',paste(group, collapse=", "), '), type=c("', paste(type, collapse='", "'),'"), ind.sup=', nrow(get(.activeDataSet))-length(individuillu)+1, ': ', nrow(get(.activeDataSet)), ', ncp=', ncp, ', name.group=c("',paste(name.group, collapse='", "'), '"), num.group.sup=c(',paste(num.group.sup, collapse=", "), '), graph=FALSE)',sep="")
      }
      else commande.MFA<-paste(nom.res, '<-MFA(', activeDataSet(), ', group=c(',paste(group, collapse=", "), '), type=c("', paste(type, collapse='", "'),'"), ncp=', ncp, ', name.group=c("',paste(name.group, collapse='", "'), '"), num.group.sup=c(',paste(num.group.sup, collapse=", "), '), graph=FALSE)',sep="")
      justDoIt(commande.MFA)
      logger(commande.MFA)


    #Commande de la fonction HCPC

    if(Rclassif==TRUE){
      commande.hcpc<-paste(nom.res,'.hcpc', '<-HCPC(', nom.res, ' ,nb.clust=', Rmeth, ',consol=', Rconsolid,',min=', Rminhcpc,',max=',Rmaxhcpc,',graph=', Rgraphhcpc, ')', sep="")
    justDoIt(commande.hcpc)
    logger(commande.hcpc)      
      if(Rreshcpc==TRUE){
        doItAndPrint(paste(nom.res,'.hcpc$data.clust[,ncol(res.hcpc$data.clust),drop=F]', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.var', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.axes', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.ind', sep=""))
      }        
    }


      #gestion des graphiques   
      if (length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0) {if (get(nom.res)$eig[1,2]==100) doItAndPrint(paste('"No graph can be plot: data are unidimensional"'))}
      if((Gchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)){
      if (get(nom.res)$eig[1,2]!=100) {
        commande.plotG<-paste('plot.MFA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="group", lab.grpe=', Glabel, sep="")
        if (is.null(GTitle)) commande.plotG <- paste(commande.plotG,')', sep="")
        else {
          if (GTitle ==" ") commande.plotG <- paste(commande.plotG,')', sep="")
          else commande.plotG <- paste(commande.plotG,', title="', GTitle,'")', sep="")
        }
        justDoIt(commande.plotG)
        logger(commande.plotG)
      }}
      
      if((Achoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)){
      if (get(nom.res)$eig[1,2]!=100) {
        commande.plotA<-paste('plot.MFA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="axes", habillage="', Ahabillage, '"', sep="")
        if (is.null(ATitle)) commande.plotA <- paste(commande.plotA,')', sep="")
        else {
          if (ATitle ==" ") commande.plotA <- paste(commande.plotA,')', sep="")
          else commande.plotA <- paste(commande.plotA,', title="', ATitle,'")', sep="")
        }
        justDoIt(commande.plotA)
        logger(commande.plotA) 
      }}
      
      if((Wchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)&(nb.GQI+nb.GQA>0)){
      if (get(nom.res)$eig[1,2]!=100) {
        commande.plotW<-paste('plot.MFA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="var", lab.var=', Wlabel.var, ', habillage="', Whabillage, '", lim.cos2.var=', Wlim.cos, sep="")
        if (!is.null(Winvisible)) commande.plotW<-paste(commande.plotW, ', invisible=c("', paste(Winvisible, collapse='", "'),'")', sep='')
        if (is.null(WTitle)) commande.plotW <- paste(commande.plotW,')', sep="")
        else {
          if (WTitle ==" ") commande.plotW <- paste(commande.plotW,')', sep="")
          else commande.plotW <- paste(commande.plotW,', title="', WTitle,'")', sep="")
        }
        justDoIt(commande.plotW)
        logger(commande.plotW)
      }}
      
      if((Rchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)){
      if (get(nom.res)$eig[1,2]!=100) {
        if ((Rhabillage!="none") & (Rhabillage!="ind") & (Rhabillage!="group")) {
          Rhabillage<-which(colnames(get(.activeDataSet))==Rhabillage)
          if(length(Rhabillage)==0) Rhabillage<-"none"
        }
        if (Rhabillage=="none") Rhabillage<-paste('"', Rhabillage, '"', sep="")
        if (Rhabillage=="ind") Rhabillage<-paste('"', Rhabillage, '"', sep="")
        if (Rhabillage=="group") Rhabillage<-paste('"', Rhabillage, '"', sep="")
        
        if(RpartialSouris){
          commande.plotI<-paste('plot.MFApartial(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), lab.ind.moy=', Rlabel.indMoy, ', lab.par=', Rlabel.indPar, ', lab.var=', Rlabel.quali,', habillage=', Rhabillage, sep="")
          if (!is.null(RXlimInd)) commande.plotI<-paste(commande.plotI, ', xlim=c(', paste(RXlimInd, collapse=", "), ')', sep='')
          if (!is.null(RYlimInd)) commande.plotI<-paste(commande.plotI, ', ylim=c(', paste(RYlimInd, collapse=", "), ')', sep='')
          if (!is.null(Rinvisible)) commande.plotI<-paste(commande.plotI, ', invisible=c("', paste(Rinvisible, collapse='", "'),'")', sep='')
          if (Rchrono) commande.plotI<-paste(commande.plotI, ', chrono=', Rchrono, sep='')
          if (is.null(RTitle)) commande.plotI <- paste(commande.plotI,')', sep='')
          else {
            if (RTitle ==" ") commande.plotI <- paste(commande.plotI,')', sep="")
            else commande.plotI <- paste(commande.plotI,', title="', RTitle,'")', sep="")
          }
        }
        else {
          commande.plotI<-paste('plot.MFA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="ind", lab.ind.moy=', Rlabel.indMoy, ', lab.par=', Rlabel.indPar, ', lab.var=', Rlabel.quali,', habillage=', Rhabillage, sep="") 
          if (!is.null(RXlimInd)) commande.plotI<-paste(commande.plotI, ', xlim=c(', paste(RXlimInd, collapse=", "), ')', sep='')
          if (!is.null(RYlimInd)) commande.plotI<-paste(commande.plotI, ', ylim=c(', paste(RYlimInd, collapse=", "), ')', sep='')
          if (!is.null(Rinvisible)) commande.plotI<-paste(commande.plotI, ', invisible=c("', paste(Rinvisible, collapse='", "'),'")', sep='')
          if (!is.null(Rpartial)) commande.plotI<-paste(commande.plotI, ', partial=c("', paste(Rpartial, collapse='", "'),'")', sep='')
          if (Rchrono) commande.plotI<-paste(commande.plotI, ', chrono=', Rchrono, sep='')
          if (is.null(RTitle)) commande.plotI <- paste(commande.plotI,')', sep="")
          else {
            if (RTitle ==" ") commande.plotI <- paste(commande.plotI,')', sep="")
            else commande.plotI <- paste(commande.plotI,', title="', RTitle,'")', sep="")
          }
        }
        justDoIt(commande.plotI)
        logger(commande.plotI)
      }}
      
      # gestion de l'édition de certains resultats
    if (RFichier==""){
      if(Rpropre) doItAndPrint(paste( nom.res, '$eig', sep=""))
      if(Rgroupe) doItAndPrint(paste( nom.res, '$group', sep=""))
      if(Rinertie) doItAndPrint(paste( nom.res, '$inertia.ratio', sep=""))
      if(Rindividu) doItAndPrint(paste( nom.res, '$ind', sep=""))
      if(Rindsup) doItAndPrint(paste( nom.res, '$ind.sup', sep=""))
      if(Rquantisummary) doItAndPrint(paste( nom.res, '$summary.quanti', sep=""))
      if(Rquanti) doItAndPrint(paste( nom.res, '$quanti.var', sep=""))      
      if(Rquantisup) doItAndPrint(paste( nom.res, '$quanti.var.sup', sep=""))
      if(Rqualisummary) doItAndPrint(paste( nom.res, '$summary.quali', sep=""))
      if(Rquali) doItAndPrint(paste( nom.res, '$quali.var', sep=""))      
      if(Rqualisup) doItAndPrint(paste( nom.res, '$quali.var.sup', sep=""))
      if(Raxepartiel) doItAndPrint(paste( nom.res, '$partial.axes', sep=""))
      if(Rdescdim) doItAndPrint(paste('dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '))', sep=""))
    }
    else {
      Fich = RFichier
      if (substr(Fich,1,1)!='"') Fich = paste('"',Fich,sep='')
      if (substr(Fich,nchar(Fich),nchar(Fich))!='"') Fich = paste(Fich,'"',sep='')
      append = FALSE
      if(Rpropre){
        doItAndPrint(paste('write.infile(', nom.res, '$eig, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rgroupe){
        doItAndPrint(paste('write.infile(', nom.res, '$group, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rinertie){
        doItAndPrint(paste('write.infile(', nom.res, '$inertia.ratio, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rindividu){
        doItAndPrint(paste('write.infile(', nom.res, '$ind, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rindsup){
        doItAndPrint(paste('write.infile(', nom.res, '$ind.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rquantisummary){
        doItAndPrint(paste('write.infile(', nom.res, '$summary.quanti, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rquanti){
        doItAndPrint(paste('write.infile(', nom.res, '$quanti.var, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rquantisup){
        doItAndPrint(paste('write.infile(', nom.res, '$quanti.var.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rqualisummary){
        doItAndPrint(paste('write.infile(', nom.res, '$summary.quali, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rquali){
        doItAndPrint(paste('write.infile(', nom.res, '$quali.var, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rqualisup){
        doItAndPrint(paste('write.infile(', nom.res, '$quali.var.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Raxepartiel){
        doItAndPrint(paste('write.infile(', nom.res, '$partial.axes, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rdescdim) doItAndPrint(paste('write.infile(dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), ')), file =',Fich,',append=',append,')', sep=""))
    }

      # Re-chargement du tableau de départ et supression du tableau temporaire
      activeDataSet(donnee.depart)
      justDoIt(paste('remove(',activeDataSet(),'.MFA)',sep=""))
      logger(paste('remove(',activeDataSet(),'.MFA)',sep=""))   
  }


    #! fonction associée au bouton OK, execute et détruit l'interface graphique
  onOK<-function()
  {
    OnAppliquer()
    tkdestroy(top)     
  }



#                   Création de la fenêtre top                                 #
################################################################################
  top<-tktoplevel(borderwidth=10)
  tkwm.title(top,gettextRcmdr("MFA"))
  tkwm.geometry(top, "-50+50")
  
  # définition des polices
  font2<-tkfont.create(family="times",size=12,weight="bold")
  fontheading<-tkfont.create(family="times",size=11,weight="bold")

  # récupération du jeu de données actif
  donnee<-get(.activeDataSet)
  vars<-colnames(donnee)
  rows<-rownames(donnee)
  
  # création du frame contenant les listes groupes quanti
  ListeQuantiFrame<- tkframe(top, borderwidth=2, relief="groove")
  label.quantiFrame.var<-tclVar(gettextRcmdr("Quantitative groups"))
  label.quantiFrame<-tklabel(ListeQuantiFrame, textvariable=label.quantiFrame.var,fg = "darkred", font=fontheading)
    # liste des groupes de variables quanti Actives
  listQuantiAct<-tklistbox(ListeQuantiFrame,selectmode="extended",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(scrQuantiAct,...))
  scrQuantiAct<-tkscrollbar(ListeQuantiFrame,repeatinterval=5,command=function(...)tkyview(listQuantiAct,...))
  listQuantiAct.nom<-NULL
  
    # liste des groupes de variables quanti Actives
  listQuantiIllu<-tklistbox(ListeQuantiFrame,selectmode="extended",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(scrQuantiIllu,...))
  scrQuantiIllu<-tkscrollbar(ListeQuantiFrame,repeatinterval=5,command=function(...)tkyview(listQuantiIllu,...))
  listQuantiIllu.nom<-NULL
  
    # boutons d'action groupes quantitative
  supprimeQuanti.funct(label="Delete")  
  ajoutQuanti.funct(label=gettextRcmdr("Add quanti. group"), firstLabel=gettextRcmdr("Add quanti group"))
  modifQuanti.funct(label=gettextRcmdr("Modify 1 group"), firstLabel=gettextRcmdr("Modify 1 group"))
    # mise en forme de ListeQuantiFrame
  #tkgrid(tklabel(ListeQuantiFrame, text = "Quantitative groups",fg = "darkred"), columnspan=11, sticky = "ew")
  tkgrid(label.quantiFrame, columnspan=11, sticky = "ew")
  tkgrid(listQuantiAct, scrQuantiAct, listQuantiIllu, scrQuantiIllu)
  tkgrid.configure(scrQuantiAct, column=3, sticky="wns")
  tkgrid.configure(scrQuantiIllu, column=9, sticky="wns")  
  tkgrid.configure(listQuantiAct, sticky = "ew", column=1, columnspan=2)
  tkgrid.configure(listQuantiIllu, sticky = "ew", column=7, columnspan=2)
  tkgrid.configure(tklabel(ListeQuantiFrame, text=" "))
  tkgrid.configure(GpeQuantiFrame,ModifGpeQuantiFrame, SupGpeQuantiFrame)
  tkgrid.configure(GpeQuantiFrame, sticky = "ew", column=1, columnspan=2)
  tkgrid.configure(ModifGpeQuantiFrame, sticky = "ew", column=4, columnspan=2)
  tkgrid.configure(SupGpeQuantiFrame, sticky = "ew", column=7, columnspan=2)
  tkgrid.columnconfigure(ListeQuantiFrame,0, minsize=25)
  tkgrid.columnconfigure(ListeQuantiFrame,10, minsize=25)
  tkgrid.columnconfigure(ListeQuantiFrame,3, minsize=25)
  tkgrid.columnconfigure(ListeQuantiFrame,9, minsize=25)
  tkgrid.columnconfigure(ListeQuantiFrame,4, minsize=35)
  tkgrid.columnconfigure(ListeQuantiFrame,5, minsize=35)
  
 # création du frame contenant les listes groupes quali
  ListeQualiFrame<- tkframe(top, borderwidth=2, relief="groove")
  label.qualiFrame.var<-tclVar(gettextRcmdr("Qualitative groups"))
  label.qualiFrame<-tklabel(ListeQualiFrame, textvariable=label.qualiFrame.var,fg = "darkred", font=fontheading)
    # liste des groupes de variables quali Actives
  listQualiAct<-tklistbox(ListeQualiFrame,selectmode="extended",exportselection="TRUE", height=4, yscrollcommand=function(...)tkset(scrQualiAct,...))
  scrQualiAct<-tkscrollbar(ListeQualiFrame,repeatinterval=5,command=function(...)tkyview(listQualiAct,...))
  listQualiAct.nom<-NULL
  
  # liste des groupes de variables quali Actives
  listQualiIllu<-tklistbox(ListeQualiFrame,selectmode="extended",exportselection="TRUE", height=4, yscrollcommand=function(...)tkset(scrQualiIllu,...))
  scrQualiIllu<-tkscrollbar(ListeQualiFrame,repeatinterval=5,command=function(...)tkyview(listQualiIllu,...))
  listQualiIllu.nom<-NULL

      # boutons d'action groupes qualitatif
  supprimeQuali.funct(label="Delete")  
  ajoutQuali.funct(label=gettextRcmdr("Add quali. group"), firstLabel=gettextRcmdr("Add quali group")) 
  modifQuali.funct(label=gettextRcmdr("Modify 1 group"), firstLabel=gettextRcmdr("Modify 1 group"))
  
    # mise en forme de ListeQualiFrame
  #tkgrid(tklabel(ListeQualiFrame, text = "Qualitative groups",fg = "darkred"), columnspan=11, sticky = "ew")
  tkgrid(label.qualiFrame, columnspan=11, sticky = "ew")
  tkgrid(listQualiAct, scrQualiAct, listQualiIllu, scrQualiIllu)
  tkgrid.configure(scrQualiAct, column=3, columnspan=1, sticky="wns")
  tkgrid.configure(scrQualiIllu, column=9, columnspan=1, sticky="wns")  
  tkgrid.configure(listQualiAct, sticky = "ew", column=1, columnspan=2)
  tkgrid.configure(listQualiIllu, sticky = "ew", column=7, columnspan=2)
  tkgrid.configure(tklabel(ListeQualiFrame, text=" "))
  tkgrid.configure(GpeQualiFrame, ModifGpeQualiFrame, SupGpeQualiFrame)
  tkgrid.configure(GpeQualiFrame, sticky = "ew", column=1, columnspan=2)
  tkgrid.configure(ModifGpeQualiFrame, sticky = "ew", column=4, columnspan=2)  
  tkgrid.configure(SupGpeQualiFrame, sticky = "ew", column=7, columnspan=2)
  tkgrid.columnconfigure(ListeQualiFrame,0, minsize=25)
  tkgrid.columnconfigure(ListeQualiFrame,10, minsize=25)
  tkgrid.columnconfigure(ListeQualiFrame,3, minsize=25)
  tkgrid.columnconfigure(ListeQualiFrame,9, minsize=25)
  tkgrid.columnconfigure(ListeQualiFrame,4, minsize=35)
  tkgrid.columnconfigure(ListeQualiFrame,5, minsize=35)


   # création de tous les boutons d'options dans IlluFrame
  IlluFrame<- tkframe(top, borderwidth=2)
  Reinitializ.but<-tkbutton(IlluFrame, text="Restart",width=18,command=Reinitializ.funct, borderwidth=3)
    # mise en page de IlluFrame
  Iillu.funct(label=gettextRcmdr("Modify supplementary individuals"), firstLabel=gettextRcmdr("Select supplementary individuals"))    
  PLOT.MFA(label=gettextRcmdr("Graphical options"), firstLabel=gettextRcmdr("Graphical options"))
  Sortie.funct(label=gettextRcmdr("Outputs"), firstLabel=gettextRcmdr("Outputs"))
  tkgrid(IilluFrame, PlotFrame, columnspan=5)
  tkgrid(tklabel(IlluFrame, text=""))
  tkgrid(SortieFrame, Reinitializ.but, columnspan=7)
  tkgrid.configure(PlotFrame, column=3, columnspan=1)
  tkgrid.configure(SortieFrame, column=1, columnspan=1)
  tkgrid.configure(IilluFrame, column=1, columnspan=1)
  tkgrid.configure(Reinitializ.but, column=3, columnspan=1)      
  tkgrid.columnconfigure(IlluFrame,0, minsize=25)
  tkgrid.columnconfigure(IlluFrame,2, minsize=40)  
  tkgrid.columnconfigure(IlluFrame,4, minsize=25)  


    # création des options dans OptionFrame  
  OptionFrame<-tkframe(top, borderwidth=2, relief="groove")
  resu.lab<-tklabel(OptionFrame,text=gettextRcmdr("Name of the result object: "))
  resu.val<-tclVar("res")
  resu<-tkentry(OptionFrame,width=10,textvariable=resu.val)
  ncp.lab<-tklabel(OptionFrame,text=gettextRcmdr("Number of dimensions: "))
  ncp.val<-tclVar("5") 
  ncp<-tkentry(OptionFrame,width=5,textvariable=ncp.val)
  Axe.label<-tklabel(OptionFrame,text=gettextRcmdr("Select the dimensions for the graphs:"))
  Axe1<-tclVar("1")
  Axe2<-tclVar("2")
  Axe1.entry <-tkentry(OptionFrame,width="5",textvariable=Axe1)
  Axe2.entry <-tkentry(OptionFrame,width="5",textvariable=Axe2)
  
    # mise en page de OptionFrame
  tkgrid(tklabel(OptionFrame,text=gettextRcmdr("Main options"), fg = "darkred"), columnspan=8, sticky="we") 
  tkgrid(tklabel(OptionFrame,text="")) 
  tkgrid(resu.lab, resu)
  tkgrid(ncp.lab, ncp)
  tkgrid(Axe.label,Axe1.entry , Axe2.entry, sticky="w")
  tkgrid.configure(ncp.lab, resu.lab, Axe.label, column=1, columnspan=4, sticky="w")
  tkgrid.configure(ncp, resu, column=6, columnspan=2, sticky="e")
  tkgrid.configure(Axe1.entry, column=6, columnspan=1, sticky="w")
  tkgrid.configure(Axe2.entry, column=7, columnspan=1, sticky="e")
  tkgrid.columnconfigure(OptionFrame,0, minsize=25)
  tkgrid.columnconfigure(OptionFrame,5, minsize=40)
  tkgrid.columnconfigure(OptionFrame,8, minsize=25)

  #Frame pour HCPC
  HcpcFrame<-tkframe(top, borderwidth=2)
  Hcpc.funct(label=gettextRcmdr("Perform Clustering after MFA"), firstLabel=gettextRcmdr("Perform Clustering after MFA")) 
  tkgrid(Hcpc2Frame, columnspan=7)
  tkgrid.configure(Hcpc2Frame,column=4, columnspan=1)


  appliquer.but<-tkbutton(top, text="Apply",width=12,command=OnAppliquer, borderwidth=3, fg="#690f96")
  OKCancelHelp(helpSubject="MFA")


  # Mise en page de top
  tkgrid(tklabel(top, text=gettextRcmdr("Multiple Factor Analysis (MFA)"),font=fontheading), columnspan=3)
  tkgrid(tklabel(top,text=""))
  if (length(listNumeric())>0){
    tkgrid(ListeQuantiFrame, column=1, columnspan=1, sticky="ew")
    tkgrid(tklabel(top,text=""))    
  }
  if (length(listFactors())>0) {
    tkgrid(ListeQualiFrame, column=1, columnspan=1, sticky="ew")
    tkgrid(tklabel(top,text=""))    
  }
  tkgrid(IlluFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text=""))    
  tkgrid(OptionFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc  
  tkgrid(HcpcFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc        
  tkgrid(appliquer.but, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc  
  tkgrid(buttonsFrame, column=1, columnspan=1, sticky="ew" )
  
}  

#############################FIN FONCTION FactoMFA ##############################


################################################################################
#                              FONCTION FactoGPA                               #
################################################################################

FactoGPA<-function()
{
  require(tcltk)
  require(FactoMineR)

# Fonction pour la gestion des noms ############################################

  nom.correct<-function(text, liste=NULL)
  {
    text<-chartr("^\ ", "...", text)
    if(!is.null(liste)) {
      while(text %in% liste) {
        text<-paste(text, ".bis", sep="")
      }
    }
    return(text)  
  }
################################################################################

#    Création des fonctions pour les options via nouvelle fenêtre graphique   


  #! suppression de groupes quantitatif
  supprimeQuanti.funct<-defmacro(label, expr=
  {
    env<-environment()
    OnSGQ<-function()
    {
      grpeActSupprime<-listQuantiAct.nom[as.numeric(tkcurselection(listQuantiAct))+1]
      if(length(grpeActSupprime)>=1)
      { 
        listQuantiAct.nom.tmp<-listQuantiAct.nom[-which(listQuantiAct.nom %in% grpeActSupprime)]
        assign("listQuantiAct.nom",listQuantiAct.nom.tmp, envir=env)
        tkdelete(listQuantiAct,"0","end")
        if(length(listQuantiAct.nom)>=1) {
          for (grpe in listQuantiAct.nom) tkinsert(listQuantiAct, "end", grpe)
        }  
      }
      nb.grpe<-length(listQuantiAct.nom) 
      if (nb.grpe>1) {
        tclvalue(label.quantiFrame.var)<-paste(nb.grpe, gettextRcmdr("groups"), sep=" ")
        tkconfigure(label.quantiFrame)
      }  
      else
      {
        tclvalue(label.quantiFrame.var)<-paste("0", gettextRcmdr("group"), sep=" ")
        tkconfigure(label.quantiFrame)
      }  
      
    }
    
    SupGpeQuantiFrame<-tkframe(ListeQuantiFrame)
    SupGpeQuanti.but<-tkbutton(SupGpeQuantiFrame, textvariable=tclVar(label), command=OnSGQ, borderwidth=3)
   tkgrid(SupGpeQuanti.but, sticky="ew")  
  })


  #! Ajout d'un groupe quantitatif
  ajoutQuanti.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.GQ<-1
    .AjoutQuantiLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnAGQ<-function()
    {
      AjoutGpeQuantiWin<-tktoplevel()
      tkwm.title(AjoutGpeQuantiWin,gettextRcmdr("Definition of a group"))
      
      #création de la fonction AGA.OK
      AGQ.OK<-function()
      {
        assign("compteur.GQ", compteur.GQ+1, envir=env)
        nom.groupe<-nom.correct(tclvalue(nomGrpeQuanti.val), liste=c(listQuantiAct.nom))
        if (nom.groupe=="") tkmessageBox(message=gettextRcmdr("Name for the group"), icon="warning", type="ok") 
        else {
          varGroupe<-listVarQuanti.nom[as.numeric(tkcurselection(listVarQuanti))+1]
          if (length(varGroupe)>=1) {
            assign(paste(nom.groupe,".var", sep=""), c(varGroupe), envir=env)
            tkinsert(listQuantiAct,"end",nom.groupe)
            assign("listQuantiAct.nom", c(listQuantiAct.nom, nom.groupe),envir=env)
            if (length(listQuantiAct.nom)==1) tclvalue(label.quantiFrame.var)<-paste(gettextRcmdr("1 group"), sep=" ")
            else tclvalue(label.quantiFrame.var)<-paste(length(listQuantiAct.nom) , gettextRcmdr("groups"), sep=" ")
            tkconfigure(label.quantiFrame)  
            tkdestroy(AjoutGpeQuantiWin)
          }
        }  
      }
      
      
      # choix du nom du groupe
      nomGrpeQuanti.lab<-tklabel(AjoutGpeQuantiWin,text=gettextRcmdr("Name of the group: "))
      nomGrpeQuanti.val<-tclVar(paste("Gc", compteur.GQ, sep=""))
      nomGrpeQuanti<-tkentry(AjoutGpeQuantiWin,width=15,textvariable=nomGrpeQuanti.val)
                  
      # création de la liste pour le choix des variables acives
      listVarQuanti<-tklistbox(AjoutGpeQuantiWin, selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrVarQuanti,...))
      scrVarQuanti<-tkscrollbar(AjoutGpeQuantiWin,repeatinterval=5,command=function(...)tkyview(listVarQuanti,...))
      listVarQuanti.nom<-NULL
      for (i in (1:ncol(donnee))) {
        if (is.numeric(donnee[,i])) {
            tkinsert(listVarQuanti,"end",vars[i])
            listVarQuanti.nom<-c(listVarQuanti.nom, vars[i])
        }
      }
      
      AGQ.but<-tkbutton(AjoutGpeQuantiWin, text="OK", width=16, command=AGQ.OK)
      
      tkgrid(nomGrpeQuanti.lab, nomGrpeQuanti)
      tkgrid.configure(nomGrpeQuanti.lab, column=0, columnspan=2, sticky="w")
      tkgrid.configure(nomGrpeQuanti, column=2, columnspan=3)      
      tkgrid(tklabel(AjoutGpeQuantiWin, text=""))      
      tkgrid(tklabel(AjoutGpeQuantiWin, text = gettextRcmdr("Select the variables for the group"), fg = "blue"), column=0, columnspan = 5, sticky = "w")
      tkgrid(listVarQuanti, scrVarQuanti, sticky = "nw")
      tkgrid.configure(scrVarQuanti, sticky = "wns", column=4,columnspan=1)
      tkgrid.configure(listVarQuanti, sticky = "ew", column=0, columnspan=4)
      tkgrid(tklabel(AjoutGpeQuantiWin, text=""))
      tkgrid(AGQ.but, column=2,columnspan=1, sticky="ew")
      tkgrid.columnconfigure(AjoutGpeQuantiWin,0, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQuantiWin,1, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQuantiWin,2, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQuantiWin,3, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQuantiWin,4, minsize=55)             
   }
   GpeQuantiFrame<-tkframe(ListeQuantiFrame)
   AjoutGpeQuanti.but<-tkbutton(GpeQuantiFrame, textvariable=.AjoutQuantiLabel, command=OnAGQ, borderwidth=3)
   tkgrid(AjoutGpeQuanti.but, sticky="ew")
  })    
  


  #! Modification d'un groupe quantitatif
  modifQuanti.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    .ModifQuantiLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnMGQ<-function() {
      ModifGpeQuantiWin<-tktoplevel()
      tkwm.title(ModifGpeQuantiWin,gettextRcmdr("Modification of a group"))
      
      #création de la fonction AGA.OK
      MGQ.OK<-function() {
        nom.groupe<-nom.correct(tclvalue(nomModifGrpeQuanti.val), liste=c(listQuantiAct.nom))
        if (nom.groupe=="") tkmessageBox(message=gettextRcmdr("Name for the group"), icon="warning", type="ok") 
        else {
          listQuantiAct.nom.tmp<-listQuantiAct.nom[-which(listQuantiAct.nom== grpeAModifier)]
          assign("listQuantiAct.nom",listQuantiAct.nom.tmp, envir=env)
          tkdelete(listQuantiAct,"0","end")
          for (grpe in listQuantiAct.nom) tkinsert(listQuantiAct, "end", grpe)
          
          varGroupe<-listModifVarQuanti.nom[as.numeric(tkcurselection(listModifVarQuanti))+1]
          if (length(varGroupe)>=1) {
            assign(paste(nom.groupe,".var", sep=""), c(varGroupe), envir=env)
            tkinsert(listQuantiAct,"end",nom.groupe)
            assign("listQuantiAct.nom", c(listQuantiAct.nom, nom.groupe),envir=env)
            tkdestroy(ModifGpeQuantiWin)
          }
        }  
      }
      
      
      if(length(as.numeric(tkcurselection(listQuantiAct)))>=1)  grpeAModifier<-listQuantiAct.nom[as.numeric(tkcurselection(listQuantiAct))+1][1]
      else {
        tkdestroy(ModifGpeQuantiWin)
        return()
      }  
        
      eval(parse(text=paste("grpeAModifier.var<-",paste(grpeAModifier,".var", sep=""),sep="")))
      # choix du nom du groupe
      nomModifGrpeQuanti.lab<-tklabel(ModifGpeQuantiWin,text=gettextRcmdr("Name of the group: "))
      nomModifGrpeQuanti.val<-tclVar(grpeAModifier)
      nomModifGrpeQuanti<-tkentry(ModifGpeQuantiWin,width=15,textvariable=nomModifGrpeQuanti.val)
            
      # création de la liste pour le choix des variables acives
      listModifVarQuanti<-tklistbox(ModifGpeQuantiWin, selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrModifVarQuanti,...))
      scrModifVarQuanti<-tkscrollbar(ModifGpeQuantiWin,repeatinterval=5,command=function(...)tkyview(listModifVarQuanti,...))
      listModifVarQuanti.nom<-NULL
      indice.num<-0
      for (i in (1:ncol(donnee))) {
        if (is.numeric(donnee[,i])) {
            tkinsert(listModifVarQuanti,"end",vars[i])
            listModifVarQuanti.nom<-c(listModifVarQuanti.nom, vars[i])
            if(vars[i] %in% grpeAModifier.var) tkselection.set(listModifVarQuanti, indice.num)
            indice.num<-indice.num+1
        }
      }
      
      MGQ.but<-tkbutton(ModifGpeQuantiWin, text="OK", width=16, command=MGQ.OK)
      
      tkgrid(nomModifGrpeQuanti.lab, nomModifGrpeQuanti)
      tkgrid.configure(nomModifGrpeQuanti.lab, column=0, columnspan=2, sticky="w")
      tkgrid.configure(nomModifGrpeQuanti, column=2, columnspan=3)      
      tkgrid(tklabel(ModifGpeQuantiWin, text=""))      
      tkgrid(tklabel(ModifGpeQuantiWin, text = gettextRcmdr("Select the variables of the group"), fg = "blue"), column=0, columnspan = 5, sticky = "w")
      tkgrid(listModifVarQuanti, scrModifVarQuanti, sticky = "nw")
      tkgrid.configure(scrModifVarQuanti, sticky = "wns", column=4,columnspan=1)
      tkgrid.configure(listModifVarQuanti, sticky = "ew", column=0, columnspan=4)
      tkgrid(tklabel(ModifGpeQuantiWin, text=""))
      tkgrid(MGQ.but, column=2,columnspan=1, sticky="ew")
      tkgrid.columnconfigure(ModifGpeQuantiWin,0, minsize=55)
      tkgrid.columnconfigure(ModifGpeQuantiWin,1, minsize=55)
      tkgrid.columnconfigure(ModifGpeQuantiWin,2, minsize=55)
      tkgrid.columnconfigure(ModifGpeQuantiWin,3, minsize=55)
      tkgrid.columnconfigure(ModifGpeQuantiWin,4, minsize=55)             
   }
   ModifGpeQuantiFrame<-tkframe(ListeQuantiFrame)
   ModifGpeQuanti.but<-tkbutton(ModifGpeQuantiFrame, textvariable=.ModifQuantiLabel, command=OnMGQ, borderwidth=3)
   tkgrid(ModifGpeQuanti.but, sticky="ew")
  })    
  
  
    #! fonction pour la réinitialisation des paramètre
  Reinitializ.funct<-function()
  {
    tkdestroy(top)
    FactoGPA()
  }


  #! fonction pour le choix des éléments de sortie
  Sortie.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.sortie<-0
    #déclaration des variables
    RFichier <- ""
    Rdep<-FALSE
    RRVs<-FALSE
    Rsimi<-FALSE
    Rscaling<-FALSE
    Rconsensus<-TRUE    
    RPANOVA<-FALSE
    RXfin<-FALSE
    Rcorrelations<-FALSE          
    RRV<-TRUE

    .SortieLabel<-tclVar(paste(firstLabel, "", sep=" "))

    OnSortie<-function()
    {
      SortieWin<-tktoplevel()
      tkwm.title(SortieWin,gettextRcmdr("Output options"))

      #création de la fonction onOKsub
      onOK.sortie<-function()
      {
        assign("compteur.sortie", compteur.sortie+1, envir=env)
        if(compteur.sortie>0) tclvalue(.SortieLabel)<-paste(label, "", sep=" ")
        tkconfigure(Sortie.but, fg="blue")
        
        if(tclvalue(depValue)=="1") assign("Rdep", TRUE, envir=env)
        else assign("Rdep", FALSE, envir=env)
        
        if(tclvalue(RVsValue)=="1") assign("RRVs", TRUE, envir=env)
        else assign("RRVs", FALSE, envir=env)
        
        if(tclvalue(simi.Value)=="1") assign("Rsimi", TRUE, envir=env)
        else assign("Rsimi", FALSE, envir=env)
        
        if(tclvalue(scalingValue)=="1") assign("Rscaling", TRUE, envir=env)
        else assign("Rscaling", FALSE, envir=env)
        
        if(tclvalue(consensusValue)=="1") assign("Rconsensus", TRUE, envir=env)
        else assign("Rconsensus", FALSE, envir=env)
        
        if(tclvalue(PANOVAValue)=="1") assign("RPANOVA", TRUE, envir=env)
        else assign("RPANOVA", FALSE, envir=env)
        
        if(tclvalue(XfinValue)=="1") assign("RXfin", TRUE, envir=env)
        else assign("RXfin", FALSE, envir=env)
        
        if(tclvalue(correlationsValue)=="1") assign("Rcorrelations", TRUE, envir=env)
        else assign("Rcorrelations", FALSE, envir=env)
        
        if(tclvalue(RVValue)=="1") assign("RRV", TRUE, envir=env)
        else assign("RRV", FALSE, envir=env)
        
        if (tclvalue(Fichier)=="") assign("RFichier", NULL, envir=env)
        assign("RFichier", tclvalue(Fichier), envir=env)

        tkdestroy(SortieWin)
      
      }
      
        RV.lab<-tklabel(SortieWin, text=gettextRcmdr("RV coefficients between partial configurations"))
      RV.check<-tkcheckbutton(SortieWin)
      if(RRV) RVValue<-tclVar("1")
      else RVValue<-tclVar("0")
      tkconfigure(RV.check,variable=RVValue)

        RVs.lab <-tklabel(SortieWin, text=gettextRcmdr("Standardized RV coefficients between partial configurations"))
        RVs.check <- tkcheckbutton(SortieWin)
        if(RRVs) RVsValue <- tclVar("1")
        else RVsValue <- tclVar("0")
        tkconfigure(RVs.check,variable=RVsValue)

      simi.lab<-tklabel(SortieWin,text=gettextRcmdr("Procrustes similarity indexes between partial configurations"))
        simi.check <- tkcheckbutton(SortieWin)
        if(Rsimi) simi.Value <- tclVar("1")
        else simi.Value <- tclVar("0")
        tkconfigure(simi.check,variable=simi.Value)
        
      scaling.lab<-tklabel(SortieWin,text=gettextRcmdr("Isotropic scaling factors"))
        scaling.check <- tkcheckbutton(SortieWin)
        if(Rscaling) scalingValue <- tclVar("1")
        else scalingValue <- tclVar("0")
        tkconfigure(scaling.check,variable=scalingValue)
      
      dep.lab <-tklabel(SortieWin, text=gettextRcmdr("Initial partial configurations"))
        dep.check <- tkcheckbutton(SortieWin)
        if(Rdep) depValue <- tclVar("1")
        else depValue <- tclVar("0")
        tkconfigure(dep.check,variable=depValue)
        
      consensus.lab<-tklabel(SortieWin,text=gettextRcmdr("Consensus configuration"))
        consensus.check <- tkcheckbutton(SortieWin)
        if(Rconsensus) consensusValue <- tclVar("1")
        else consensusValue <- tclVar("0")
        tkconfigure(consensus.check,variable=consensusValue)
      
      Xfin.lab<-tklabel(SortieWin,text=gettextRcmdr("Partial configurations after transformations"))
        Xfin.check <- tkcheckbutton(SortieWin)
        if(RXfin) XfinValue <- tclVar("1")
        else XfinValue <- tclVar("0")
        tkconfigure(Xfin.check,variable=XfinValue)
      
      PANOVA.lab<-tklabel(SortieWin,text=gettextRcmdr("Procrustes Analysis of Variance tables, per configuration, per objet, per dimension"))
        PANOVA.check <- tkcheckbutton(SortieWin)
        if(RPANOVA) PANOVAValue <- tclVar("1")
        else PANOVAValue <- tclVar("0")
        tkconfigure(PANOVA.check,variable=PANOVAValue)
      
      correlations.lab<-tklabel(SortieWin,text=gettextRcmdr("Correlations between initial partial configurations and consensus dimensions"))
        correlations.check <- tkcheckbutton(SortieWin)
      if(Rcorrelations) correlationsValue <- tclVar("1")
        else correlationsValue <- tclVar("0")
        tkconfigure(correlations.check,variable=correlationsValue)
        
      RFichierFrame<-tkframe(SortieWin,borderwidth=2)
      if (is.null(RFichier)) Fichier <- tclVar("")
      else Fichier<-tclVar(RFichier)
      Fichier.entry <-tkentry(RFichierFrame,width="40",textvariable=Fichier)

      SortieOK.but<-tkbutton(SortieWin,text="OK",width=16,command=onOK.sortie)

      tkgrid(tklabel(SortieWin, text = gettextRcmdr("Select output options"), fg ="blue"),  columnspan = 2, sticky = "w")
      tkgrid(tklabel(SortieWin, text = " "))
      tkgrid(consensus.lab,consensus.check,sticky="w")
      tkgrid(RV.lab,RV.check,sticky="w")
      tkgrid(RVs.lab,RVs.check,sticky="w")
      tkgrid(simi.lab,simi.check,sticky="w")
      tkgrid(scaling.lab,scaling.check,sticky="w")
      tkgrid(dep.lab,dep.check,sticky="w")
      tkgrid(Xfin.lab,Xfin.check,sticky="w")      
      tkgrid(correlations.lab,correlations.check,sticky="w")      
      tkgrid(PANOVA.lab,PANOVA.check,sticky="w")      
      tkgrid(tklabel(SortieWin, text = " "))
      tkgrid(tklabel(RFichierFrame,text=gettextRcmdr("Print results on a 'csv' file")),Fichier.entry)
      tkgrid(RFichierFrame)
      tkgrid(SortieOK.but)
   }
    
    SortieFrame<-tkframe(IlluFrame)
    Sortie.but<-tkbutton(SortieFrame, textvariable=.SortieLabel, command=OnSortie, borderwidth=3)
    tkgrid(Sortie.but, sticky="ew")
  })



  #! fonction pour la gestion des options graphiques 
  PLOT.GPA<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.graph<-0
    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))
    #déclaration des variables
        
    Rchoix<-TRUE
    RTitle<-NULL
    Rlabel.indMoy<-TRUE
    Rhabillage<-"group"
    Rpartial<-"all"
    RpartialSouris<-FALSE
    Rchrono<-FALSE
    RXlimInd<-NULL
    RYlimInd<-NULL
    
    OnPlot<-function()
    {
      PlotWin<-tktoplevel()
      tkwm.title(PlotWin,gettextRcmdr("Graphical options"))
      tkwm.geometry(PlotWin, "-100+50")
      PlotWin2<-tkframe(PlotWin)

      #création de la fonction onOKsub
      onOKsub<-function()
      {
        assign("compteur.graph", compteur.graph+1, envir=env)
        if(compteur.graph>0) tclvalue(.PlotLabel)<-paste(label, gettextRcmdr(""), sep=" ")
        tkconfigure(Plot.but, fg="blue")
        #récupération des dimensions à représenter
            
        # gestion des entrées de la partie graphique des individus
        if(tclvalue(ind.check.value)==1) assign("Rchoix", TRUE, envir=env)
        else assign("Rchoix", FALSE, envir=env)

        if(Rchoix)
        {
          if (tclvalue(Titre)==" ") assign("RTitle", NULL, envir=env)
          assign("RTitle", tclvalue(Titre), envir=env)

          label.tmp.indMoy<-tclvalue(label.indMoy.checkValue)
          if(label.tmp.indMoy==1) assign("Rlabel.indMoy", TRUE, envir=env)
          else assign("Rlabel.indMoy", FALSE, envir=env)
          
          habillage.tmp<-listgraph.nom[as.numeric(tkcurselection(listgraph))+1]
          if(length(habillage.tmp)==0) assign("Rhabillage","none", envir=env)
          else assign("Rhabillage", habillage.tmp, envir=env)

          if(tclvalue(XlimIndMin)=="" | tclvalue(XlimIndMax)=="") assign("RXlimInd", NULL, envir=env)
          else assign("RXlimInd", c(as.numeric(tclvalue(XlimIndMin)), as.numeric(tclvalue(XlimIndMax))), envir=env)
          if(tclvalue(YlimIndMin)=="" | tclvalue(YlimIndMax)=="") assign("RYlimInd", NULL, envir=env)
          else assign("RYlimInd", c(as.numeric(tclvalue(YlimIndMin)), as.numeric(tclvalue(YlimIndMax))), envir=env)
          
          partial.tmp<-listpartial.nom[as.numeric(tkcurselection(listpartial))+1]
          if(length(partial.tmp)==0) assign("Rpartial",NULL, envir=env)
          else assign("Rpartial", partial.tmp, envir=env)
          
          chrono.tmp<-tclvalue(partial.chrono.checkValue)
          if(chrono.tmp=="1") assign("Rchrono", TRUE, envir=env)
          else assign("Rchrono", FALSE, envir=env)
          
          souris.tmp<-tclvalue(partial.souris.checkValue)
          if(souris.tmp=="1") assign("RpartialSouris", TRUE, envir=env)
          else assign("RpartialSouris", FALSE, envir=env)
          
        }
        tkdestroy(PlotWin)
      }
    
      # création l'interface "options graphiques"
    
              
      ##########################  
      # construction de la partie graphique des individus
      PlotIndFrame<-tkframe(PlotWin, borderwidth=5, relief="groove")
      
      RchoixFrame<-tkframe(PlotIndFrame,borderwidth=2)
      ind.check<-tkcheckbutton(RchoixFrame)
      if(Rchoix) ind.check.value<-tclVar("1")
      else ind.check.value<-tclVar("0")
      tkconfigure(ind.check, variable=ind.check.value)
      tkgrid(tklabel(RchoixFrame, text=gettextRcmdr("Graph of the individuals"), font=font2),ind.check)
      tkgrid(tklabel(RchoixFrame, text=" "))
      
      RTitleFrame<-tkframe(PlotIndFrame,borderwidth=2)
      if (is.null(RTitle)) Titre <- tclVar(" ")
      else Titre<-tclVar(RTitle)
      Titre.entry <-tkentry(RTitleFrame,width="40",textvariable=Titre)
      tkgrid(tklabel(RTitleFrame,text=gettextRcmdr("Title of the graph")),Titre.entry)
      
      RlabelFrame<-tkframe(PlotIndFrame,borderwidth=2)
      label.indMoy.check<-tkcheckbutton(RlabelFrame)
      if (Rlabel.indMoy) label.indMoy.checkValue<-tclVar("1")
      else label.indMoy.checkValue<-tclVar("0") 
      tkconfigure(label.indMoy.check, variable=label.indMoy.checkValue)
      tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Draw labels for the mean individuals")),label.indMoy.check)
        
      RhabillageFrame<-tkframe(PlotIndFrame,borderwidth=2)
      listgraph<-tklistbox(RhabillageFrame,height=3, selectmode="single",exportselection="FALSE",yscrollcommand=function(...) tkset(scrgraph,...))
      scrgraph <-tkscrollbar(RhabillageFrame,repeatinterval=5,command=function(...)tkyview(listgraph,...))
      listgraph.nom<-c("group","ind")
      tkinsert(listgraph,"end","by.group")
      tkinsert(listgraph,"end","by.individual")
      if(Rhabillage=="group") tkselection.set(listgraph,0)
      if(Rhabillage=="ind") tkselection.set(listgraph,1)
##      indice<-2      
##      for (i in 1:ncol(donnee))
##      {
##          if(is.factor(donnee[,i]))
##          {
##          tkinsert(listgraph,"end",vars[i])
##          listgraph.nom<-c(listgraph.nom,vars[i])
##          if(Rhabillage==vars[i]) tkselection.set(listgraph, indice)
##          indice<-indice+1
##        }
##      }
      tkgrid(tklabel(RhabillageFrame, text=gettextRcmdr("Select drawing for the individuals")))
      tkgrid(listgraph, scrgraph, sticky = "nw")
      tkgrid.configure(scrgraph, sticky = "wns")
      tkgrid.configure(listgraph, sticky = "ew")
      
                  
      RlimFrame<-tkframe(PlotIndFrame,borderwidth=2)
      if(is.null(RXlimInd)) XlimIndMin<-tclVar("")
      else XlimIndMin<-tclVar(paste(RXlimInd[1]))
      XlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMin)
      if (is.null(RXlimInd)) XlimIndMax<- tclVar("")
      else XlimIndMax<-tclVar(paste(RXlimInd[1]))
      XlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMax)
      tkgrid(tklabel(RlimFrame,text=gettextRcmdr("x limits of the graph:")),XlimIndMin.entry, XlimIndMax.entry)
        if(is.null(RYlimInd)) YlimIndMin<- tclVar("")
      else YlimIndMin<-tclVar(paste(RYlimInd[1]))
      YlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMin)
      if (is.null(RYlimInd)) YlimIndMax<- tclVar("")
      else YlimIndMax<-tclVar(paste(RYlimInd[2]))
        YlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMax)
        tkgrid(tklabel(RlimFrame,text=gettextRcmdr("y limits of the graph:")),YlimIndMin.entry,YlimIndMax.entry)
  
      RpartialFrame<-tkframe(PlotIndFrame,borderwidth=2)
      listpartial<-tklistbox(RpartialFrame,height=7, selectmode="extended",exportselection="FALSE",yscrollcommand=function(...) tkset(scrpartial,...))
      scrpartial<-tkscrollbar(RpartialFrame,repeatinterval=5,command=function(...)tkyview(listpartial,...))
      listpartial.nom<-NULL
      indice<-0
      for (i in 1:nrow(donnee)) {
          tkinsert(listpartial,"end",rows[i])
        listpartial.nom<-c(listpartial.nom,rows[i])
        if(rows[i] %in% Rpartial) tkselection.set(listpartial, indice)
        indice<-indice+1 
      }
      partial.souris.check<-tkcheckbutton(RpartialFrame)
      if (RpartialSouris) partial.souris.checkValue<-tclVar("1")
      else partial.souris.checkValue<-tclVar("0") 
      partial.chrono.check<-tkcheckbutton(RpartialFrame)
      if (Rchrono) partial.chrono.checkValue<-tclVar("1")
      else partial.chrono.checkValue<-tclVar("0")
      tkconfigure(partial.souris.check, variable=partial.souris.checkValue)
      tkconfigure(partial.chrono.check, variable=partial.chrono.checkValue)      
      tkgrid(tklabel(RpartialFrame, text=gettextRcmdr("Select the individuals for which partial points are drawn")))
      tkgrid(listpartial, scrpartial, sticky = "nw")
      tkgrid.configure(scrpartial, sticky = "wns")
      tkgrid.configure(listpartial, sticky = "ew")
      tkgrid(tklabel(RpartialFrame, text=gettextRcmdr("Interactive selection of the individuals")), partial.souris.check)
      tkgrid(tklabel(RpartialFrame, text=gettextRcmdr("Chronologic representation of the partial points")), partial.chrono.check)
  
      #mise en page des différents frames de PlotIndFrame
      tkgrid(RchoixFrame)
      tkgrid(RTitleFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))
      tkgrid(RlabelFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))
      tkgrid(RhabillageFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))
      tkgrid(RpartialFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))      
      tkgrid(RlimFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))
              
      #mise en page de plotWin
      subOKCancelHelp(PlotWin, "plot.GPA")
      tkgrid(PlotIndFrame, PlotWin2, sticky="ns")
      tkgrid(subButtonsFrame, sticky="ew", columnspan=2)
    }

    PlotFrame<-tkframe(IlluFrame)
    Plot.but<-tkbutton(PlotFrame, textvariable=.PlotLabel, command=OnPlot, borderwidth=3)
    tkgrid(Plot.but, sticky="ew")
  }) 



    #! fonction associée au bouton Appliquer, execute sans détruire l'interface graphique
  OnAppliquer<-function()
  {  
    # récupération des paramètres de la fenêtre principale
    nom.res<-tclvalue(resu.val)
    if (length(ls(pat=nom.res))>0) justDoIt(paste('remove (',nom.res,')'))
    nbiter<-as.numeric(tclvalue(nbiter.val))
    scaling<-as.logical(as.numeric(tclvalue(scale.bool)))
    tolerance<-as.numeric(tclvalue(tolerance.val))
    Axe<-c(as.numeric(tclvalue(Axe1)), as.numeric(tclvalue(Axe2)))
    
    # gestion du tableau de données pour la GPA
    group<-NULL
    type<-NULL
    name.group<-NULL
    num.group.sup<-NULL
    variables<-NULL
    indice.grpe<-1
      #récupération des groupes quanti actif
    nb.GQA<-length(listQuantiAct.nom)
    if(nb.GQA>=1) {
      name.group<-c(name.group, listQuantiAct.nom)
      for(i in 1:nb.GQA) {
        eval(parse(text=paste("liste.var.GQA<-", listQuantiAct.nom[i], ".var", sep="")))
        group<-c(group, length(liste.var.GQA)-1)
        type<-c(type,liste.var.GQA[1])
        variables<-c(variables, liste.var.GQA[-1])
        indice.grpe<-indice.grpe+1
      }
    }
    
      #construction du tableau de données.GPA
      commande.data<-paste(activeDataSet(),'.GPA', '<-', activeDataSet(),'[ , c("',paste(variables, collapse='", "'), '")]', sep="")
      
      justDoIt(commande.data)
      logger(commande.data)
      donnee.depart<-activeDataSet()
      activeDataSet(paste(activeDataSet(),'.GPA', sep=""))

      # gestion de la commande réalisant la GPA     
      commande.GPA<-paste(nom.res, '<-GPA(', activeDataSet(), ', group=c(',paste(group, collapse=", "), '), name.group=c("',paste(name.group, collapse='", "'), '"), scale=',scaling,', graph=FALSE',sep="")
      if (nbiter!=200) commande.GPA<-paste(commande.GPA, ', nbiteration =',nbiter,sep='')
      if (tolerance!=1e-10) commande.GPA<-paste(commande.GPA, ', tolerance =',tolerance,sep='')
      commande.GPA<-paste(commande.GPA, ')',sep='')
      justDoIt(commande.GPA)
      logger(commande.GPA)
      
      if((Rchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)){
        if ((Rhabillage!="none") & (Rhabillage!="ind") & (Rhabillage!="group")) {
          Rhabillage<-which(colnames(get(.activeDataSet))==Rhabillage)
          if(length(Rhabillage)==0) Rhabillage<-"none"
        }
        if (Rhabillage=="none") Rhabillage<-paste('"', Rhabillage, '"', sep="")
        if (Rhabillage=="ind") Rhabillage<-paste('"', Rhabillage, '"', sep="")
        if (Rhabillage=="group") Rhabillage<-paste('"', Rhabillage, '"', sep="")
        
        if(RpartialSouris){
          commande.plotI<-paste('plot.GPApartial(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), lab.ind.moy=', Rlabel.indMoy, ', habillage=', Rhabillage, sep="")
          if (!is.null(RXlimInd)) commande.plotI<-paste(commande.plotI, ', xlim=c(', paste(RXlimInd, collapse=", "), ')')
          if (!is.null(RYlimInd)) commande.plotI<-paste(commande.plotI, ', ylim=c(', paste(RYlimInd, collapse=", "), ')')
          if (Rchrono) commande.plotI<-paste(commande.plotI, ', chrono=', Rchrono, sep='')
          if (is.null(RTitle)) commande.plotI <- paste(commande.plotI,')', sep="")
          else {
            if (RTitle ==" ") commande.plotI <- paste(commande.plotI,')', sep="")
            else commande.plotI <- paste(commande.plotI,', title="', RTitle,'")', sep="")
          }
        }
        else {
          commande.plotI<-paste('plot.GPA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), lab.ind.moy=', Rlabel.indMoy, ', habillage=', Rhabillage, sep="") 
          if (!is.null(RXlimInd)) commande.plotI<-paste(commande.plotI, ', xlim=c(', paste(RXlimInd, collapse=", "), ')')
          if (!is.null(RYlimInd)) commande.plotI<-paste(commande.plotI, ', ylim=c(', paste(RYlimInd, collapse=", "), ')')
          if (!is.null(Rpartial)) commande.plotI<-paste(commande.plotI, ', partial=c("', paste(Rpartial, collapse='", "'),'")', sep='')
          if (Rchrono) commande.plotI<-paste(commande.plotI, ', chrono=', Rchrono, sep='')
          if (is.null(RTitle)) commande.plotI <- paste(commande.plotI,')', sep="")
          else {
            if (RTitle ==" ") commande.plotI <- paste(commande.plotI,')', sep="")
            else commande.plotI <- paste(commande.plotI,', title="', RTitle,'")', sep="")
          }
        }
        justDoIt(commande.plotI)
        logger(commande.plotI)
      }
      
      # gestion de l'édition de certains resultats
    if (RFichier==""){
      if(RRV) doItAndPrint(paste(nom.res, '$RV', sep=""))
      if(RRVs) doItAndPrint(paste( nom.res, '$RVs', sep=""))
      if(Rsimi) doItAndPrint(paste( nom.res, '$simi', sep=""))
      if(Rscaling) doItAndPrint(paste( nom.res, '$scaling', sep=""))
      if(Rdep) doItAndPrint(paste( nom.res, '$dep', sep=""))
      if(Rconsensus) doItAndPrint(paste( nom.res, '$consensus', sep=""))
      if(RXfin) doItAndPrint(paste( nom.res, '$Xfin', sep=""))
      if(Rcorrelations) doItAndPrint(paste( nom.res, '$correlations', sep=""))
      if(RPANOVA) doItAndPrint(paste( nom.res, '$PANOVA', sep=""))
    }
    else {
      Fich = RFichier
      if (substr(Fich,1,1)!='"') Fich = paste('"',Fich,sep='')
      if (substr(Fich,nchar(Fich),nchar(Fich))!='"') Fich = paste(Fich,'"',sep='')
      append = FALSE
      if(RRV){
        doItAndPrint(paste('write.infile(', nom.res, '$RV, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(RRVs){
        doItAndPrint(paste('write.infile(', nom.res, '$RVs, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rsimi){
        doItAndPrint(paste('write.infile(', nom.res, '$simi, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rscaling){
        doItAndPrint(paste('write.infile(', nom.res, '$scaling, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rdep){
        doItAndPrint(paste('write.infile(', nom.res, '$dep, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rconsensus){
        doItAndPrint(paste('write.infile(', nom.res, '$consensus, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(RXfin){
        doItAndPrint(paste('write.infile(', nom.res, '$Xfin, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rcorrelations){
        doItAndPrint(paste('write.infile(', nom.res, '$correlations, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(RPANOVA) doItAndPrint(paste('write.infile(', nom.res, '$PANOVA, file =',Fich,',append=',append,')', sep=""))
    }

      # Re-chargement du tableau de départ et supression du tableau temporaire
      activeDataSet(donnee.depart)
      justDoIt(paste('remove(',activeDataSet(),'.GPA)',sep=""))
      logger(paste('remove(',activeDataSet(),'.GPA)',sep=""))   
  }


    #! fonction associée au bouton OK, execute et détruit l'interface graphique
  onOK<-function()
  {
    OnAppliquer()
    tkdestroy(top)     
  }



#                   Création de la fenêtre top                                 #
################################################################################
  top<-tktoplevel(borderwidth=10)
  tkwm.title(top,gettextRcmdr("GPA"))
  tkwm.geometry(top, "-50+50")
  
  # définition des polices
  font2<-tkfont.create(family="times",size=12,weight="bold")
  fontheading<-tkfont.create(family="times",size=11,weight="bold")

  # récupération du jeu de données actif
  donnee<-get(.activeDataSet)
  vars<-colnames(donnee)
  rows<-rownames(donnee)
  
  # création du frame contenant les listes groupes quanti
  ListeQuantiFrame<- tkframe(top, borderwidth=2, relief="groove")
  label.quantiFrame.var<-tclVar(gettextRcmdr("group"))
  label.quantiFrame<-tklabel(ListeQuantiFrame, textvariable=label.quantiFrame.var,fg = "darkred", font=fontheading)
  # liste des groupes de variables quanti Actives
  listQuantiAct<-tklistbox(ListeQuantiFrame,selectmode="extended",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(scrQuantiAct,...))
  scrQuantiAct<-tkscrollbar(ListeQuantiFrame,repeatinterval=5,command=function(...)tkyview(listQuantiAct,...))
  listQuantiAct.nom<-NULL
  
    # boutons d'action groupes quantitative
  supprimeQuanti.funct(label="Delete")  
  ajoutQuanti.funct(label=gettextRcmdr("Add 1 group"), firstLabel=gettextRcmdr("Add 1 group"))
  modifQuanti.funct(label=gettextRcmdr("Modify 1 group"), firstLabel=gettextRcmdr("Modify 1 group"))
    # mise en forme de ListeQuantiFrame
  tkgrid(label.quantiFrame, columnspan=11, sticky = "ew")
  tkgrid(listQuantiAct, scrQuantiAct)
  tkgrid.configure(scrQuantiAct, column=3, sticky="wns")
  tkgrid.configure(listQuantiAct, sticky = "ew", column=4, columnspan=2)
  tkgrid.configure(tklabel(ListeQuantiFrame, text=" "))
  tkgrid.configure(GpeQuantiFrame,ModifGpeQuantiFrame, SupGpeQuantiFrame)
  tkgrid.configure(GpeQuantiFrame, sticky = "ew", column=1, columnspan=2)
  tkgrid.configure(ModifGpeQuantiFrame, sticky = "ew", column=4, columnspan=2)
  tkgrid.configure(SupGpeQuantiFrame, sticky = "ew", column=7, columnspan=2)
  tkgrid.columnconfigure(ListeQuantiFrame,0, minsize=25)
  tkgrid.columnconfigure(ListeQuantiFrame,10, minsize=25)
  tkgrid.columnconfigure(ListeQuantiFrame,3, minsize=25)
  tkgrid.columnconfigure(ListeQuantiFrame,9, minsize=25)
  tkgrid.columnconfigure(ListeQuantiFrame,4, minsize=35)
  tkgrid.columnconfigure(ListeQuantiFrame,5, minsize=35)
 
    
   # création de tous les boutons d'options dans IlluFrame
  IlluFrame<- tkframe(top, borderwidth=2)
  Reinitializ.but<-tkbutton(IlluFrame, text="Restart",width=18,command=Reinitializ.funct, borderwidth=3)
    # mise en page de IlluFrame
  PLOT.GPA(label=gettextRcmdr("Graphical options"), firstLabel=gettextRcmdr("Graphical options"))
  Sortie.funct(label=gettextRcmdr("Outputs"), firstLabel=gettextRcmdr("Outputs"))
  tkgrid(tklabel(IlluFrame, text=""))
  tkgrid(PlotFrame, SortieFrame, Reinitializ.but, columnspan=7)
  tkgrid.configure(PlotFrame, column=1, columnspan=1)
  tkgrid.configure(SortieFrame, column=3, columnspan=1)
  tkgrid.configure(Reinitializ.but, column=5, columnspan=1)      
  tkgrid.columnconfigure(IlluFrame,0, minsize=25)
  tkgrid.columnconfigure(IlluFrame,2, minsize=40)  
  tkgrid.columnconfigure(IlluFrame,4, minsize=25)  


    # création des options dans OptionFrame  
  OptionFrame<-tkframe(top, borderwidth=2, relief="groove")
  resu.lab<-tklabel(OptionFrame,text=gettextRcmdr("Name of the result object: "))
  resu.val<-tclVar("res")
  resu<-tkentry(OptionFrame,width=10,textvariable=resu.val)
    scale.check <- tkcheckbutton(top)
    scale.bool <- tclVar("1")
    tkconfigure(scale.check,variable=scale.bool)
    scale.lab<-tklabel(OptionFrame,text="Scale unit the variables:")
  nbiter.lab<-tklabel(OptionFrame,text=gettextRcmdr("Maximum number of iteration for the algorithm: "))
  nbiter.val<-tclVar("200") 
  nbiter<-tkentry(OptionFrame,width=5,textvariable=nbiter.val)
  tolerance.lab<-tklabel(OptionFrame,text=gettextRcmdr("Threshold with respect to which the algorithm stops: "))
  tolerance.val<-tclVar("1e-10") 
  tolerance<-tkentry(OptionFrame,width=5,textvariable=tolerance.val)
  Axe.label<-tklabel(OptionFrame,text=gettextRcmdr("Select the dimensions for the graphs:"))
  Axe1<-tclVar("1")
  Axe2<-tclVar("2")
  Axe1.entry <-tkentry(OptionFrame,width="5",textvariable=Axe1)
  Axe2.entry <-tkentry(OptionFrame,width="5",textvariable=Axe2)
  
    # mise en page de OptionFrame
  tkgrid(tklabel(OptionFrame,text=gettextRcmdr("Main options"), fg = "darkred"), columnspan=8, sticky="we") 
  tkgrid(tklabel(OptionFrame,text="")) 
  tkgrid(scale.lab,scale.check,sticky="w")
  tkgrid(Axe.label,Axe1.entry , Axe2.entry, sticky="w")
  tkgrid(nbiter.lab, nbiter)
  tkgrid(tolerance.lab, tolerance)
  tkgrid(resu.lab, resu)
  tkgrid.configure(scale.lab, nbiter.lab, tolerance.lab, resu.lab, Axe.label, column=1, columnspan=4, sticky="w")
  tkgrid.configure(scale.check, tolerance, nbiter, resu, column=6, columnspan=2, sticky="e")
  tkgrid.configure(Axe1.entry, column=6, columnspan=1, sticky="w")
  tkgrid.configure(Axe2.entry, column=7, columnspan=1, sticky="e")
  tkgrid.columnconfigure(OptionFrame,0, minsize=25)
  tkgrid.columnconfigure(OptionFrame,5, minsize=40)
  tkgrid.columnconfigure(OptionFrame,8, minsize=25)

  appliquer.but<-tkbutton(top, text="Apply",width=12,command=OnAppliquer, borderwidth=3, fg="#690f96")
  OKCancelHelp(helpSubject="GPA")

  # Mise en page de top
  tkgrid(tklabel(top, text=gettextRcmdr("General Procrustes Analysis (GPA)"),font=fontheading), columnspan=3)
  tkgrid(tklabel(top,text=""))
  tkgrid(ListeQuantiFrame, column=1, columnspan=1, sticky="ew")
  tkgrid(tklabel(top,text=""))    
  tkgrid(tklabel(top,text=""))    
  tkgrid(IlluFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text=""))    
  tkgrid(OptionFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc       
  tkgrid(appliquer.but, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc  
  tkgrid(buttonsFrame, column=1, columnspan=1, sticky="ew" )
  
}  

#############################FIN FONCTION FactoGPA ##############################

#############################DEBUT FONCTION Factocatdes ##############################
Factocatdes<-function(){
require(tcltk)
#require(SensoMineR)
top<-tktoplevel(borderwidth=10)
tkwm.title(top,"Description of categories")


####### FRAMES -------------------------------------------------------------------------------------------------------------------------------
    descopFrame<-tkframe(top)
    descFrame <- tkframe(descopFrame)
    listdesc<-tklistbox(descFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
   
    factFrame <- tkframe(descopFrame)
    listfact<-tklistbox(factFrame,selectmode="single",height=7,width=20,yscrollcommand=function(...) tkset(scrfact,...))
   
    scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
    tkgrid(listfact, scrfact,sticky = "nw")
    tkgrid.configure(scrfact, sticky = "wns")
    tkgrid.configure(listfact,sticky = "ew")

    descopFrame2<-tkframe(top)
    descFrame2 <- tkframe(descopFrame2)
    listdesc2<-tklistbox(descFrame2,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr2 <- tkscrollbar(descFrame2,repeatinterval=5,command=function(...)tkyview(listdesc,...))

    donnee<-get(.activeDataSet)
    nomdonnee<-.activeDataSet
    vars<-colnames(donnee)
    vars.fact = NULL
    vars.desc = NULL
    vars.desc2 = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
      else {
        vars.fact = c(vars.fact,vars[i])
        tkinsert(listfact,"end",vars[i])
        vars.desc2 = c(vars.desc2,vars[i])
        tkinsert(listdesc2,"end",vars[i])
      }
    }
    tkgrid(listdesc, scr,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")
    
    tkgrid(listdesc2, scr2,sticky = "nw")
    tkgrid.configure(scr2, sticky = "wns")
    tkgrid.configure(listdesc2,sticky = "ew")
  optionsFrame <- tkframe(descopFrame,borderwidth=2,relief="ridge")
  
    probab.val<-tclVar("0.05")
    probab<-tkentry(optionsFrame,width=10,textvariable=probab.val)
    probab.lab<-tklabel(optionsFrame,text="Probability: ")
    tkgrid(probab.lab,probab,sticky="w")
   
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the results in: ")
    tkgrid(resu.lab,resu,sticky="w")


#################### Fonctions liées au bouton 'Sorties' --------------------------------------------------------------------------------------------------

env<-environment()
if (length(vars.fact)>1){
  Gquali<-TRUE
  Gcateg<-TRUE
}
else{
  Gquali<-FALSE
  Gcateg<-FALSE
}
if (length(vars.desc)>0) Gquanti<-TRUE
else  Gquanti<-FALSE

onSortie<-function(){
    sortiestop<-tktoplevel(borderwidth=10)
    tkwm.title(sortiestop,"Outputs")

    onOKsortie<-function(){
        if(tclvalue(qualiValue)=="1") assign("Gquali", TRUE, envir=env)
        else assign("Gquali", FALSE, envir=env)
        if(tclvalue(categValue)=="1") assign("Gcateg", TRUE, envir=env)
        else assign("Gcateg", FALSE, envir=env)
        if(tclvalue(quantiValue)=="1") assign("Gquanti", TRUE, envir=env)
        else assign("Gquanti", FALSE, envir=env)
        tkdestroy(sortiestop)
    }

    quali.check <- tkcheckbutton(sortiestop)
    if (Gquali) qualiValue <- tclVar("1")
    else qualiValue <- tclVar("0")
    quali.lab<-tklabel(sortiestop,text="Description by qualitative variables")
    tkconfigure(quali.check,variable=qualiValue)

    categ.check <- tkcheckbutton(sortiestop)
    if (Gcateg) categValue <- tclVar("1")
    else categValue <- tclVar("0")
    tkconfigure(categ.check,variable=categValue)
    categ.lab<-tklabel(sortiestop,text="Description by category")

    quanti.check <- tkcheckbutton(sortiestop)
    if (Gquanti) quantiValue <- tclVar("1")
    else quantiValue <- tclVar("0")
    tkconfigure(quanti.check,variable=quantiValue)
    quanti.lab<-tklabel(sortiestop,text="Description by quantitative variables")

      tkgrid(tklabel(sortiestop,text="Options to print the results for description of categories",fg="red"))
      tkgrid(quali.lab,quali.check,sticky="w")
      tkgrid(categ.lab,categ.check,sticky="w")
      tkgrid(quanti.lab,quanti.check,sticky="w")

    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tkbutton(sortiestop,text="OK",width=9,command=onOKsortie))
    tkfocus(sortiestop)
}

####### Fonction liée au bouton Appliquer, appel de la fonction Paneliperf -------------------------------------------------------------------------------
App<-function(){
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))

nbitemlist2<-c(tclvalue(tkcurselection(listdesc2)))
nbitem2<-unlist(strsplit(nbitemlist2,"\\ "))

    variable <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
    variable2 <- vars.desc2[as.numeric(tkcurselection(listdesc2))+1]
    resultat<-tclvalue(resu.val)
    probabltat<-tclvalue(probab.val)
    done = 0  
    if (length(vars.fact[as.numeric(tkcurselection(listfact))+1])==0) tkmessageBox(message="No variable selected for the variable to describe",icon="warning",type="ok")         
    else {
      done=1
      if (length(nbitem2)>0)  {      
        if(any(ind<-grep(vars.fact[as.numeric(tkcurselection(listfact))+1],variable2))) variable2=variable2[-ind]   
        if(length(variable2!=0)) {     
          if (length(nbitem)>0) {  command3=paste(resultat,'=catdes(',nomdonnee,'[,c("', paste(vars.fact[as.numeric(tkcurselection(listfact))+1], collapse='", "'),'", "',   paste(variable, collapse='", "'),'", "', paste(variable2, collapse='", "'),'")] ,num.var=1,proba=',as.numeric(tclvalue(probab.val)),')',sep='')  }
          else{
            if(length(vars.desc>0)){ command3=paste(resultat,'=catdes(',nomdonnee,'[,c("', paste(vars.fact[as.numeric(tkcurselection(listfact))+1], collapse='", "'),'", "',paste(vars.desc, collapse='", "'),'", "', paste(variable2, collapse='", "'),'")] ,num.var=1,proba=',as.numeric(tclvalue(probab.val)),')',sep='')  }
            else{command3=paste(resultat,'=catdes(',nomdonnee,'[,c("', paste(vars.fact[as.numeric(tkcurselection(listfact))+1], collapse='", "'),'", "', paste(variable2, collapse='", "'),'")] ,num.var=1,proba=',as.numeric(tclvalue(probab.val)),')',sep='') }
          }
          justDoIt(command3)
          logger(command3)
        }
        else{
          if (length(nbitem)>0) command3=paste(resultat,'=catdes(',nomdonnee,'[,c("', paste(vars.fact[as.numeric(tkcurselection(listfact))+1], collapse='", "'),'", "',   paste(variable, collapse='", "'),'")] ,num.var=1,proba=',as.numeric(tclvalue(probab.val)),')',sep='')  
          else{
            i=grep(vars.fact[as.numeric(tkcurselection(listfact))+1],vars)
            command3=paste(resultat,'=catdes(',nomdonnee,',num.var=',i,',proba=',as.numeric(tclvalue(probab.val)),')',sep='') 
          }
          justDoIt(command3)
          logger(command3)
        }
      }
      else{
        if(length(vars.desc2)==1){ 
          if (length(nbitem)>0) { command3=paste(resultat,'=catdes(',nomdonnee,'[,c("', paste(vars.fact[as.numeric(tkcurselection(listfact))+1], collapse='", "'),'", "', paste(variable, collapse='", "'),'")] ,num.var=1,proba=',as.numeric(tclvalue(probab.val)),')',sep='')  }
          else{
            i=grep(vars.fact[as.numeric(tkcurselection(listfact))+1],vars)
            command3=paste(resultat,'=catdes(',nomdonnee,',num.var=',i,',proba=',as.numeric(tclvalue(probab.val)),')',sep='')
          }
          justDoIt(command3)
          logger(command3)
        }
        else{          
          if(any(ind<-grep(vars.fact[as.numeric(tkcurselection(listfact))+1],vars.desc2))) vars.desc2=vars.desc2[-ind]  
          if (length(nbitem)>0) command3=paste(resultat,'=catdes(',nomdonnee,'[,c("', paste(vars.fact[as.numeric(tkcurselection(listfact))+1], collapse='", "'),'", "', paste(variable, collapse='", "'),'", "', paste(vars.desc2, collapse='", "'),'")] ,num.var=1,proba=',as.numeric(tclvalue(probab.val)),')',sep='')  
          else{
            i=grep(vars.fact[as.numeric(tkcurselection(listfact))+1],vars)
            command3=paste(resultat,'=catdes(',nomdonnee,',num.var=',i,',proba=',as.numeric(tclvalue(probab.val)),')',sep='')
          }  
          justDoIt(command3)
          logger(command3) 
        }
      }
               
      if (Gquali==1){                
        if ((length(nbitem2)>0) & (length(variable2)>0 ))  {           
          doItAndPrint(paste(resultat,'$test.chi',sep=''))
        }
        if((length(nbitem2)==0) & (length(vars.desc2)>=1))  {       
          doItAndPrint(paste(resultat,'$test.chi',sep=''))
        }
      }
      if ((Gquanti==1)& (length(vars.desc)>0)){
        doItAndPrint(paste(resultat,'$quanti',sep=''))
      }
      if ((Gcateg==1)){
        doItAndPrint(paste(resultat,'$category',sep=''))
      }
    }
        
    return(done)
  }


####### Fonction liée au bouton OK, appel de la fonction Paneliperf -------------------------------------------------------------------------------
onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}

sorties<-tkbutton(optionsFrame,text="Outputs",borderwidth=3,width=12,fg="darkred",command=onSortie)
appel<-tkbutton(top,text="Submit",borderwidth=3,width=12,fg="blue",command=App)
OKCancelHelp(helpSubject="catdes")

tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top, text = "Choose the variable to describe:", fg = "blue"), columnspan = 2, sticky = "w")
##tkgrid(factFrame,columnspan = 2,sticky="w")
tkgrid(descopFrame,columnspan = 2, sticky="w")
tkgrid(factFrame,tklabel(descopFrame,text="    "),optionsFrame,sticky="w")
tkgrid(sorties,sticky="e")
tkgrid.configure(optionsFrame,sticky="s")
 
tkgrid(tklabel(descopFrame,text=""))
if (length(vars.desc)>0){ 
  tkgrid(tklabel(descopFrame, text = "Choose the quantitatives variables (by default all)", fg = "blue"), columnspan = 2, sticky = "w")

  tkgrid(descopFrame,columnspan = 2, sticky="w")
  tkgrid(descFrame,tklabel(descopFrame,text="    "),optionsFrame,sticky="w")
}

if (length(vars.fact)>1){
  tkgrid(tklabel(top,text=""))
  tkgrid(tklabel(top, text = "Choose the qualitatives variables (by default all)", fg = "blue"), columnspan = 2, sticky = "w")
  tkgrid(descopFrame2,columnspan = 2, sticky="w")
  tkgrid(descFrame2,tklabel(descopFrame2,text="    "),sticky="w")
}

tkgrid(tklabel(top,text=""))
tkgrid(appel, sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://factominer.free.fr"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}
#############################FIN FONCTION Factocatdes ##############################

################################################################################
#                              FONCTION FactoAFDM                              #
################################################################################
#! version JMA du 27/12/2006 17:10:39 

FactoAFDM<-function()
{
  require(tcltk)
  require(FactoMineR)

# Fonction pour la gestion des noms ############################################

  nom.correct<-function(text, liste=NULL)
  {
    text<-chartr("^\ ", "...", text)
    if(!is.null(liste)) {
      while(text %in% liste) text<-paste(text, ".bis", sep="")
    }
    return(text)  
  }
################################################################################

#    Création des fonctions pour les options via nouvelle fenêtre graphique   

  top<-tktoplevel(borderwidth=10)
  tkwm.title(top,gettextRcmdr("AFDM"))
  tkwm.geometry(top, "-50+50")
  
  # définition des polices
  font2<-tkfont.create(family="times",size=12,weight="bold")
  fontheading<-tkfont.create(family="times",size=11,weight="bold")

  # récupération du jeu de données actif
  donnee<-get(.activeDataSet)
  vars<-colnames(donnee)
  rows<-rownames(donnee)

    listFrame <- tkframe(top,borderwidth=2)                                                                                                          
    lab1 = tklabel(listFrame,text=paste("Select quantitative variables"),fg="blue")
    lab2 = tklabel(listFrame,text="Select qualitative variables",fg="blue")
    lab3 = tklabel(listFrame,text="      ")
    tkgrid(lab1,lab3,lab2)
    tkgrid.configure(lab1,column=1, columnspan=2, sticky = "nw")
    tkgrid.configure(lab2,column=4, columnspan=2, sticky = "ne")
    tkgrid.configure(lab3,column=3, columnspan=1, sticky = "n")

    listdesc<-tklistbox(listFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(listFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    tkselection.set(listdesc,0)
    listfact<-tklistbox(listFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scrfact,...))
    scrfact <- tkscrollbar(listFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
    vars<-colnames(donnee)
    vars.fact = NULL
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
      else {
        vars.fact = c(vars.fact,vars[i])
        tkinsert(listfact,"end",vars[i])
      }
    }

    tkgrid(listdesc, scr,tklabel(listFrame,text="                                  "),listfact,scrfact,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns", column=2, columnspan=1)
    tkgrid.configure(listdesc,sticky = "ew", column=1, columnspan=1)
    tkgrid.configure(scrfact,sticky = "wns", column=5, columnspan=1)
    tkgrid.configure(listfact,sticky = "ew", column=4, columnspan=1)

 
  Fillu.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    variablefact<-NULL
    .FilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    .factors<-Factors()
    OnFillu<-function()
    {
      if(length(.factors)==0) errorCondition(recall=NULL, message=gettextRcmdr("No Factor available"))
          
      FilluWin<-tktoplevel()
      tkwm.title(FilluWin,gettextRcmdr("Choice of supplementary factors"))
      #création de la fonction FOK.funct
      FOK.funct<-function()
      {
        fact.select<-listfact.nom[as.numeric(tkcurselection(listfact))+1]
        if(length(fact.select)==0) {
          assign("variablefact", NULL, envir=env)
          tclvalue(.FilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Fillu.but, fg="black")
          tkdestroy(FilluWin)
          return()
        }
        assign("variablefact", fact.select, envir=env)
        tclvalue(.FilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Fillu.but, fg="blue")
        tkdestroy(FilluWin)
      }
      
      # création et mise en page de la fenetre Fillu
      listfact<-tklistbox(FilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrfact,...)) # Liste vide
      scrfact <-tkscrollbar(FilluWin,repeatinterval=5,command=function(...)tkyview(listfact,...))
      listfact.nom<-NULL
      indice<-0
      for (i in (1:ncol(donnee))) {
        if (is.factor(donnee[,i])) {
          tkinsert(listfact,"end",vars[i]) # On renseigne la liste
          listfact.nom<-c(listfact.nom,vars[i])
          if(vars[i] %in% variablefact) tkselection.set(listfact, indice)
          indice<-indice+1
        }
      }
  
      FOK.but<-tkbutton(FilluWin, text="OK", width=16,command=FOK.funct)

      tkgrid(tklabel(FilluWin, text=""))
      tkgrid(tklabel(FilluWin, text = gettextRcmdr("Select supplementary factor(s)"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
      tkgrid(listfact, scrfact, sticky = "nw")
      tkgrid.configure(scrfact, sticky = "ens", columnspan=1)
      tkgrid.configure(listfact, sticky = "ew", column=1, columnspan=1)
      tkgrid(tklabel(FilluWin, text=""))
      tkgrid(FOK.but, column=1,columnspan=1, sticky="ew")
      tkgrid(tklabel(FilluWin, text=""))
      tkgrid.columnconfigure(FilluWin,0, minsize=25)
      tkgrid.columnconfigure(FilluWin,2, minsize=25)
  }  

   FilluFrame<-tkframe(IlluFrame)
   if(length(.factors)==0){
     Fillu.but<-tkbutton(FilluFrame, text=gettextRcmdr("No factors available"), borderwidth=3)
     tkconfigure(Fillu.but, fg="grey")
   }
   else Fillu.but<-tkbutton(FilluFrame, textvariable=.FilluLabel, command=OnFillu, borderwidth=3)
   tkgrid(Fillu.but, sticky="ew")

##   Fillu.but<-tkbutton(FilluFrame, textvariable=.FilluLabel, command=OnFillu, borderwidth=3)
##   tkgrid(Fillu.but, sticky="ew")
  })

  #! fonction pour le choix des variables quantitatives supplémentaires 
  Dillu.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    variableillu<-NULL
    .DilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnDillu<-function()
    { 
      DilluWin<-tktoplevel()
      tkwm.title(DilluWin,gettextRcmdr("Select supplementary quantitative variables"))
      #création de la fonction DOK.funct
      DOK.funct<-function()
      {
        vsup.select<-listvar.nom[as.numeric(tkcurselection(listvar))+1]
        if(length(vsup.select)==0)
        {
          assign("variableillu", NULL, envir=env)
          tclvalue(.DilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Dillu.but, fg="black")
          tkdestroy(DilluWin)
          return()
        }
        assign("variableillu", vsup.select, envir=env)
        tclvalue(.DilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Dillu.but, fg="blue")
        tkdestroy(DilluWin)
      }
      
      # création et mise en page de la fenetre Dillu
      listvar<-tklistbox(DilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrvar,...)) # Liste vide
      scrvar <-tkscrollbar(DilluWin,repeatinterval=5,command=function(...)tkyview(listvar,...)) 
      listvar.nom<-NULL
      indice<-0
      for (i in (1:ncol(donnee))) {
          if (is.numeric(donnee[,i])) {
            tkinsert(listvar,"end",vars[i]) # On renseigne la liste
            listvar.nom<-c(listvar.nom,vars[i])
            if(vars[i] %in% variableillu) tkselection.set(listvar, indice)
            indice<-indice+1
          }
      }
  
      DOK.but<-tkbutton(DilluWin, text="OK", width=16,command=DOK.funct)

      tkgrid(tklabel(DilluWin, text=""))
        tkgrid(tklabel(DilluWin, text = gettextRcmdr("Select supplementary quantitative variables"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
        tkgrid(listvar, scrvar, sticky = "nw")
        tkgrid.configure(scrvar, sticky = "ens", columnspan=1)
        tkgrid.configure(listvar, sticky = "ew", column=1, columnspan=1)
        tkgrid(tklabel(DilluWin, text=""))
        tkgrid(DOK.but, column=1,columnspan=1, sticky="ew")
        tkgrid(tklabel(DilluWin, text=""))
        tkgrid.columnconfigure(DilluWin,0, minsize=25)
      tkgrid.columnconfigure(DilluWin,2, minsize=25)
  }  

   DilluFrame<-tkframe(IlluFrame)
   if(length(listNumeric())==0){
     Dillu.but<-tkbutton(DilluFrame, text=gettextRcmdr("No quantitative variable available"), borderwidth=3)
     tkconfigure(Dillu.but, fg="grey")
   }
   else Dillu.but<-tkbutton(DilluFrame, textvariable=.DilluLabel, command=OnDillu, borderwidth=3)
   tkgrid(Dillu.but, sticky="ew")
  })
  
  #! fonction pour le choix des individus supplémentaires 
  Iillu.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    individuillu<-NULL
    .IilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnIillu<-function()
    {   
      IilluWin<-tktoplevel()
      tkwm.title(IilluWin,gettextRcmdr("Select supplementary individuals"))
      #création de la fonction IOK.funct
      IOK.funct<-function()
      {
        ind.select<-rows[as.numeric(tkcurselection(listind))+1]
        if(length(ind.select)==0) {
          assign("individuillu", NULL, envir=env)
          tclvalue(.IilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Iillu.but, fg="black")
          tkdestroy(IilluWin)
          return()
        }
        assign("individuillu", ind.select, envir=env)
        tclvalue(.IilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Iillu.but, fg="blue")
        tkdestroy(IilluWin)
      }
      
      # création et mise en page de la fenetre Fillu
      listind<-tklistbox(IilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrind,...)) # Liste vide
      scrind <-tkscrollbar(IilluWin,repeatinterval=5,command=function(...)tkyview(listind,...)) 
      indice<-0
      for (i in (1:nrow(donnee))) {
        tkinsert(listind,"end",rows[i]) # On renseigne la liste
        if(rows[i] %in% individuillu) tkselection.set(listind, indice)
        indice<-indice+1
      }
  
      IOK.but<-tkbutton(IilluWin, text="OK", width=16,command=IOK.funct)

      tkgrid(tklabel(IilluWin, text=""))
      tkgrid(tklabel(IilluWin, text = gettextRcmdr("Select supplementary individuals"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
      tkgrid(listind, scrind, sticky = "nw")
      tkgrid.configure(scrind, sticky = "ens", columnspan=1)
      tkgrid.configure(listind, sticky = "ew", column=1, columnspan=1)
      tkgrid(tklabel(IilluWin, text=""))
      tkgrid(IOK.but, column=1,columnspan=1, sticky="ew")
      tkgrid(tklabel(IilluWin, text=""))
      tkgrid.columnconfigure(IilluWin,0, minsize=25)
      tkgrid.columnconfigure(IilluWin,2, minsize=25)
  }  

   IilluFrame<-tkframe(IlluFrame)
   Iillu.but<-tkbutton(IilluFrame, textvariable=.IilluLabel, command=OnIillu, borderwidth=3)
   tkgrid(Iillu.but, sticky="ew")
  })
    
  
    #! fonction pour la réinitialisation des paramètres
  Reinitializ.funct<-function()
  {
    tkdestroy(top)
    FactoAFDM()
  }


  #! fonction pour le choix des éléments de sortie
  Sortie.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.sortie<-0
    #déclaration des variables
    Rpropre<-FALSE
    RFichier <- ""
    Rgroupe<-FALSE
    Rindividu<-FALSE
    Rindsup<-FALSE
#    Rquantisummary<-FALSE
    Rquanti<-FALSE
    Rquantisup<-FALSE    
#    Rqualisummary<-FALSE
    Rquali<-FALSE
    Rqualisup<-FALSE
    Rdescdim<-FALSE

    .SortieLabel<-tclVar(paste(firstLabel, "", sep=" "))

    OnSortie<-function()
    {
      SortieWin<-tktoplevel()
      tkwm.title(SortieWin,gettextRcmdr("Output options"))

      #création de la fonction onOKsub
      onOK.sortie<-function()
      {
        assign("compteur.sortie", compteur.sortie+1, envir=env)
        if(compteur.sortie>0) tclvalue(.SortieLabel)<-paste(label, "", sep=" ")
        tkconfigure(Sortie.but, fg="blue")
        
        if(tclvalue(eigValue)=="1") assign("Rpropre", TRUE, envir=env)
        else assign("Rpropre", FALSE, envir=env)
        
        if(tclvalue(groupeValue)=="1") assign("Rgroupe", TRUE, envir=env)
        else assign("Rgroupe", FALSE, envir=env)
        
        if(tclvalue(indValue)=="1") assign("Rindividu", TRUE, envir=env)
        else assign("Rindividu", FALSE, envir=env)
        
        if(tclvalue(ind.sup.Value)=="1") assign("Rindsup", TRUE, envir=env)
        else assign("Rindsup", FALSE, envir=env)
        
#        if(tclvalue(quantiSummaryValue)=="1") assign("Rquantisummary", TRUE, envir=env)
#        else assign("Rquantisummary", FALSE, envir=env)
        
        if(tclvalue(quantiValue)=="1") assign("Rquanti", TRUE, envir=env)
        else assign("Rquanti", FALSE, envir=env)
        
        if(tclvalue(quantisupValue)=="1") assign("Rquantisup", TRUE, envir=env)
        else assign("Rquantisup", FALSE, envir=env)
        
#        if(tclvalue(qualiSummaryValue)=="1") assign("Rqualisummary", TRUE, envir=env)
#        else assign("Rqualisummary", FALSE, envir=env)
        
        if(tclvalue(qualiValue)=="1") assign("Rquali", TRUE, envir=env)
        else assign("Rquali", FALSE, envir=env)
        
        if(tclvalue(qualisupValue)=="1") assign("Rqualisup", TRUE, envir=env)
        else assign("Rqualisup", FALSE, envir=env)
        
        if(tclvalue(descdimValue)=="1") assign("Rdescdim", TRUE, envir=env)
        else assign("Rdescdim", FALSE, envir=env)
        
        if (tclvalue(Fichier)=="") assign("RFichier", NULL, envir=env)
        assign("RFichier", tclvalue(Fichier), envir=env)

        tkdestroy(SortieWin)
      
      }
      
        eig.lab <-tklabel(SortieWin, text=gettextRcmdr("Eigenvalues"))
        eig.check <- tkcheckbutton(SortieWin)
        if(Rpropre) eigValue <- tclVar("1")
        else eigValue <- tclVar("0")
        tkconfigure(eig.check,variable=eigValue)
        
        groupe.lab <-tklabel(SortieWin, text=gettextRcmdr("Results for the variables"))
        groupe.check <- tkcheckbutton(SortieWin)
        if(Rgroupe) groupeValue <- tclVar("1")
        else groupeValue <- tclVar("0")
        tkconfigure(groupe.check,variable=groupeValue)
        
        ind.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for the active individuals"))
        ind.check <- tkcheckbutton(SortieWin)
        if(Rindividu) indValue <- tclVar("1")
        else indValue <- tclVar("0")
        tkconfigure(ind.check,variable=indValue)

        ind.sup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for the supplementary individuals"))
        ind.sup.check <- tkcheckbutton(SortieWin)
        if(Rindsup) ind.sup.Value <- tclVar("1")
        else ind.sup.Value <- tclVar("0")
        tkconfigure(ind.sup.check,variable=ind.sup.Value)
        
#        quantiSummary.lab<-tklabel(SortieWin,text=gettextRcmdr("Summary of the quantitative variables"))
#        quantiSummary.check <- tkcheckbutton(SortieWin)
#        if(Rquantisummary) quantiSummaryValue <- tclVar("1")
#        else quantiSummaryValue <- tclVar("0")
#        tkconfigure(quantiSummary.check,variable=quantiSummaryValue)
      
        quanti.lab<-tklabel(SortieWin,text=gettextRcmdr("Results of the quantitative variables"))
        quanti.check <- tkcheckbutton(SortieWin)
        if(Rquanti) quantiValue <- tclVar("1")
        else quantiValue <- tclVar("0")
        tkconfigure(quanti.check,variable=quantiValue)

        quantisup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results of the supplementary quantitative variables"))
        quantisup.check <- tkcheckbutton(SortieWin)
        if(Rquantisup) quantisupValue <- tclVar("1")
        else quantisupValue <- tclVar("0")
        tkconfigure(quantisup.check,variable=quantisupValue)
      
#        qualiSummary.lab<-tklabel(SortieWin,text=gettextRcmdr("Summary of the qualitative variables"))
#        qualiSummary.check <- tkcheckbutton(SortieWin)
#        if(Rqualisummary) qualiSummaryValue <- tclVar("1")
#        else qualiSummaryValue <- tclVar("0")
#        tkconfigure(qualiSummary.check,variable=qualiSummaryValue)
      
        quali.lab<-tklabel(SortieWin,text=gettextRcmdr("Results of the qualitative variables"))
        quali.check <- tkcheckbutton(SortieWin)
        if(Rquali) qualiValue <- tclVar("1")
        else qualiValue <- tclVar("0")
        tkconfigure(quali.check,variable=qualiValue)

        qualisup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results of the supplementary qualitative variables"))
        qualisup.check <- tkcheckbutton(SortieWin)
        if(Rqualisup) qualisupValue <- tclVar("1")
        else qualisupValue <- tclVar("0")
        tkconfigure(qualisup.check,variable=qualisupValue)
      
      descdim.lab<-tklabel(SortieWin, text=gettextRcmdr("Description of the dimensions"))
      descdim.check<-tkcheckbutton(SortieWin)
      if(Rdescdim) descdimValue<-tclVar("1")
      else descdimValue<-tclVar("0")
      tkconfigure(descdim.check,variable=descdimValue)

      RFichierFrame<-tkframe(SortieWin,borderwidth=2)
      if (is.null(RFichier)) Fichier <- tclVar("")
      else Fichier<-tclVar(RFichier)
      Fichier.entry <-tkentry(RFichierFrame,width="40",textvariable=Fichier)
      tkgrid(tklabel(RFichierFrame,text=gettextRcmdr("Print results on a 'csv' file")),Fichier.entry)

      SortieOK.but<-tkbutton(SortieWin,text="OK",width=16,command=onOK.sortie)

      tkgrid(tklabel(SortieWin, text = gettextRcmdr("Select output options"), fg ="blue"),  columnspan = 2, sticky = "w")
      tkgrid(tklabel(SortieWin, text = " "))
      tkgrid(eig.lab,eig.check,sticky="w")
      tkgrid(groupe.lab,groupe.check,sticky="w")
      tkgrid(ind.lab,ind.check,sticky="w")
      if (!is.null(individuillu)) tkgrid(ind.sup.lab,ind.sup.check,sticky="w")
#      tkgrid(quantiSummary.lab,quantiSummary.check,sticky="w")
      tkgrid(quanti.lab,quanti.check,sticky="w")      
      if (!is.null(variableillu)) tkgrid(quantisup.lab,quantisup.check,sticky="w")
#      tkgrid(qualiSummary.lab,qualiSummary.check,sticky="w")
      tkgrid(quali.lab,quali.check,sticky="w")        
      if (!is.null(variablefact)) tkgrid(qualisup.lab,qualisup.check,sticky="w")
      tkgrid(descdim.lab,descdim.check,sticky="w")
      tkgrid(tklabel(SortieWin, text = " "))
      tkgrid(RFichierFrame)
      tkgrid(SortieOK.but)
      tkgrid(tklabel(SortieWin, text = " "))
   }
    
    SortieFrame<-tkframe(IlluFrame)
    Sortie.but<-tkbutton(SortieFrame, textvariable=.SortieLabel, command=OnSortie, borderwidth=3)
    tkgrid(Sortie.but, sticky="ew")
  })



  #! fonction pour la gestion des options graphiques 
  PLOT.AFDM<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.graph<-0
    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))
    #déclaration des variables
    Gchoix<-TRUE
    GTitle<-NULL
    Gcol.var<-Gcol.var.tmp<-"red"
    Gcol.quanti.sup<-Gcol.quanti.sup.tmp<-"darkred"
    Gcol.quali<-Gcol.quali.tmp<-"green"
    Gcol.quali.sup<-Gcol.quali.sup.tmp<-"darkgreen"
    GAxeGrpe<-c(1,2)
    Glabel<-TRUE
        
    Rchoix<-TRUE
    RTitle<-NULL
    Rlabel.indMoy<-TRUE
    Rlabel.quali<-TRUE    
    Rhabillage<-"none"
    Rinvisible<-NULL
    RXlimInd<-NULL
    RYlimInd<-NULL
    
    Wchoix=TRUE
    WTitle<-NULL
    WAxeVar<-c(1,2)
    Wlabel.var<-TRUE
    Wcol.quanti.sup<-Wcol.quanti.sup.tmp<-"blue"
    Wcol.var<-Wcol.var.tmp<-"black"
    Winvisible<-NULL
    Wlim.cos<-0.
    
        
    OnPlot<-function()
    {
      PlotWin<-tktoplevel()
      tkwm.title(PlotWin,gettextRcmdr("Graphical options"))
      tkwm.geometry(PlotWin, "-100+50")
      PlotWin2<-tkframe(PlotWin)

      #création de la fonction onOKsub
      onOKsub<-function()
      {
        assign("compteur.graph", compteur.graph+1, envir=env)
        if(compteur.graph>0) tclvalue(.PlotLabel)<-paste(label, gettextRcmdr(""), sep=" ")
        tkconfigure(Plot.but, fg="blue")

        # gestion des entrées de la partie graphique des Groupes
        if(tclvalue(grpe.check.value)==1) assign("Gchoix", TRUE, envir=env)
        else assign("Gchoix", FALSE, envir=env)

        if(Gchoix) {
          if (tclvalue(GTitre)==" ") assign("GTitle", NULL, envir=env)
          assign("GTitle", tclvalue(GTitre), envir=env)

          assign("Gcol.var", Gcol.var.tmp, envir=env)
          assign("Gcol.quanti.sup", Gcol.quanti.sup.tmp, envir=env)
          assign("Gcol.quali", Gcol.quali.tmp, envir=env)
          assign("Gcol.quali.sup", Gcol.quali.sup.tmp, envir=env)
          
          label.tmp.grpe<-tclvalue(label.grpe.checkValue)
          if(label.tmp.grpe==1) assign("Glabel", TRUE, envir=env)
          else assign("Glabel", FALSE, envir=env)
        }
                    
        # gestion des entrées de la partie graphique des variables
        if(tclvalue(var.check.value)==1) assign("Wchoix", TRUE, envir=env)
        else assign("Wchoix", FALSE, envir=env)

        if(Wchoix) {
          if (tclvalue(WTitre)==" ") assign("WTitle", NULL, envir=env)
          assign("WTitle", tclvalue(WTitre), envir=env)

          assign("Wlim.cos", tclvalue(WlimCosValue), envir=env)

          label.tmp.var<-tclvalue(label.var.checkValue)
          if(label.tmp.var==1) assign("Wlabel.var", TRUE, envir=env)
          else assign("Wlabel.var", FALSE, envir=env)

          assign("Wcol.var", Wcol.var.tmp, envir=env)
          assign("Wcol.quanti.sup", Wcol.quanti.sup.tmp, envir=env)

          if(tclvalue(inv.Value)=="aucun")  assign("Winvisible", NULL, envir=env)
          else assign("Winvisible", tclvalue(inv.Value), envir=env)
        }

        # gestion des entrées de la partie graphique des individus
        if(tclvalue(ind.check.value)==1) assign("Rchoix", TRUE, envir=env)
        else assign("Rchoix", FALSE, envir=env)

        if(Rchoix) {
          if (tclvalue(Titre)==" ") assign("RTitle", NULL, envir=env)
          assign("RTitle", tclvalue(Titre), envir=env)

          label.tmp.indMoy<-tclvalue(label.indMoy.checkValue)
          label.tmp.quali<-tclvalue(label.quali.checkValue)
          if(label.tmp.indMoy==1) assign("Rlabel.indMoy", TRUE, envir=env)
          else assign("Rlabel.indMoy", FALSE, envir=env)
          if(label.tmp.quali==1) assign("Rlabel.quali", TRUE, envir=env)
          else assign("Rlabel.quali", FALSE, envir=env)
          
          habillage.tmp<-listgraph.nom[as.numeric(tkcurselection(listgraph))+1]
          if(length(habillage.tmp)==0) assign("Rhabillage","none", envir=env)
          else assign("Rhabillage", habillage.tmp, envir=env)

          if(tclvalue(XlimIndMin)=="" | tclvalue(XlimIndMax)=="") assign("RXlimInd", NULL, envir=env)
          else assign("RXlimInd", c(as.numeric(tclvalue(XlimIndMin)), as.numeric(tclvalue(XlimIndMax))), envir=env)
          if(tclvalue(YlimIndMin)=="" | tclvalue(YlimIndMax)=="") assign("RYlimInd", NULL, envir=env)
          else assign("RYlimInd", c(as.numeric(tclvalue(YlimIndMin)), as.numeric(tclvalue(YlimIndMax))), envir=env)
          
          inv.ind.tmp<-tclvalue(inv.ind.checkValue)
          inv.ind.sup.tmp<-tclvalue(inv.ind.sup.checkValue)
          inv.quali.tmp<-tclvalue(inv.quali.checkValue)
          assign("Rinvisible", NULL, envir=env)
          if(inv.ind.tmp=="1") assign("Rinvisible", c(Rinvisible, "ind"), envir=env)
          if(inv.ind.sup.tmp=="1") assign("Rinvisible", c(Rinvisible, "ind.sup"), envir=env)          
          if(inv.quali.tmp=="1") assign("Rinvisible", c(Rinvisible, "quali"), envir=env)          
        }
        tkdestroy(PlotWin)
      }
    
      # création l'interface "options graphiques"
    
      ##########################
      # construction de la partie graphique des Groupes
      PlotGrpeFrame<-tkframe(PlotWin2, borderwidth=5, relief="groove")
  
      GchoixFrame<-tkframe(PlotGrpeFrame,borderwidth=2)
      grpe.check<-tkcheckbutton(GchoixFrame)
      if(Gchoix) grpe.check.value<-tclVar("1")
      else grpe.check.value<-tclVar("0")
      tkconfigure(grpe.check, variable=grpe.check.value)
      tkgrid(tklabel(GchoixFrame, text=gettextRcmdr("Graph of all the variables"), font=font2),grpe.check)
      tkgrid(tklabel(GchoixFrame, text="  "))
  
      GTitleFrame<-tkframe(PlotGrpeFrame,borderwidth=2)
      if (is.null(GTitle)) GTitre <- tclVar(" ")
      else GTitre<-tclVar(GTitle)
      GTitre.entry <-tkentry(GTitleFrame,width="40",textvariable=GTitre)
      tkgrid(tklabel(GTitleFrame,text=gettextRcmdr("Title of the graph")),GTitre.entry)
    
    GcolFrame<-tkframe(PlotGrpeFrame,borderwidth=2)
    Gcol.var.value <- Gcol.var
    canvas.var <- tkcanvas(GcolFrame,width="80",height="25",bg=Gcol.var.value)
    ChangeColor.var <- function()
    {
      Gcol.var.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Gcol.var.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Gcol.var.value)>0) {
        tkconfigure(canvas.var,bg=Gcol.var.value)
        assign("Gcol.var.tmp", Gcol.var.value, envir=env)
      }
    }
    ChangeColor.var.button <- tkbutton(GcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.var)
    tkgrid(tklabel(GcolFrame, text=gettextRcmdr("Color of the quantitative variables")),canvas.var,ChangeColor.var.button)
    
    Gcol.quanti.sup.value<-Gcol.quanti.sup
    canvas.quanti.sup <- tkcanvas(GcolFrame,width="80",height="25",bg=Gcol.quanti.sup.value)
    ChangeColor.quanti.sup <- function()
    {
      Gcol.quanti.sup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Gcol.quanti.sup.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Gcol.quanti.sup.value)>0) {
        tkconfigure(canvas.quanti.sup,bg=Gcol.quanti.sup.value)
        assign("Gcol.quanti.sup.tmp", Gcol.quanti.sup.value, envir=env)
      }
    }
    ChangeColor.quanti.sup.button <- tkbutton(GcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.quanti.sup)
    if(!is.null(variableillu)) tkgrid(tklabel(GcolFrame, text=gettextRcmdr("color for supplementary quantitative variables")),canvas.quanti.sup,ChangeColor.quanti.sup.button)

    Gcol.quali.value<-Gcol.quali
    canvas.quali <- tkcanvas(GcolFrame,width="80",height="25",bg=Gcol.quali.value)
    ChangeColor.quali <- function()
    {
      Gcol.quali.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Gcol.quali.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Gcol.quali.value)>0) {
        tkconfigure(canvas.quali,bg=Gcol.quali.value)
        assign("Gcol.quali.tmp", Gcol.quali.value, envir=env)
      }
    }
    ChangeColor.quali.button <- tkbutton(GcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.quali)
    tkgrid(tklabel(GcolFrame, text=gettextRcmdr("color for qualitative variables")),canvas.quali,ChangeColor.quali.button)

    Gcol.quali.sup.value<-Gcol.quali.sup
    canvas.quali.sup <- tkcanvas(GcolFrame,width="80",height="25",bg=Gcol.quali.sup.value)
    ChangeColor.quali.sup <- function()
    {
      Gcol.quali.sup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Gcol.quali.sup.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Gcol.quali.sup.value)>0) {
        tkconfigure(canvas.quali.sup,bg=Gcol.quali.sup.value)
        assign("Gcol.quali.sup.tmp", Gcol.quali.sup.value, envir=env)
      }
    }
    ChangeColor.quali.sup.button <- tkbutton(GcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.quali.sup)
    if(!is.null(variablefact)) tkgrid(tklabel(GcolFrame, text=gettextRcmdr("color for supplementary qualitative variables")),canvas.quali.sup,ChangeColor.quali.sup.button)

      GlabelFrame<-tkframe(PlotGrpeFrame,borderwidth=2)
      label.grpe.check<-tkcheckbutton(GlabelFrame)
      if (Glabel) label.grpe.checkValue<-tclVar("1")
      else label.grpe.checkValue<-tclVar("0")
      tkconfigure(label.grpe.check, variable=label.grpe.checkValue)
      tkgrid(tklabel(GlabelFrame, text=gettextRcmdr("Draw labels of the variables")),label.grpe.check)
       
      #mise en page des différents frames de PlotGrpeFrame
      tkgrid(GchoixFrame)
      tkgrid(GTitleFrame)
      tkgrid(GcolFrame)
      tkgrid(GlabelFrame)
      tkgrid(tklabel(PlotGrpeFrame, text=" "))
      
      
      ########################
      # construction de la partie graphique des variables
      PlotVarFrame<-tkframe(PlotWin2, borderwidth=5, relief="groove")
  
      WchoixFrame<-tkframe(PlotVarFrame,borderwidth=2)
      var.check<-tkcheckbutton(WchoixFrame)
      if(Wchoix) var.check.value<-tclVar("1")
      else var.check.value<-tclVar("0")
      tkconfigure(var.check, variable=var.check.value)
      tkgrid(tklabel(WchoixFrame, text=gettextRcmdr("Graph of the quantitative variables"), font=font2),var.check)
      tkgrid(tklabel(WchoixFrame, text="  "))
  
      WTitleFrame<-tkframe(PlotVarFrame,borderwidth=2)
      if (is.null(WTitle)) WTitre <- tclVar(" ")
      else WTitre<-tclVar(WTitle)
      WTitre.entry <-tkentry(WTitleFrame,width="40",textvariable=WTitre)
      tkgrid(tklabel(WTitleFrame,text=gettextRcmdr("Title of the graph")),WTitre.entry)
  
      WcosFrame<-tkframe(PlotVarFrame,borderwidth=2)
      WlimCosValue<-tclVar(paste(Wlim.cos))
      WlimCos.entry<-tkentry(WcosFrame, width=5, textvariable=WlimCosValue)
      tkgrid(tklabel(WcosFrame,text=gettextRcmdr("Draw variables with a cos2 >:")),WlimCos.entry)
  
      WlabelFrame<-tkframe(PlotVarFrame,borderwidth=2)
      label.var.check<-tkcheckbutton(WlabelFrame)
      if (Wlabel.var) label.var.checkValue<-tclVar("1")
      else label.var.checkValue<-tclVar("0")
      tkconfigure(label.var.check, variable=label.var.checkValue)
      tkgrid(tklabel(WlabelFrame, text=gettextRcmdr("Draw the labels of the variables")),label.var.check)
      
    WcolFrame<-tkframe(PlotVarFrame,borderwidth=2)
    Wcol.var.value <- Wcol.var
    Wcanvas.var <- tkcanvas(WcolFrame,width="80",height="25",bg=Wcol.var.value)
    WChangeColor.var <- function()
    {
      Wcol.var.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Wcol.var.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Wcol.var.value)>0) {
        tkconfigure(Wcanvas.var,bg=Wcol.var.value)
        assign("Wcol.var.tmp", Wcol.var.value, envir=env)
      }
    }
    WChangeColor.var.button <- tkbutton(WcolFrame,text=gettextRcmdr("Change Color"),command=WChangeColor.var)
    tkgrid(tklabel(WcolFrame, text=gettextRcmdr("Color of the active variables")),Wcanvas.var,WChangeColor.var.button)
    
    Wcol.quanti.sup.value<-Wcol.quanti.sup
    Wcanvas.quanti.sup <- tkcanvas(WcolFrame,width="80",height="25",bg=Wcol.quanti.sup.value)
    WChangeColor.quanti.sup <- function()
    {
      Wcol.quanti.sup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Wcol.quanti.sup.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Wcol.quanti.sup.value)>0) {
        tkconfigure(Wcanvas.quanti.sup,bg=Wcol.quanti.sup.value)
        assign("Wcol.quanti.sup.tmp", Wcol.quanti.sup.value, envir=env)
      }
    }
    WChangeColor.quanti.sup.button <- tkbutton(WcolFrame,text=gettextRcmdr("Change Color"),command=WChangeColor.quanti.sup)
    if(!is.null(variableillu)) tkgrid(tklabel(WcolFrame, text=gettextRcmdr("color for supplementary variables")),Wcanvas.quanti.sup,WChangeColor.quanti.sup.button)
         
      WinvisibleFrame<-tkframe(PlotVarFrame,borderwidth=2)
      inv.aucun.check<-tkradiobutton(WinvisibleFrame)
      inv.act.check<-tkradiobutton(WinvisibleFrame)
      inv.sup.check<-tkradiobutton(WinvisibleFrame)
      if(is.null(Winvisible)) inv.Value<-tclVar("aucun")
      else inv.Value<-tclVar(Winvisible) 
      tkconfigure(inv.aucun.check,variable=inv.Value,value="aucun")
      tkconfigure(inv.act.check,variable=inv.Value, value="actif")
      tkconfigure(inv.sup.check,variable=inv.Value, value="sup")
      tkgrid(tklabel(WinvisibleFrame, text=gettextRcmdr("Hide some elements:")), columnspan=6, sticky="w")
      tkgrid(tklabel(WinvisibleFrame, text="None"),inv.aucun.check, tklabel(WinvisibleFrame, text=gettextRcmdr("active variables")),inv.act.check, tklabel(WinvisibleFrame, text=gettextRcmdr("supplementary variables")),inv.sup.check, sticky="w")
        
      #mise en page des différents frames de PlotVarFrame
      tkgrid(WchoixFrame)
      tkgrid(WTitleFrame)
      tkgrid(WcolFrame)
      tkgrid(WcosFrame)
      tkgrid(WlabelFrame)
      tkgrid(WinvisibleFrame)            
      tkgrid(tklabel(PlotVarFrame, text=" "))
        
      ##########################  
      # construction de la partie graphique des individus
      PlotIndFrame<-tkframe(PlotWin, borderwidth=5, relief="groove")
      
      RchoixFrame<-tkframe(PlotIndFrame,borderwidth=2)
      ind.check<-tkcheckbutton(RchoixFrame)
      if(Rchoix) ind.check.value<-tclVar("1")
      else ind.check.value<-tclVar("0")
      tkconfigure(ind.check, variable=ind.check.value)
      tkgrid(tklabel(RchoixFrame, text=gettextRcmdr("Graph of the individuals"), font=font2),ind.check)
      tkgrid(tklabel(RchoixFrame, text=" "))
      
      RTitleFrame<-tkframe(PlotIndFrame,borderwidth=2)
      if (is.null(RTitle)) Titre <- tclVar(" ")
      else Titre<-tclVar(RTitle)
      Titre.entry <-tkentry(RTitleFrame,width="40",textvariable=Titre)
      tkgrid(tklabel(RTitleFrame,text=gettextRcmdr("Title of the graph")),Titre.entry)
      
      RlabelFrame<-tkframe(PlotIndFrame,borderwidth=2)
      label.indMoy.check<-tkcheckbutton(RlabelFrame)
      if (Rlabel.indMoy) label.indMoy.checkValue<-tclVar("1")
      else label.indMoy.checkValue<-tclVar("0") 
      tkconfigure(label.indMoy.check, variable=label.indMoy.checkValue)
      tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Draw labels for the mean individuals")),label.indMoy.check)
      label.quali.check<-tkcheckbutton(RlabelFrame)
      if (Rlabel.quali) label.quali.checkValue<-tclVar("1")
      else label.quali.checkValue<-tclVar("0")
      tkconfigure(label.quali.check, variable=label.quali.checkValue)
      tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Draw labels for the qualitative variables")), label.quali.check)
              
      RhabillageFrame<-tkframe(PlotIndFrame,borderwidth=2)
      listgraph<-tklistbox(RhabillageFrame,height=4, selectmode="single",exportselection="FALSE",yscrollcommand=function(...) tkset(scrgraph,...))
      scrgraph <-tkscrollbar(RhabillageFrame,repeatinterval=5,command=function(...)tkyview(listgraph,...))
      listgraph.nom<-c("ind")
      tkinsert(listgraph,"end","by.individual")
      if(Rhabillage=="ind") tkselection.set(listgraph,0)
      indice<-1      
      nbauxli<-c(tclvalue(tkcurselection(listfact)))
      nbaux<-unlist(strsplit(nbauxli,"\\ "))
      varaux = vars.fact[as.numeric(tkcurselection(listfact))+1]
      
      if (!is.null(variablefact)|(length(nbaux)>0)){
        for (j in 1:ncol(donnee)){
         if(vars[j] %in% c(variablefact,varaux)){
          tkinsert(listgraph,"end",vars[j])
          listgraph.nom<-c(listgraph.nom,vars[j])
          if(Rhabillage==vars[j]) tkselection.set(listgraph, indice)
          indice<-indice+1
        }}
      }
      tkgrid(tklabel(RhabillageFrame, text=gettextRcmdr("Select drawing for the individuals")))
      tkgrid(listgraph, scrgraph, sticky = "nw")
      tkgrid.configure(scrgraph, sticky = "wns")
      tkgrid.configure(listgraph, sticky = "ew")
      
      
      RinvisibleFrame<-tkframe(PlotIndFrame,borderwidth=2)
      inv.ind.check<-tkcheckbutton(RinvisibleFrame)
      if ("ind" %in% Rinvisible) inv.ind.checkValue<-tclVar("1")
      else inv.ind.checkValue<-tclVar("0") 
      inv.ind.sup.check<-tkcheckbutton(RinvisibleFrame)
      if ("ind.sup" %in% Rinvisible) inv.ind.sup.checkValue<-tclVar("1")
      else inv.ind.sup.checkValue<-tclVar("0") 
      inv.quali.check<-tkcheckbutton(RinvisibleFrame)      
      if ("quali" %in% Rinvisible) inv.quali.checkValue<-tclVar("1")
      else inv.quali.checkValue<-tclVar("0") 
      tkconfigure(inv.ind.check, variable=inv.ind.checkValue)
      tkconfigure(inv.ind.sup.check, variable=inv.ind.sup.checkValue)
      tkconfigure(inv.quali.check, variable=inv.quali.checkValue)            
      tkgrid(tklabel(RinvisibleFrame, text=gettextRcmdr("Hide some elements:")), columnspan=6, sticky="w")
      tkgrid(tklabel(RinvisibleFrame, text="ind"),inv.ind.check, tklabel(RinvisibleFrame, text="ind sup"),inv.ind.sup.check, tklabel(RinvisibleFrame, text="quali"),inv.quali.check, sticky="w")
      
            
      RlimFrame<-tkframe(PlotIndFrame,borderwidth=2)
      if(is.null(RXlimInd)) XlimIndMin<-tclVar("")
      else XlimIndMin<-tclVar(paste(RXlimInd[1]))
      XlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMin)
      if (is.null(RXlimInd)) XlimIndMax<- tclVar("")
      else XlimIndMax<-tclVar(paste(RXlimInd[1]))
      XlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMax)
      tkgrid(tklabel(RlimFrame,text=gettextRcmdr("x limits of the graph:")),XlimIndMin.entry, XlimIndMax.entry)
      if(is.null(RYlimInd)) YlimIndMin<- tclVar("")
      else YlimIndMin<-tclVar(paste(RYlimInd[1]))
      YlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMin)
      if (is.null(RYlimInd)) YlimIndMax<- tclVar("")
      else YlimIndMax<-tclVar(paste(RYlimInd[2]))
      YlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMax)
      tkgrid(tklabel(RlimFrame,text=gettextRcmdr("y limits of the graph:")),YlimIndMin.entry,YlimIndMax.entry)
  
  
      #mise en page des différents frames de PlotIndFrame
      tkgrid(RchoixFrame)
      tkgrid(RTitleFrame)
      tkgrid(RlabelFrame)
      tkgrid(RinvisibleFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))
      tkgrid(RhabillageFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))      
      tkgrid(RlimFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))
      
              
      #mise en page de plotWin
      subOKCancelHelp(PlotWin, "plot.AFDM")
      tkgrid(PlotGrpeFrame)
      tkgrid(PlotVarFrame)
      tkgrid(PlotIndFrame, PlotWin2, sticky="ns")
      tkgrid(subButtonsFrame, sticky="ew", columnspan=2)
    }

    PlotFrame<-tkframe(IlluFrame)
    Plot.but<-tkbutton(PlotFrame, textvariable=.PlotLabel, command=OnPlot, borderwidth=3)
    tkgrid(Plot.but, sticky="ew")
  }) 



    #! fonction associée au bouton Appliquer, execute sans détruire l'interface graphique
  OnAppliquer<-function()
  {
      #liste de l'ensemble des variables créées
      #sur la fenêtre top
#      listQuantiAct
#      listQuantiIllu
#      listQualiAct
#      listQualiIllu
#      resu.val
#      ncp.val
      #pour les individus illustratifs
#      individuillu
      #pour l'affichage
#      Rpropre
#          Rgroupe
#          Rindividu
#        Rindsup
#      Rquantisummary
#      Rquanti
#      Rquantisup  
#          Rqualisummary
#          Rquali
#          Rqualisup
#          Rdescdim
      # pour les graphiques
#      Gchoix
#      GTitle
#      GAxeGrpe
#      Glabel
#        
#      Rchoix
#      RTitle
#      Rlabel.indMoy
#      Rlabel.quali  
#      Rhabillage
#      Rinvisible
#      Rpartial
#      RpartialSouris
#      Rchrono
#      RXlimInd
#      RYlimInd
#    
#      Wchoix
#      WTitle
#      WAxeVar
#      Wlabel.var
#      Winvisible
#      Wlim.cos
#
#      Axe 
  
    # récupération des paramètres de la fenêtre principale
    nom.res<-tclvalue(resu.val)
    if (length(ls(pat=nom.res))>0) justDoIt(paste('remove (',nom.res,')'))
    ncp<-as.numeric(tclvalue(ncp.val))
    #reduction<-TRUE
    #if(tclvalue(reduitValue)=="0") reduction<-FALSE
    Axe<-c(as.numeric(tclvalue(Axe1)), as.numeric(tclvalue(Axe2)))
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    nbitemlist.q<-c(tclvalue(tkcurselection(listfact)))
    nbitem.q<-unlist(strsplit(nbitemlist.q,"\\ "))

 
    # gestion du tableau de données pour l'AFDM
    variables <- variables.q <- NULL
    if (length(nbitem)>0) variables <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
    if (length(nbitem.q)>0) variables.q <- vars.fact[as.numeric(tkcurselection(listfact))+1]
    allvariables = c(variables,variables.q,variableillu,variablefact)
    num.group.sup<-NULL
    if (length(variableillu)+length(variablefact)>0) num.group.sup <- ((length(variables)+length(variables.q)+1):length(allvariables))
    #construction du tableau de données.AFDM
      if(!is.null(individuillu)) {
        ind.actif<-rows[-which(rows %in% individuillu)]
        commande.data<-paste(activeDataSet(),'.AFDM', '<-', activeDataSet(),'[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '"),', sep='')
      }
      else commande.data<-paste(activeDataSet(),'.AFDM', '<-', activeDataSet(),'[,', sep='')
      commande.data<-paste(commande.data,' c("',paste(allvariables, collapse='", "'), '")]',sep='')
      
      justDoIt(commande.data)
      logger(commande.data)
      donnee.depart<-activeDataSet()
      activeDataSet(paste(activeDataSet(),'.AFDM', sep=""))

      # gestion de la commande réalisant l'AFM     
      commande.AFDM<-paste(nom.res, '<-AFDM(', activeDataSet(),sep='')
      if(!is.null(individuillu)) commande.AFDM<-paste(commande.AFDM, ', ind.sup=', nrow(get(.activeDataSet))-length(individuillu)+1, ': ', nrow(get(.activeDataSet)),sep='')
#      commande.AFDM<-paste(commande.AFDM, ', type=c(',sep='')
#      if (!is.null(variables)){
#        if (reduction) commande.AFDM<-paste(commande.AFDM, 'rep("s",',length(variables),')',sep='')
#        else commande.AFDM<-paste(commande.AFDM, 'rep("c",',length(variables),')',sep='')
#        if (!is.null(variables.q)) commande.AFDM<-paste(commande.AFDM, ', ',sep='')
#      }
#      if (!is.null(variables.q)) commande.AFDM<-paste(commande.AFDM, 'rep("n",',length(variables.q),')',sep='')
#      if (!is.null(variableillu)){
#        if (reduction) commande.AFDM<-paste(commande.AFDM, ', rep("s",',length(variableillu),')',sep='')
#        else commande.AFDM<-paste(commande.AFDM, ', rep("c",',length(variableillu),')',sep='')
#      }
#      if (!is.null(variablefact)) commande.AFDM<-paste(commande.AFDM, ', rep("n",',length(variablefact),')',sep='')
#      else commande.AFDM<-paste(commande.AFDM, '), ncp=', ncp,sep='')
      commande.AFDM<-paste(commande.AFDM, ', ncp=', ncp,sep='')
      if (!is.null(num.group.sup)) commande.AFDM<-paste(commande.AFDM, ', sup.var=',num.group.sup[1],':',num.group.sup[length(num.group.sup)],sep='')
      commande.AFDM<-paste(commande.AFDM, ', graph=FALSE)',sep='')
      justDoIt(commande.AFDM)
      logger(commande.AFDM)

      #gestion des graphiques   
      if (length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0) {if (get(nom.res)$eig[1,2]==100) doItAndPrint(paste('"No graph can be plot: data are unidimensional"'))}
      if((Gchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)){
      if (get(nom.res)$eig[1,2]!=100) {
        commande.plotG<-paste('plot.AFDM(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="group", lab.grpe=', Glabel, sep="")
        commande.plotG <- paste(commande.plotG, ',col.hab = c(',sep='')
        auxi = 0
        if (length(variables)>0){
          commande.plotG <- paste(commande.plotG, 'rep("',Gcol.var,'",',length(variables),')',sep='')
          auxi = 1
        }
        if (length(variableillu)>0){
          if (auxi==1) commande.plotG <- paste(commande.plotG, ',',sep='')
          commande.plotG <- paste(commande.plotG, 'rep("',Gcol.quanti.sup,'",',length(variableillu),')',sep='')
        }
        auxi=0
        if (length(variables.q)>0){
          commande.plotG <- paste(commande.plotG, ',rep("',Gcol.quali,'",',length(variables.q),')',sep='')
          auxi=1
        }
        if (length(variablefact)>0){
          if (auxi==1) commande.plotG <- paste(commande.plotG, ',',sep='')
          commande.plotG <- paste(commande.plotG, 'rep("',Gcol.quali.sup,'",',length(variablefact),')',sep='')
        }
        commande.plotG <- paste(commande.plotG, ')',sep='')
        if (is.null(GTitle)) commande.plotG <- paste(commande.plotG,')', sep="")
        else {
          if (GTitle ==" ") commande.plotG <- paste(commande.plotG,')', sep="")
          else commande.plotG <- paste(commande.plotG,', title="', GTitle,'")', sep="")
        }
        justDoIt(commande.plotG)
        logger(commande.plotG)
      }}
            
      if((Wchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)&(length(variables)>0)){
      if (get(nom.res)$eig[1,2]!=100) {
        commande.plotW<-paste('plot.AFDM(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="var", lab.var=', Wlabel.var, ', lim.cos2.var=', Wlim.cos, sep="")

        if (!is.null(Winvisible)) {
          commande.plotW<-paste(commande.plotW, ', invisible=c("', paste(Winvisible, collapse='", "'),'")', sep='')
          if(Winvisible=="actif") commande.plotW<-paste(commande.plotW, ', col.hab=c(rep("', Wcol.quanti.sup,'",length(rownames(', nom.res, '$quanti.var.sup[[1]]))),rep("', Wcol.var, '",length(rownames(', nom.res,'$quanti.var[[1]]))))', sep='')
          else commande.plotW<-paste(commande.plotW, ', col.hab=c(rep("', Wcol.var, '",length(rownames(', nom.res,'$quanti.var[[1]]))),rep("', Wcol.quanti.sup,'",length(rownames(', nom.res, '$quanti.var.sup[[1]]))))', sep='')
        }
        if(is.null(Winvisible)) commande.plotW<-paste(commande.plotW, ', col.hab=c(rep("', Wcol.var, '",length(rownames(', nom.res,'$quanti.var[[1]]))),rep("', Wcol.quanti.sup,'",length(rownames(', nom.res, '$quanti.var.sup[[1]]))))', sep='')

        if (is.null(WTitle)) commande.plotW <- paste(commande.plotW,')', sep="")
        else {
          if (WTitle ==" ") commande.plotW <- paste(commande.plotW,')', sep="")
          else commande.plotW <- paste(commande.plotW,', title="', WTitle,'")', sep="")
        }
        justDoIt(commande.plotW)
        logger(commande.plotW)
      }}
      
      if((Rchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)){
      if (get(nom.res)$eig[1,2]!=100) {
        if ((Rhabillage!="none") & (Rhabillage!="ind")) {
          Rhabillage<-which(colnames(get(.activeDataSet))==Rhabillage)
          if(length(Rhabillage)==0) Rhabillage<-"none"
        }
        if (Rhabillage=="none") Rhabillage<-paste('"', Rhabillage, '"', sep="")
        if (Rhabillage=="ind") Rhabillage<-paste('"', Rhabillage, '"', sep="")
        
          commande.plotI<-paste('plot.AFDM(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="ind", lab.ind=', Rlabel.indMoy, ', lab.var=', Rlabel.quali, ', habillage=', Rhabillage, sep="") 
          if (!is.null(RXlimInd)) commande.plotI<-paste(commande.plotI, ', xlim=c(', paste(RXlimInd, collapse=", "), ')', sep='')
          if (!is.null(RYlimInd)) commande.plotI<-paste(commande.plotI, ', ylim=c(', paste(RYlimInd, collapse=", "), ')', sep='')
          if (!is.null(Rinvisible)) commande.plotI<-paste(commande.plotI, ', invisible=c("', paste(Rinvisible, collapse='", "'),'")', sep='')
          if (is.null(RTitle)) commande.plotI <- paste(commande.plotI,')', sep="")
          else {
            if (RTitle ==" ") commande.plotI <- paste(commande.plotI,')', sep="")
            else commande.plotI <- paste(commande.plotI,', title="', RTitle,'")', sep="")
          }
        }
        justDoIt(commande.plotI)
        logger(commande.plotI)
      }
      
      # gestion de l'édition de certains resultats
    if (RFichier==""){
      if(Rpropre) doItAndPrint(paste( nom.res, '$eig', sep=""))
      if(Rgroupe) doItAndPrint(paste( nom.res, '$group', sep=""))
      if(Rindividu) doItAndPrint(paste( nom.res, '$ind', sep=""))
      if(Rindsup) doItAndPrint(paste( nom.res, '$ind.sup', sep=""))
#      if(Rquantisummary) doItAndPrint(paste( nom.res, '$summary.quanti', sep=""))
      if(Rquanti) doItAndPrint(paste( nom.res, '$quanti.var', sep=""))      
      if(Rquantisup) doItAndPrint(paste( nom.res, '$quanti.var.sup', sep=""))
#      if(Rqualisummary) doItAndPrint(paste( nom.res, '$summary.quali', sep=""))
      if(Rquali) doItAndPrint(paste( nom.res, '$quali.var', sep=""))      
      if(Rqualisup) doItAndPrint(paste( nom.res, '$quali.var.sup', sep=""))
      if(Rdescdim) doItAndPrint(paste('dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '))', sep=""))
    }
    else {
      Fich = RFichier
      if (substr(Fich,1,1)!='"') Fich = paste('"',Fich,sep='')
      if (substr(Fich,nchar(Fich),nchar(Fich))!='"') Fich = paste(Fich,'"',sep='')
      append = FALSE
      if(Rpropre){
        doItAndPrint(paste('write.infile(', nom.res, '$eig, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rgroupe){
        doItAndPrint(paste('write.infile(', nom.res, '$group, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rindividu){
        doItAndPrint(paste('write.infile(', nom.res, '$ind, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rindsup){
        doItAndPrint(paste('write.infile(', nom.res, '$ind.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
#      if(Rquantisummary){
#        doItAndPrint(paste('write.infile(', nom.res, '$summary.quanti, file =',Fich,',append=',append,')', sep=""))
#        append = TRUE
#      }
      if(Rquanti){
        doItAndPrint(paste('write.infile(', nom.res, '$quanti.var, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rquantisup){
        doItAndPrint(paste('write.infile(', nom.res, '$quanti.var.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
#      if(Rqualisummary){
#        doItAndPrint(paste('write.infile(', nom.res, '$summary.quali, file =',Fich,',append=',append,')', sep=""))
#        append = TRUE
#      }
      if(Rquali){
        doItAndPrint(paste('write.infile(', nom.res, '$quali.var, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rqualisup){
        doItAndPrint(paste('write.infile(', nom.res, '$quali.var.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rdescdim) doItAndPrint(paste('write.infile(dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), ')), file =',Fich,',append=',append,')', sep=""))
    }

      # Re-chargement du tableau de départ et supression du tableau temporaire
      activeDataSet(donnee.depart)
      justDoIt(paste('remove(',activeDataSet(),'.AFDM)',sep=""))
      logger(paste('remove(',activeDataSet(),'.AFDM)',sep=""))   
  }


    #! fonction associée au bouton OK, execute et détruit l'interface graphique
  onOK<-function()
  {
    OnAppliquer()
    tkdestroy(top)     
  }



#                   Création de la fenêtre top                                 #
################################################################################
##  top<-tktoplevel(borderwidth=10)
##  tkwm.title(top,gettextRcmdr("AFDM"))
##  tkwm.geometry(top, "-50+50")
##  
##  # définition des polices
##  font2<-tkfont.create(family="times",size=12,weight="bold")
##  fontheading<-tkfont.create(family="times",size=11,weight="bold")
##
##  # récupération du jeu de données actif
##  donnee<-get(.activeDataSet)
##  vars<-colnames(donnee)
##  rows<-rownames(donnee)
  

   # création de tous les boutons d'options dans IlluFrame
  IlluFrame<- tkframe(top, borderwidth=2)
  Reinitializ.but<-tkbutton(IlluFrame, text="Restart",width=18,command=Reinitializ.funct, borderwidth=3)


    # mise en page de IlluFrame
  Fillu.funct(label=gettextRcmdr("Modify supplementary factors"), firstLabel=gettextRcmdr("Select supplementary factors"))
  Dillu.funct(label=gettextRcmdr("Modify supplementary variables"), firstLabel=gettextRcmdr("Select supplementary variables"))
  Iillu.funct(label=gettextRcmdr("Modify supplementary individuals"), firstLabel=gettextRcmdr("Select supplementary individuals"))    
  PLOT.AFDM(label=gettextRcmdr("Graphical options"), firstLabel=gettextRcmdr("Graphical options"))
  Sortie.funct(label=gettextRcmdr("Outputs"), firstLabel=gettextRcmdr("Outputs"))
  tkgrid(DilluFrame, FilluFrame, IilluFrame, columnspan=7)
  tkgrid(tklabel(IlluFrame, text=""))
  tkgrid(PlotFrame, SortieFrame, Reinitializ.but, columnspan=7)

  tkgrid.configure(DilluFrame, column=1, columnspan=1)
  tkgrid.configure(PlotFrame, column=1, columnspan=1)
  tkgrid.configure(FilluFrame, column=3, columnspan=1)
  tkgrid.configure(SortieFrame, column=3, columnspan=1)
  tkgrid.configure(IilluFrame, column=5, columnspan=1)
  tkgrid.configure(Reinitializ.but, column=5, columnspan=1)      
  tkgrid.columnconfigure(IlluFrame,0, minsize=25)
  tkgrid.columnconfigure(IlluFrame,2, minsize=40)  
  tkgrid.columnconfigure(IlluFrame,4, minsize=25)  


    # création des options dans OptionFrame  
  OptionFrame<-tkframe(top, borderwidth=2, relief="groove")
  resu.lab<-tklabel(OptionFrame,text=gettextRcmdr("Name of the result object: "))
  resu.val<-tclVar("res")
  resu<-tkentry(OptionFrame,width=10,textvariable=resu.val)
#  reduit.lab<-tklabel(OptionFrame,text=gettextRcmdr("Scale the quantative variables: "))
#  reduit.check <- tkcheckbutton(OptionFrame)
#  reduitValue <- tclVar("1")
#  tkconfigure(reduit.check,variable=reduitValue)
  ncp.lab<-tklabel(OptionFrame,text=gettextRcmdr("Number of dimensions: "))
  ncp.val<-tclVar("5") 
  ncp<-tkentry(OptionFrame,width=5,textvariable=ncp.val)
  Axe.label<-tklabel(OptionFrame,text=gettextRcmdr("Select the dimensions for the graphs:"))
  Axe1<-tclVar("1")
  Axe2<-tclVar("2")
  Axe1.entry <-tkentry(OptionFrame,width="5",textvariable=Axe1)
  Axe2.entry <-tkentry(OptionFrame,width="5",textvariable=Axe2)
  
    # mise en page de OptionFrame
  tkgrid(tklabel(OptionFrame,text=gettextRcmdr("Main options"), fg = "darkred"), columnspan=8, sticky="we") 
  tkgrid(tklabel(OptionFrame,text="")) 
#  tkgrid(reduit.lab,reduit.check)
  tkgrid(ncp.lab, ncp)
  tkgrid(Axe.label,Axe1.entry , Axe2.entry, sticky="w")
  tkgrid(resu.lab, resu)
# tkgrid.configure(ncp.lab, reduit.lab, resu.lab, Axe.label, column=1, columnspan=4, sticky="w")
# tkgrid.configure(ncp, resu, reduit.check, column=6, columnspan=2, sticky="e")
  tkgrid.configure(ncp.lab, resu.lab, Axe.label, column=1, columnspan=4, sticky="w")
  tkgrid.configure(ncp, resu, column=6, columnspan=2, sticky="e")
  tkgrid.configure(Axe1.entry, column=6, columnspan=1, sticky="w")
  tkgrid.configure(Axe2.entry, column=7, columnspan=1, sticky="e")
  tkgrid.columnconfigure(OptionFrame,0, minsize=25)
  tkgrid.columnconfigure(OptionFrame,5, minsize=40)
  tkgrid.columnconfigure(OptionFrame,8, minsize=25)

  appliquer.but<-tkbutton(top, text="Apply",width=12,command=OnAppliquer, borderwidth=3, fg="#690f96")
  OKCancelHelp(helpSubject="AFDM")

  # Mise en page de top
  tkgrid(tklabel(top, text=gettextRcmdr("Factor Analysis for Mixed Data (AFDM)"),font=fontheading), columnspan=3)
  tkgrid(tklabel(top,text=""))
  tkgrid(listFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text=""))
  tkgrid(IlluFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text=""))    
  tkgrid(OptionFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text=""))   
  tkgrid(appliquer.but, column=1, columnspan=1)
  tkgrid(tklabel(top,text=""))
  tkgrid(buttonsFrame, column=1, columnspan=1, sticky="ew" )  
}  

#############################FIN FONCTION FactoAFDM ##############################

################################################################################
#                              FONCTION FactoDMFA                              #
################################################################################
#! version JMA du 27/12/2006 17:10:39 

FactoDMFA<-function()
{
  require(tcltk)
  require(FactoMineR)

# Fonction pour la gestion des noms ############################################

  nom.correct<-function(text, liste=NULL)
  {
    text<-chartr("^\ ", "...", text)
    if(!is.null(liste)) {
      while(text %in% liste) text<-paste(text, ".bis", sep="")
    }
    return(text)  
  }
################################################################################

#    Création des fonctions pour les options via nouvelle fenêtre graphique   

  top<-tktoplevel(borderwidth=10)
  tkwm.title(top,gettextRcmdr("DMFA"))
  tkwm.geometry(top, "-50+50")
  
  # définition des polices
  font2<-tkfont.create(family="times",size=12,weight="bold")
  fontheading<-tkfont.create(family="times",size=11,weight="bold")

  # récupération du jeu de données actif
  donnee<-get(.activeDataSet)
  vars<-colnames(donnee)
  rows<-rownames(donnee)

    listFrame <- tkframe(top,borderwidth=2)                                                                                                          
    lab1 = tklabel(listFrame,text=paste("Select quantitative variables"),fg="blue")
    lab2 = tklabel(listFrame,text="Select the group variable",fg="blue")
    lab3 = tklabel(listFrame,text="      ")
    tkgrid(lab1,lab3,lab2)
    tkgrid.configure(lab1,column=1, columnspan=2, sticky = "nw")
    tkgrid.configure(lab2,column=4, columnspan=2, sticky = "ne")
    tkgrid.configure(lab3,column=3, columnspan=1, sticky = "n")

    listdesc<-tklistbox(listFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(listFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    tkselection.set(listdesc,0)
    listfact<-tklistbox(listFrame,selectmode="single",exportselection=FALSE,yscrollcommand=function(...) tkset(scrfact,...))
    scrfact <- tkscrollbar(listFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
    vars<-colnames(donnee)
    vars.fact = NULL
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
      else {
        vars.fact = c(vars.fact,vars[i])
        tkinsert(listfact,"end",vars[i])
      }
    }

    tkgrid(listdesc, scr,tklabel(listFrame,text="                                  "),listfact,scrfact,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns", column=2, columnspan=1)
    tkgrid.configure(listdesc,sticky = "ew", column=1, columnspan=1)
    tkgrid.configure(scrfact,sticky = "wns", column=5, columnspan=1)
    tkgrid.configure(listfact,sticky = "ew", column=4, columnspan=1)

 
  Fillu.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    variablefact<-NULL
    .FilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    .factors<-Factors()
    OnFillu<-function()
    {
      if(length(.factors)==0) errorCondition(recall=NULL, message=gettextRcmdr("No Factor available"))
          
      FilluWin<-tktoplevel()
      tkwm.title(FilluWin,gettextRcmdr("Choice of supplementary factors"))
      #création de la fonction FOK.funct
      FOK.funct<-function()
      {
        fact.select<-listfact.nom[as.numeric(tkcurselection(listfact))+1]
        if(length(fact.select)==0) {
          assign("variablefact", NULL, envir=env)
          tclvalue(.FilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Fillu.but, fg="black")
          tkdestroy(FilluWin)
          return()
        }
        assign("variablefact", fact.select, envir=env)
        tclvalue(.FilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Fillu.but, fg="blue")
        tkdestroy(FilluWin)
      }
      
      # création et mise en page de la fenetre Fillu
      listfact<-tklistbox(FilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrfact,...)) # Liste vide
      scrfact <-tkscrollbar(FilluWin,repeatinterval=5,command=function(...)tkyview(listfact,...))
      listfact.nom<-NULL
      indice<-0
      for (i in (1:ncol(donnee))) {
        if (is.factor(donnee[,i])) {
          tkinsert(listfact,"end",vars[i]) # On renseigne la liste
          listfact.nom<-c(listfact.nom,vars[i])
          if(vars[i] %in% variablefact) tkselection.set(listfact, indice)
          indice<-indice+1
        }
      }
  
      FOK.but<-tkbutton(FilluWin, text="OK", width=16,command=FOK.funct)

      tkgrid(tklabel(FilluWin, text=""))
      tkgrid(tklabel(FilluWin, text = gettextRcmdr("Select supplementary factor(s)"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
      tkgrid(listfact, scrfact, sticky = "nw")
      tkgrid.configure(scrfact, sticky = "ens", columnspan=1)
      tkgrid.configure(listfact, sticky = "ew", column=1, columnspan=1)
      tkgrid(tklabel(FilluWin, text=""))
      tkgrid(FOK.but, column=1,columnspan=1, sticky="ew")
      tkgrid(tklabel(FilluWin, text=""))
      tkgrid.columnconfigure(FilluWin,0, minsize=25)
      tkgrid.columnconfigure(FilluWin,2, minsize=25)
  }  

   FilluFrame<-tkframe(IlluFrame)
   if(length(.factors)==0){
     Fillu.but<-tkbutton(FilluFrame, text=gettextRcmdr("No factors available"), borderwidth=3)
     tkconfigure(Fillu.but, fg="grey")
   }
   else Fillu.but<-tkbutton(FilluFrame, textvariable=.FilluLabel, command=OnFillu, borderwidth=3)
   tkgrid(Fillu.but, sticky="ew")

##   Fillu.but<-tkbutton(FilluFrame, textvariable=.FilluLabel, command=OnFillu, borderwidth=3)
##   tkgrid(Fillu.but, sticky="ew")
  })

  #! fonction pour le choix des variables quantitatives supplémentaires 
  Dillu.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    variableillu<-NULL
    .DilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnDillu<-function()
    { 
      DilluWin<-tktoplevel()
      tkwm.title(DilluWin,gettextRcmdr("Select supplementary quantitative variables"))
      #création de la fonction DOK.funct
      DOK.funct<-function()
      {
        vsup.select<-listvar.nom[as.numeric(tkcurselection(listvar))+1]
        if(length(vsup.select)==0)
        {
          assign("variableillu", NULL, envir=env)
          tclvalue(.DilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Dillu.but, fg="black")
          tkdestroy(DilluWin)
          return()
        }
        assign("variableillu", vsup.select, envir=env)
        tclvalue(.DilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Dillu.but, fg="blue")
        tkdestroy(DilluWin)
      }
      
      # création et mise en page de la fenetre Dillu
      listvar<-tklistbox(DilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrvar,...)) # Liste vide
      scrvar <-tkscrollbar(DilluWin,repeatinterval=5,command=function(...)tkyview(listvar,...)) 
      listvar.nom<-NULL
      indice<-0
      for (i in (1:ncol(donnee))) {
          if (is.numeric(donnee[,i])) {
            tkinsert(listvar,"end",vars[i]) # On renseigne la liste
            listvar.nom<-c(listvar.nom,vars[i])
            if(vars[i] %in% variableillu) tkselection.set(listvar, indice)
            indice<-indice+1
          }
      }
  
      DOK.but<-tkbutton(DilluWin, text="OK", width=16,command=DOK.funct)

      tkgrid(tklabel(DilluWin, text=""))
        tkgrid(tklabel(DilluWin, text = gettextRcmdr("Select supplementary quantitative variables"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
        tkgrid(listvar, scrvar, sticky = "nw")
        tkgrid.configure(scrvar, sticky = "ens", columnspan=1)
        tkgrid.configure(listvar, sticky = "ew", column=1, columnspan=1)
        tkgrid(tklabel(DilluWin, text=""))
        tkgrid(DOK.but, column=1,columnspan=1, sticky="ew")
        tkgrid(tklabel(DilluWin, text=""))
        tkgrid.columnconfigure(DilluWin,0, minsize=25)
      tkgrid.columnconfigure(DilluWin,2, minsize=25)
  }  

   DilluFrame<-tkframe(IlluFrame)
   if(length(listNumeric())==0){
     Dillu.but<-tkbutton(DilluFrame, text=gettextRcmdr("No quantitative variable available"), borderwidth=3)
     tkconfigure(Dillu.but, fg="grey")
   }
   else Dillu.but<-tkbutton(DilluFrame, textvariable=.DilluLabel, command=OnDillu, borderwidth=3)
   tkgrid(Dillu.but, sticky="ew")
  })
  
  #! fonction pour le choix des individus supplémentaires 
  Iillu.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    individuillu<-NULL
    .IilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnIillu<-function()
    {   
      IilluWin<-tktoplevel()
      tkwm.title(IilluWin,gettextRcmdr("Select supplementary individuals"))
      #création de la fonction IOK.funct
      IOK.funct<-function()
      {
        ind.select<-rows[as.numeric(tkcurselection(listind))+1]
        if(length(ind.select)==0) {
          assign("individuillu", NULL, envir=env)
          tclvalue(.IilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Iillu.but, fg="black")
          tkdestroy(IilluWin)
          return()
        }
        assign("individuillu", ind.select, envir=env)
        tclvalue(.IilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Iillu.but, fg="blue")
        tkdestroy(IilluWin)
      }
      
      # création et mise en page de la fenetre Fillu
      listind<-tklistbox(IilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrind,...)) # Liste vide
      scrind <-tkscrollbar(IilluWin,repeatinterval=5,command=function(...)tkyview(listind,...)) 
      indice<-0
      for (i in (1:nrow(donnee))) {
        tkinsert(listind,"end",rows[i]) # On renseigne la liste
        if(rows[i] %in% individuillu) tkselection.set(listind, indice)
        indice<-indice+1
      }
  
      IOK.but<-tkbutton(IilluWin, text="OK", width=16,command=IOK.funct)

      tkgrid(tklabel(IilluWin, text=""))
      tkgrid(tklabel(IilluWin, text = gettextRcmdr("Select supplementary individuals"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
      tkgrid(listind, scrind, sticky = "nw")
      tkgrid.configure(scrind, sticky = "ens", columnspan=1)
      tkgrid.configure(listind, sticky = "ew", column=1, columnspan=1)
      tkgrid(tklabel(IilluWin, text=""))
      tkgrid(IOK.but, column=1,columnspan=1, sticky="ew")
      tkgrid(tklabel(IilluWin, text=""))
      tkgrid.columnconfigure(IilluWin,0, minsize=25)
      tkgrid.columnconfigure(IilluWin,2, minsize=25)
  }  

   IilluFrame<-tkframe(IlluFrame)
   Iillu.but<-tkbutton(IilluFrame, textvariable=.IilluLabel, command=OnIillu, borderwidth=3)
   tkgrid(Iillu.but, sticky="ew")
  })
    
  
    #! fonction pour la réinitialisation des paramètres
  Reinitializ.funct<-function()
  {
    tkdestroy(top)
    FactoDMFA()
  }


  #! fonction pour le choix des éléments de sortie
  Sortie.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.sortie<-0
    #déclaration des variables
    Rpropre<-FALSE
    RFichier <- ""
    Rgroupe<-FALSE
    Rindividu<-FALSE
    RXc<-FALSE
    Rvar.partiel<-FALSE
    Rquanti<-FALSE
    Rquantisup<-FALSE    
    RCov<-FALSE
    Rquali<-FALSE
    Rqualisup<-FALSE
    Rdescdim<-FALSE

    .SortieLabel<-tclVar(paste(firstLabel, "", sep=" "))

    OnSortie<-function()
    {
      SortieWin<-tktoplevel()
      tkwm.title(SortieWin,gettextRcmdr("Output options"))

      #création de la fonction onOKsub
      onOK.sortie<-function()
      {
        assign("compteur.sortie", compteur.sortie+1, envir=env)
        if(compteur.sortie>0) tclvalue(.SortieLabel)<-paste(label, "", sep=" ")
        tkconfigure(Sortie.but, fg="blue")
        
        if(tclvalue(eigValue)=="1") assign("Rpropre", TRUE, envir=env)
        else assign("Rpropre", FALSE, envir=env)
        
        if(tclvalue(groupeValue)=="1") assign("Rgroupe", TRUE, envir=env)
        else assign("Rgroupe", FALSE, envir=env)
        
        if(tclvalue(indValue)=="1") assign("Rindividu", TRUE, envir=env)
        else assign("Rindividu", FALSE, envir=env)
        
        if(tclvalue(Xc.Value)=="1") assign("RXc", TRUE, envir=env)
        else assign("RXc", FALSE, envir=env)
        
        if(tclvalue(var.partielValue)=="1") assign("Rvar.partiel", TRUE, envir=env)
        else assign("Rvar.partiel", FALSE, envir=env)
        
        if(tclvalue(quantiValue)=="1") assign("Rquanti", TRUE, envir=env)
        else assign("Rquanti", FALSE, envir=env)
        
        if(tclvalue(quantisupValue)=="1") assign("Rquantisup", TRUE, envir=env)
        else assign("Rquantisup", FALSE, envir=env)
        
        if(tclvalue(CovValue)=="1") assign("RCov", TRUE, envir=env)
        else assign("RCov", FALSE, envir=env)
        
        if(tclvalue(qualiValue)=="1") assign("Rquali", TRUE, envir=env)
        else assign("Rquali", FALSE, envir=env)
        
        if(tclvalue(qualisupValue)=="1") assign("Rqualisup", TRUE, envir=env)
        else assign("Rqualisup", FALSE, envir=env)
        
        if(tclvalue(descdimValue)=="1") assign("Rdescdim", TRUE, envir=env)
        else assign("Rdescdim", FALSE, envir=env)
        
        if (tclvalue(Fichier)=="") assign("RFichier", NULL, envir=env)
        assign("RFichier", tclvalue(Fichier), envir=env)

        tkdestroy(SortieWin)
      
      }
      
      eig.lab <-tklabel(SortieWin, text=gettextRcmdr("Eigenvalues"))
        eig.check <- tkcheckbutton(SortieWin)
        if(Rpropre) eigValue <- tclVar("1")
        else eigValue <- tclVar("0")
        tkconfigure(eig.check,variable=eigValue)
        
        groupe.lab <-tklabel(SortieWin, text=gettextRcmdr("Results for the variables"))
        groupe.check <- tkcheckbutton(SortieWin)
        if(Rgroupe) groupeValue <- tclVar("1")
        else groupeValue <- tclVar("0")
        tkconfigure(groupe.check,variable=groupeValue)
        
      ind.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for the active individuals"))
        ind.check <- tkcheckbutton(SortieWin)
        if(Rindividu) indValue <- tclVar("1")
        else indValue <- tclVar("0")
        tkconfigure(ind.check,variable=indValue)

      Xc.lab<-tklabel(SortieWin,text=gettextRcmdr("Group of individuals"))
        Xc.check <- tkcheckbutton(SortieWin)
        if(RXc) Xc.Value <- tclVar("1")
        else Xc.Value <- tclVar("0")
        tkconfigure(Xc.check,variable=Xc.Value)
        
      var.partiel.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for partial variables"))
        var.partiel.check <- tkcheckbutton(SortieWin)
        if(Rvar.partiel) var.partielValue <- tclVar("1")
        else var.partielValue <- tclVar("0")
        tkconfigure(var.partiel.check,variable=var.partielValue)
      
      quanti.lab<-tklabel(SortieWin,text=gettextRcmdr("Results of the quantitative variables"))
        quanti.check <- tkcheckbutton(SortieWin)
        if(Rquanti) quantiValue <- tclVar("1")
        else quantiValue <- tclVar("0")
        tkconfigure(quanti.check,variable=quantiValue)

      quantisup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results of the supplementary quantitative variables"))
        quantisup.check <- tkcheckbutton(SortieWin)
        if(Rquantisup) quantisupValue <- tclVar("1")
        else quantisupValue <- tclVar("0")
        tkconfigure(quantisup.check,variable=quantisupValue)
      
      Cov.lab<-tklabel(SortieWin,text=gettextRcmdr("Covariance matrices by group"))
        Cov.check <- tkcheckbutton(SortieWin)
        if(RCov) CovValue <- tclVar("1")
        else CovValue <- tclVar("0")
        tkconfigure(Cov.check,variable=CovValue)
      
      quali.lab<-tklabel(SortieWin,text=gettextRcmdr("Results of the qualitative variables"))
        quali.check <- tkcheckbutton(SortieWin)
        if(Rquali) qualiValue <- tclVar("1")
        else qualiValue <- tclVar("0")
        tkconfigure(quali.check,variable=qualiValue)

      qualisup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results of the supplementary qualitative variables"))
        qualisup.check <- tkcheckbutton(SortieWin)
        if(Rqualisup) qualisupValue <- tclVar("1")
        else qualisupValue <- tclVar("0")
        tkconfigure(qualisup.check,variable=qualisupValue)
      
        descdim.lab<-tklabel(SortieWin, text=gettextRcmdr("Description of the dimensions"))
      descdim.check<-tkcheckbutton(SortieWin)
      if(Rdescdim) descdimValue<-tclVar("1")
      else descdimValue<-tclVar("0")
      tkconfigure(descdim.check,variable=descdimValue)

      RFichierFrame<-tkframe(SortieWin,borderwidth=2)
      if (is.null(RFichier)) Fichier <- tclVar("")
      else Fichier<-tclVar(RFichier)
      Fichier.entry <-tkentry(RFichierFrame,width="40",textvariable=Fichier)
      tkgrid(tklabel(RFichierFrame,text=gettextRcmdr("Print results on a 'csv' file")),Fichier.entry)

      SortieOK.but<-tkbutton(SortieWin,text="OK",width=16,command=onOK.sortie)

      tkgrid(tklabel(SortieWin, text = gettextRcmdr("Select output options"), fg ="blue"),  columnspan = 2, sticky = "w")
      tkgrid(tklabel(SortieWin, text = " "))
      tkgrid(eig.lab,eig.check,sticky="w")
      tkgrid(groupe.lab,groupe.check,sticky="w")
      tkgrid(ind.lab,ind.check,sticky="w")
      tkgrid(quanti.lab,quanti.check,sticky="w")      
      tkgrid(var.partiel.lab,var.partiel.check,sticky="w")
      if (!is.null(variableillu)) tkgrid(quantisup.lab,quantisup.check,sticky="w")
      if (!is.null(variablefact)) tkgrid(qualisup.lab,qualisup.check,sticky="w")
      tkgrid(Cov.lab,Cov.check,sticky="w")
      tkgrid(Xc.lab,Xc.check,sticky="w")
      tkgrid(descdim.lab,descdim.check,sticky="w")
      tkgrid(tklabel(SortieWin, text = " "))
      tkgrid(RFichierFrame)
      tkgrid(tklabel(SortieWin, text = " "))
      tkgrid(SortieOK.but)
   }
    
    SortieFrame<-tkframe(IlluFrame)
    Sortie.but<-tkbutton(SortieFrame, textvariable=.SortieLabel, command=OnSortie, borderwidth=3)
    tkgrid(Sortie.but, sticky="ew")
  })



  #! fonction pour la gestion des options graphiques 
  PLOT.DMFA<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.graph<-0
    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))
    #déclaration des variables
    Gchoix<-TRUE
    GTitle<-NULL
    GAxeGrpe<-c(1,2)
    Glabel<-"all"
        
    Rchoix<-TRUE
    RTitle<-NULL
    Rlabel.indMoy<-"ind"
    Rlabel.quali<-NULL    
    Rhabillage<-"none"
    Rinvisible<-NULL
    RXlimInd<-NULL
    RYlimInd<-NULL
    
    Wchoix=TRUE
    WTitle<-NULL
    WAxeVar<-c(1,2)
    Wlabel.var<-"all"
    #Wcol.quanti.sup<-Wcol.quanti.sup.tmp<-"blue"
    #Wcol.var<-Wcol.var.tmp<-"black"
    #Winvisible<-NULL
    #Wlim.cos<-0.
    
        
    OnPlot<-function()
    {
      PlotWin<-tktoplevel()
      tkwm.title(PlotWin,gettextRcmdr("Graphical options"))
      tkwm.geometry(PlotWin, "-100+50")
      PlotWin2<-tkframe(PlotWin)

      #création de la fonction onOKsub
      onOKsub<-function()
      {
        assign("compteur.graph", compteur.graph+1, envir=env)
        if(compteur.graph>0) tclvalue(.PlotLabel)<-paste(label, gettextRcmdr(""), sep=" ")
        tkconfigure(Plot.but, fg="blue")

        # gestion des entrées de la partie graphique des Groupes
        if(tclvalue(grpe.check.value)==1) assign("Gchoix", TRUE, envir=env)
        else assign("Gchoix", FALSE, envir=env)

        if(Gchoix) {
          if (tclvalue(GTitre)==" ") assign("GTitle", NULL, envir=env)
          assign("GTitle", tclvalue(GTitre), envir=env)

          label.tmp.grpe<-tclvalue(label.grpe.checkValue)
          if(label.tmp.grpe==1) assign("Glabel", "all", envir=env)
          else assign("Glabel", "none", envir=env)
        }
                    
        # gestion des entrées de la partie graphique des variables
        if(tclvalue(var.check.value)==1) assign("Wchoix", TRUE, envir=env)
        else assign("Wchoix", FALSE, envir=env)

        if(Wchoix) {
          if (tclvalue(WTitre)==" ") assign("WTitle", NULL, envir=env)
          assign("WTitle", tclvalue(WTitre), envir=env)

          #assign("Wlim.cos", tclvalue(WlimCosValue), envir=env)

          label.tmp.var<-tclvalue(label.var.checkValue)
          if(label.tmp.var==1) assign("Wlabel.var", "all", envir=env)
          else assign("Wlabel.var", "none", envir=env)

          #assign("Wcol.var", Wcol.var.tmp, envir=env)
          #assign("Wcol.quanti.sup", Wcol.quanti.sup.tmp, envir=env)

          #if(tclvalue(inv.Value)=="aucun")  assign("Winvisible", NULL, envir=env)
          #else assign("Winvisible", tclvalue(inv.Value), envir=env)
        }

        # gestion des entrées de la partie graphique des individus
        if(tclvalue(ind.check.value)==1) assign("Rchoix", TRUE, envir=env)
        else assign("Rchoix", FALSE, envir=env)

        if(Rchoix) {
          if (tclvalue(Titre)==" ") assign("RTitle", NULL, envir=env)
          assign("RTitle", tclvalue(Titre), envir=env)

          label.tmp.indMoy<-tclvalue(label.indMoy.checkValue)
          label.tmp.quali<-tclvalue(label.quali.checkValue)
          if(label.tmp.indMoy==1) assign("Rlabel.indMoy", "ind", envir=env)
          else assign("Rlabel.indMoy", NULL, envir=env)
          if(label.tmp.quali==1) assign("Rlabel.quali", "quali", envir=env)
          else assign("Rlabel.quali", NULL, envir=env)
          
##          habillage.tmp<-listgraph.nom[as.numeric(tkcurselection(listgraph))+1]
##          if(length(habillage.tmp)==0) assign("Rhabillage","none", envir=env)
##          else assign("Rhabillage", habillage.tmp, envir=env)

          if(tclvalue(XlimIndMin)=="" | tclvalue(XlimIndMax)=="") assign("RXlimInd", NULL, envir=env)
          else assign("RXlimInd", c(as.numeric(tclvalue(XlimIndMin)), as.numeric(tclvalue(XlimIndMax))), envir=env)
          if(tclvalue(YlimIndMin)=="" | tclvalue(YlimIndMax)=="") assign("RYlimInd", NULL, envir=env)
          else assign("RYlimInd", c(as.numeric(tclvalue(YlimIndMin)), as.numeric(tclvalue(YlimIndMax))), envir=env)
          
          inv.ind.tmp<-tclvalue(inv.ind.checkValue)
          inv.ind.sup.tmp<-tclvalue(inv.ind.sup.checkValue)
          inv.quali.tmp<-tclvalue(inv.quali.checkValue)
          assign("Rinvisible", NULL, envir=env)
          if(inv.ind.tmp=="1") assign("Rinvisible", c(Rinvisible, "ind"), envir=env)
          if(inv.ind.sup.tmp=="1") assign("Rinvisible", c(Rinvisible, "ind.sup"), envir=env)          
          if(inv.quali.tmp=="1") assign("Rinvisible", c(Rinvisible, "quali"), envir=env)          
        }
        tkdestroy(PlotWin)
      }
    
      # création l'interface "options graphiques"
    
      ##########################
      # construction de la partie graphique des Groupes
      PlotGrpeFrame<-tkframe(PlotWin2, borderwidth=5, relief="groove")
  
      GchoixFrame<-tkframe(PlotGrpeFrame,borderwidth=2)
      grpe.check<-tkcheckbutton(GchoixFrame)
      if(Gchoix) grpe.check.value<-tclVar("1")
      else grpe.check.value<-tclVar("0")
      tkconfigure(grpe.check, variable=grpe.check.value)
      tkgrid(tklabel(GchoixFrame, text=gettextRcmdr("Graph the groups"), font=font2),grpe.check)
      tkgrid(tklabel(GchoixFrame, text="  "))
  
      GTitleFrame<-tkframe(PlotGrpeFrame,borderwidth=2)
      if (is.null(GTitle)) GTitre <- tclVar(" ")
      else GTitre<-tclVar(GTitle)
      GTitre.entry <-tkentry(GTitleFrame,width="40",textvariable=GTitre)
      tkgrid(tklabel(GTitleFrame,text=gettextRcmdr("Title of the graph")),GTitre.entry)
    
      GlabelFrame<-tkframe(PlotGrpeFrame,borderwidth=2)
      label.grpe.check<-tkcheckbutton(GlabelFrame)
      if (Glabel=="all") label.grpe.checkValue<-tclVar("1")
      else label.grpe.checkValue<-tclVar("0")
      tkconfigure(label.grpe.check, variable=label.grpe.checkValue)
      tkgrid(tklabel(GlabelFrame, text=gettextRcmdr("Draw labels of the groups")),label.grpe.check)
       
      #mise en page des différents frames de PlotGrpeFrame
      tkgrid(GchoixFrame)
      tkgrid(GTitleFrame)
      tkgrid(GlabelFrame)
      tkgrid(tklabel(PlotGrpeFrame, text=" "))
      
      
      ########################
      # construction de la partie graphique des variables
      PlotVarFrame<-tkframe(PlotWin2, borderwidth=5, relief="groove")
  
      WchoixFrame<-tkframe(PlotVarFrame,borderwidth=2)
      var.check<-tkcheckbutton(WchoixFrame)
      if(Wchoix) var.check.value<-tclVar("1")
      else var.check.value<-tclVar("0")
      tkconfigure(var.check, variable=var.check.value)
      tkgrid(tklabel(WchoixFrame, text=gettextRcmdr("Graph of the quantitative variables"), font=font2),var.check)
      tkgrid(tklabel(WchoixFrame, text="  "))
  
      WTitleFrame<-tkframe(PlotVarFrame,borderwidth=2)
      if (is.null(WTitle)) WTitre <- tclVar(" ")
      else WTitre<-tclVar(WTitle)
      WTitre.entry <-tkentry(WTitleFrame,width="40",textvariable=WTitre)
      tkgrid(tklabel(WTitleFrame,text=gettextRcmdr("Title of the graph")),WTitre.entry)
  
      #WcosFrame<-tkframe(PlotVarFrame,borderwidth=2)
      #WlimCosValue<-tclVar(paste(Wlim.cos))
      #WlimCos.entry<-tkentry(WcosFrame, width=5, textvariable=WlimCosValue)
      #tkgrid(tklabel(WcosFrame,text=gettextRcmdr("Draw variables with a cos2 >:")),WlimCos.entry)
  
      WlabelFrame<-tkframe(PlotVarFrame,borderwidth=2)
      label.var.check<-tkcheckbutton(WlabelFrame)
      if (Wlabel.var=="all") label.var.checkValue<-tclVar("1")
      else label.var.checkValue<-tclVar("0")
      tkconfigure(label.var.check, variable=label.var.checkValue)
      tkgrid(tklabel(WlabelFrame, text=gettextRcmdr("Draw the labels of the variables")),label.var.check)
      
#    WcolFrame<-tkframe(PlotVarFrame,borderwidth=2)
#    Wcol.var.value <- Wcol.var
#    Wcanvas.var <- tkcanvas(WcolFrame,width="80",height="25",bg=Wcol.var.value)
#    WChangeColor.var <- function()
#    {
#      Wcol.var.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Wcol.var.value,title=gettextRcmdr("Choose a color")))
#      if (nchar(Wcol.var.value)>0) {
#        tkconfigure(Wcanvas.var,bg=Wcol.var.value)
#        assign("Wcol.var.tmp", Wcol.var.value, envir=env)
#      }
#    }
#    WChangeColor.var.button <- tkbutton(WcolFrame,text=gettextRcmdr("Change Color"),command=WChangeColor.var)
#    tkgrid(tklabel(WcolFrame, text=gettextRcmdr("Color of the active variables")),Wcanvas.var,WChangeColor.var.button)
    
#    Wcol.quanti.sup.value<-Wcol.quanti.sup
#    Wcanvas.quanti.sup <- tkcanvas(WcolFrame,width="80",height="25",bg=Wcol.quanti.sup.value)
#    WChangeColor.quanti.sup <- function()
#    {
#      Wcol.quanti.sup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Wcol.quanti.sup.value,title=gettextRcmdr("Choose a color")))
#      if (nchar(Wcol.quanti.sup.value)>0) {
#        tkconfigure(Wcanvas.quanti.sup,bg=Wcol.quanti.sup.value)
#        assign("Wcol.quanti.sup.tmp", Wcol.quanti.sup.value, envir=env)
#      }
#    }
#    WChangeColor.quanti.sup.button <- tkbutton(WcolFrame,text=gettextRcmdr("Change Color"),command=WChangeColor.quanti.sup)
#    if(!is.null(variableillu)) tkgrid(tklabel(WcolFrame, text=gettextRcmdr("color for supplementary variables")),Wcanvas.quanti.sup,WChangeColor.quanti.sup.button)
         
#      WinvisibleFrame<-tkframe(PlotVarFrame,borderwidth=2)
#      inv.aucun.check<-tkradiobutton(WinvisibleFrame)
#      inv.act.check<-tkradiobutton(WinvisibleFrame)
#      inv.sup.check<-tkradiobutton(WinvisibleFrame)
#      if(is.null(Winvisible)) inv.Value<-tclVar("aucun")
#      else inv.Value<-tclVar(Winvisible) 
#      tkconfigure(inv.aucun.check,variable=inv.Value,value="aucun")
#      tkconfigure(inv.act.check,variable=inv.Value, value="actif")
#      tkconfigure(inv.sup.check,variable=inv.Value, value="sup")
#      tkgrid(tklabel(WinvisibleFrame, text=gettextRcmdr("Hide some elements:")), columnspan=6, sticky="w")
#      tkgrid(tklabel(WinvisibleFrame, text="None"),inv.aucun.check, tklabel(WinvisibleFrame, text=gettextRcmdr("active variables")),inv.act.check, tklabel(WinvisibleFrame, text=gettextRcmdr("supplementary variables")),inv.sup.check, sticky="w")
        
      #mise en page des différents frames de PlotVarFrame
      tkgrid(WchoixFrame)
      tkgrid(WTitleFrame)
      #tkgrid(WcolFrame)
      #tkgrid(WcosFrame)
      tkgrid(WlabelFrame)
      #tkgrid(WinvisibleFrame)            
      tkgrid(tklabel(PlotVarFrame, text=" "))
        
      ##########################  
      # construction de la partie graphique des individus
      PlotIndFrame<-tkframe(PlotWin, borderwidth=5, relief="groove")
      
      RchoixFrame<-tkframe(PlotIndFrame,borderwidth=2)
      ind.check<-tkcheckbutton(RchoixFrame)
      if(Rchoix) ind.check.value<-tclVar("1")
      else ind.check.value<-tclVar("0")
      tkconfigure(ind.check, variable=ind.check.value)
      tkgrid(tklabel(RchoixFrame, text=gettextRcmdr("Graph of the individuals"), font=font2),ind.check)
      tkgrid(tklabel(RchoixFrame, text=" "))
      
      RTitleFrame<-tkframe(PlotIndFrame,borderwidth=2)
      if (is.null(RTitle)) Titre <- tclVar(" ")
      else Titre<-tclVar(RTitle)
      Titre.entry <-tkentry(RTitleFrame,width="40",textvariable=Titre)
      tkgrid(tklabel(RTitleFrame,text=gettextRcmdr("Title of the graph")),Titre.entry)
      
      RlabelFrame<-tkframe(PlotIndFrame,borderwidth=2)
      label.indMoy.check<-tkcheckbutton(RlabelFrame)
      if (!is.null(Rlabel.indMoy)) label.indMoy.checkValue<-tclVar("1")
      else label.indMoy.checkValue<-tclVar("0") 
      tkconfigure(label.indMoy.check, variable=label.indMoy.checkValue)
      tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Draw labels for the mean individuals")),label.indMoy.check)
      label.quali.check<-tkcheckbutton(RlabelFrame)
      if (!is.null(Rlabel.quali)) label.quali.checkValue<-tclVar("1")
      else label.quali.checkValue<-tclVar("0")
      tkconfigure(label.quali.check, variable=label.quali.checkValue)
      if (!is.null(variablefact)) tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Draw labels for the qualitative variables")), label.quali.check)
              
##      RhabillageFrame<-tkframe(PlotIndFrame,borderwidth=2)
##      listgraph<-tklistbox(RhabillageFrame,height=4, selectmode="single",exportselection="FALSE",yscrollcommand=function(...) tkset(scrgraph,...))
##      scrgraph <-tkscrollbar(RhabillageFrame,repeatinterval=5,command=function(...)tkyview(listgraph,...))
##      listgraph.nom<-c("ind")
##      tkinsert(listgraph,"end","by.individual")
##      if(Rhabillage=="ind") tkselection.set(listgraph,0)
##      indice<-1      
##      nbauxli<-c(tclvalue(tkcurselection(listfact)))
##      nbaux<-unlist(strsplit(nbauxli,"\\ "))
##      varaux = vars.fact[as.numeric(tkcurselection(listfact))+1]
##      
##      if (!is.null(variablefact)|(length(nbaux)>0)){
##        for (j in 1:ncol(donnee)){
##         if(vars[j] %in% c(variablefact,varaux)){
##          tkinsert(listgraph,"end",vars[j])
##          listgraph.nom<-c(listgraph.nom,vars[j])
##          if(Rhabillage==vars[j]) tkselection.set(listgraph, indice)
##          indice<-indice+1
##        }}
##      }
##      tkgrid(tklabel(RhabillageFrame, text=gettextRcmdr("Select drawing for the individuals")))
##      tkgrid(listgraph, scrgraph, sticky = "nw")
##      tkgrid.configure(scrgraph, sticky = "wns")
##      tkgrid.configure(listgraph, sticky = "ew")
      
      
      RinvisibleFrame<-tkframe(PlotIndFrame,borderwidth=2)
      inv.ind.check<-tkcheckbutton(RinvisibleFrame)
      if ("ind" %in% Rinvisible) inv.ind.checkValue<-tclVar("1")
      else inv.ind.checkValue<-tclVar("0") 
      inv.ind.sup.check<-tkcheckbutton(RinvisibleFrame)
      if ("ind.sup" %in% Rinvisible) inv.ind.sup.checkValue<-tclVar("1")
      else inv.ind.sup.checkValue<-tclVar("0") 
      inv.quali.check<-tkcheckbutton(RinvisibleFrame)      
      if ("quali" %in% Rinvisible) inv.quali.checkValue<-tclVar("1")
      else inv.quali.checkValue<-tclVar("0") 
      tkconfigure(inv.ind.check, variable=inv.ind.checkValue)
      tkconfigure(inv.ind.sup.check, variable=inv.ind.sup.checkValue)
      tkconfigure(inv.quali.check, variable=inv.quali.checkValue)            
      if (!is.null(variablefact)) {
        tkgrid(tklabel(RinvisibleFrame, text=gettextRcmdr("Hide some elements:")), columnspan=6, sticky="w")
#        tkgrid(tklabel(RinvisibleFrame, text="ind"),inv.ind.check, tklabel(RinvisibleFrame, text="ind sup"),inv.ind.sup.check, tklabel(RinvisibleFrame, text="quali"),inv.quali.check, sticky="w")
        tkgrid(tklabel(RinvisibleFrame, text="ind"),inv.ind.check, tklabel(RinvisibleFrame, text="quali"),inv.quali.check, sticky="w")
      }
      
            
      RlimFrame<-tkframe(PlotIndFrame,borderwidth=2)
      if(is.null(RXlimInd)) XlimIndMin<-tclVar("")
      else XlimIndMin<-tclVar(paste(RXlimInd[1]))
      XlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMin)
      if (is.null(RXlimInd)) XlimIndMax<- tclVar("")
      else XlimIndMax<-tclVar(paste(RXlimInd[1]))
      XlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMax)
      tkgrid(tklabel(RlimFrame,text=gettextRcmdr("x limits of the graph:")),XlimIndMin.entry, XlimIndMax.entry)
      if(is.null(RYlimInd)) YlimIndMin<- tclVar("")
      else YlimIndMin<-tclVar(paste(RYlimInd[1]))
      YlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMin)
      if (is.null(RYlimInd)) YlimIndMax<- tclVar("")
      else YlimIndMax<-tclVar(paste(RYlimInd[2]))
      YlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMax)
      tkgrid(tklabel(RlimFrame,text=gettextRcmdr("y limits of the graph:")),YlimIndMin.entry,YlimIndMax.entry)
  
  
      #mise en page des différents frames de PlotIndFrame
      tkgrid(RchoixFrame)
      tkgrid(RTitleFrame)
      tkgrid(RlabelFrame)
      tkgrid(RinvisibleFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))
##      tkgrid(RhabillageFrame)
##      tkgrid(tklabel(PlotIndFrame, text=" "))      
      tkgrid(RlimFrame)
      tkgrid(tklabel(PlotIndFrame, text=" "))
      
              
      #mise en page de plotWin
      subOKCancelHelp(PlotWin, "plot.DMFA")
      tkgrid(PlotGrpeFrame)
      tkgrid(PlotVarFrame)
      tkgrid(PlotIndFrame, PlotWin2, sticky="ns")
      tkgrid(subButtonsFrame, sticky="ew", columnspan=2)
    }

    PlotFrame<-tkframe(IlluFrame)
    Plot.but<-tkbutton(PlotFrame, textvariable=.PlotLabel, command=OnPlot, borderwidth=3)
    tkgrid(Plot.but, sticky="ew")
  }) 



    #! fonction associée au bouton Appliquer, execute sans détruire l'interface graphique
  OnAppliquer<-function()
  {
      #liste de l'ensemble des variables créées
      #sur la fenêtre top
#      listQuantiAct
#      listQuantiIllu
#      listQualiAct
#      listQualiIllu
#      resu.val
#      ncp.val
      #pour les individus illustratifs
#      individuillu
      #pour l'affichage
#      Rpropre
#          Rgroupe
#          Rindividu
#        RXc
#      Rvar.partiel
#      Rquanti
#      Rquantisup  
#          RCov
#          Rquali
#          Rqualisup
#          Rdescdim
      # pour les graphiques
#      Gchoix
#      GTitle
#      GAxeGrpe
#      Glabel
#        
#      Rchoix
#      RTitle
#      Rlabel.indMoy
#      Rlabel.quali  
#      Rhabillage
#      Rinvisible
#      Rpartial
#      RpartialSouris
#      Rchrono
#      RXlimInd
#      RYlimInd
#    
#      Wchoix
#      WTitle
#      WAxeVar
#      Wlabel.var
#      Winvisible
#      Wlim.cos
#
#      Axe 
  
    # récupération des paramètres de la fenêtre principale
    nom.res<-tclvalue(resu.val)
    if (length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0) justDoIt(paste('remove (',nom.res,')'))
    ncp<-as.numeric(tclvalue(ncp.val))
    reduction<-TRUE
    if(tclvalue(reduitValue)=="0") reduction<-FALSE
    Axe<-c(as.numeric(tclvalue(Axe1)), as.numeric(tclvalue(Axe2)))

    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    nbitemlist.q<-c(tclvalue(tkcurselection(listfact)))
    nbitem.q<-unlist(strsplit(nbitemlist.q,"\\ "))

 
    # gestion du tableau de données pour DMFA
    variables <- variables.q <- NULL
    variables <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
    variables.q <- vars.fact[as.numeric(tkcurselection(listfact))+1]
    allvariables = c(variables,variables.q,variableillu,variablefact)
    num.group.sup<-NULL
    if (length(variableillu)+length(variablefact)>0) num.group.sup <- ((length(variables)+length(variables.q)+1):length(allvariables))
    #construction du tableau de données.DMFA
      if(!is.null(individuillu)) {
        ind.actif<-rows[-which(rows %in% individuillu)]
        commande.data<-paste(activeDataSet(),'.DMFA', '<-', activeDataSet(),'[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '"),', sep='')
      }
      else commande.data<-paste(activeDataSet(),'.DMFA', '<-', activeDataSet(),'[,', sep='')
      commande.data<-paste(commande.data,' c("',paste(allvariables, collapse='", "'), '")]',sep='')
      
      justDoIt(commande.data)
      logger(commande.data)
      donnee.depart<-activeDataSet()
      activeDataSet(paste(activeDataSet(),'.DMFA', sep=""))

      # gestion de la commande réalisant l'AFMD     
      commande.DMFA<-paste(nom.res, '<-DMFA(', activeDataSet(), ', num.fact=',length(variables)+1,', ncp=', ncp,', scale.unit = ', reduction,sep='')
      if(!is.null(individuillu)) commande.DMFA<-paste(commande.DMFA, ', ind.sup=', nrow(get(.activeDataSet))-length(individuillu)+1, ': ', nrow(get(.activeDataSet)),sep='')
      if (!is.null(variableillu)) commande.DMFA<-paste(commande.DMFA, ', quanti.sup =',length(variables)+2,':',length(variables)+1+length(variableillu),sep='')
      if (!is.null(variablefact)) commande.DMFA<-paste(commande.DMFA, ', quali.sup =',length(allvariables)-length(variablefact)+1,':',length(allvariables),sep='')
      commande.DMFA<-paste(commande.DMFA, ', graph=FALSE)',sep='')
      justDoIt(commande.DMFA)
      logger(commande.DMFA)

      #gestion des graphiques   
      if (length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0) {if (get(nom.res)$eig[1,2]==100) doItAndPrint(paste('"No graph can be plot: data are unidimensional"'))}
      if((Gchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)){
      if (get(nom.res)$eig[1,2]!=100) {
        commande.plotG<-paste('plot.DMFA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="group"', ',label="', Glabel,'"', sep='')
#        if (!Glabel) commande.plotG <- paste(commande.plotG,', label="none"', sep="")
#        else commande.plotG <- paste(commande.plotG,', label="', Glabel,'"', sep="")
        if (is.null(GTitle)) commande.plotG <- paste(commande.plotG,')', sep="")
        else {
          if (GTitle ==" ") commande.plotG <- paste(commande.plotG,')', sep="")
          else commande.plotG <- paste(commande.plotG,', title="', GTitle,'")', sep="")
        }
        justDoIt(commande.plotG)
        logger(commande.plotG)
      }}
            
      if((Wchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)&(length(variables)>0)){
      if (get(nom.res)$eig[1,2]!=100) {
#        commande.plotW<-paste('plot.DMFA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="var", col.var="', Wcol.var, '", col.quanti.sup="', Wcol.quanti.sup, '", label=c("', paste(Wlabel.var, collapse='", ")'), '"), lim.cos2.var=', Wlim.cos, sep="")
#        commande.plotW<-paste('plot.DMFA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="var", col.var="', Wcol.var, '", col.quanti.sup="', Wcol.quanti.sup, '", label=c("', paste(Wlabel.var, collapse='", ")'), '")', sep="")
        commande.plotW<-paste('plot.DMFA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="var", label=c("', paste(Wlabel.var, collapse='", ")'), '")', sep="")        

#        if (!is.null(Winvisible)) commande.plotW<-paste(commande.plotW, ', invisible=c("', paste(Winvisible, collapse='", "'),'")', sep='')        
        if (is.null(WTitle)) commande.plotW <- paste(commande.plotW,')', sep="")
        else {
          if (WTitle ==" ") commande.plotW <- paste(commande.plotW,')', sep="")
          else commande.plotW <- paste(commande.plotW,', title="', WTitle,'")', sep="")
        }
        justDoIt(commande.plotW)
        logger(commande.plotW)
      }}
      
      if((Rchoix)&(length(which(ls(envir = .GlobalEnv, all.names = TRUE)==nom.res))>0)){
      if (get(nom.res)$eig[1,2]!=100) {
        if ((Rhabillage!="none") & (Rhabillage!="ind")) {
          Rhabillage<-which(colnames(get(.activeDataSet))==Rhabillage)
          if(length(Rhabillage)==0) Rhabillage<-"none"
        }
        if (Rhabillage=="none") Rhabillage<-paste('"', Rhabillage, '"', sep="")
        if (Rhabillage=="ind") Rhabillage<-paste('"', Rhabillage, '"', sep="")
        

          commande.plotI<-paste('plot.DMFA(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="ind"', sep="") 
          if (!is.null(RXlimInd)) commande.plotI<-paste(commande.plotI, ', xlim=c(', paste(RXlimInd, collapse=", "), ')', sep='')
          if (!is.null(RYlimInd)) commande.plotI<-paste(commande.plotI, ', ylim=c(', paste(RYlimInd, collapse=", "), ')', sep='')
          if (!is.null(Rinvisible)) commande.plotI<-paste(commande.plotI, ', invisible=c("', paste(Rinvisible, collapse='", "'),'")', sep='')
          if (!is.null(Rlabel.indMoy) & !is.null(Rlabel.quali)) commande.plotI<-paste(commande.plotI, ', label = c("',Rlabel.indMoy, '","', Rlabel.quali,'")',sep='')
          if (!is.null(Rlabel.indMoy) & is.null(Rlabel.quali)) commande.plotI<-paste(commande.plotI, ', label = c("',Rlabel.indMoy, '")',sep='')  
          if (is.null(Rlabel.indMoy) & !is.null(Rlabel.quali)) commande.plotI<-paste(commande.plotI, ', label = c("',Rlabel.quali,'")',sep='')   
          if (is.null(Rlabel.indMoy) & is.null(Rlabel.quali)) commande.plotI<-paste(commande.plotI, ', label = "none"',sep='')                          
          if (is.null(RTitle)) commande.plotI <- paste(commande.plotI,')', sep="")
          else {
            if (RTitle ==" ") commande.plotI <- paste(commande.plotI,')', sep="")
            else commande.plotI <- paste(commande.plotI,', title="', RTitle,'")', sep="")
          }
        }
        justDoIt(commande.plotI)
        logger(commande.plotI)
      }
      
      # gestion de l'édition de certains resultats
    if (RFichier==""){
      if(Rpropre) doItAndPrint(paste(nom.res, '$eig', sep=""))
      if(Rgroupe) doItAndPrint(paste(nom.res, '$group', sep=""))
      if(Rindividu) doItAndPrint(paste(nom.res, '$ind', sep=""))
      if(Rquanti) doItAndPrint(paste(nom.res, '$var', sep=""))      
      if(Rvar.partiel) doItAndPrint(paste(nom.res, '$var.partiel', sep=""))
      if(Rquantisup) doItAndPrint(paste(nom.res, '$quanti.sup', sep=""))
      if(Rqualisup) doItAndPrint(paste(nom.res, '$quali.sup', sep=""))
      if(RCov) doItAndPrint(paste( nom.res, '$Cov', sep=""))
      if(RXc) doItAndPrint(paste( nom.res, '$Xc', sep=""))
      if(Rdescdim) doItAndPrint(paste('dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '))', sep=""))
    }
    else {
      Fich = RFichier
      if (substr(Fich,1,1)!='"') Fich = paste('"',Fich,sep='')
      if (substr(Fich,nchar(Fich),nchar(Fich))!='"') Fich = paste(Fich,'"',sep='')
      append = FALSE
      if(Rpropre){
        doItAndPrint(paste('write.infile(', nom.res, '$eig, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rgroupe){
        doItAndPrint(paste('write.infile(', nom.res, '$group, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rindividu){
        doItAndPrint(paste('write.infile(', nom.res, '$ind, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rquanti){
        doItAndPrint(paste('write.infile(', nom.res, '$var, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rvar.partiel){
        doItAndPrint(paste('write.infile(', nom.res, '$var.partiel, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rquantisup){
        doItAndPrint(paste('write.infile(', nom.res, '$quanti.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rqualisup){
        doItAndPrint(paste('write.infile(', nom.res, '$quali.sup, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(RCov){
        doItAndPrint(paste('write.infile(', nom.res, '$Cov, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(RXc){
        doItAndPrint(paste('write.infile(', nom.res, '$Xc, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rdescdim) doItAndPrint(paste('write.infile(dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), ')), file =',Fich,',append=',append,')', sep=""))
    }

      # Re-chargement du tableau de départ et supression du tableau temporaire
      activeDataSet(donnee.depart)
      justDoIt(paste('remove(',activeDataSet(),'.DMFA)',sep=""))
      logger(paste('remove(',activeDataSet(),'.DMFA)',sep=""))   
  }


    #! fonction associée au bouton OK, execute et détruit l'interface graphique
  onOK<-function()
  {
    OnAppliquer()
    tkdestroy(top)     
  }



#                   Création de la fenêtre top                                 #
################################################################################
##  top<-tktoplevel(borderwidth=10)
##  tkwm.title(top,gettextRcmdr("DMFA"))
##  tkwm.geometry(top, "-50+50")
##  
##  # définition des polices
##  font2<-tkfont.create(family="times",size=12,weight="bold")
##  fontheading<-tkfont.create(family="times",size=11,weight="bold")
##
##  # récupération du jeu de données actif
##  donnee<-get(.activeDataSet)
##  vars<-colnames(donnee)
##  rows<-rownames(donnee)
  

   # création de tous les boutons d'options dans IlluFrame
  IlluFrame<- tkframe(top, borderwidth=2)
  Reinitializ.but<-tkbutton(IlluFrame, text="Restart",width=18,command=Reinitializ.funct, borderwidth=3)


    # mise en page de IlluFrame
  Fillu.funct(label=gettextRcmdr("Modify supplementary factors"), firstLabel=gettextRcmdr("Select supplementary factors"))
  Dillu.funct(label=gettextRcmdr("Modify supplementary variables"), firstLabel=gettextRcmdr("Select supplementary variables"))
  Iillu.funct(label=gettextRcmdr("Modify supplementary individuals"), firstLabel=gettextRcmdr("Select supplementary individuals"))    
  PLOT.DMFA(label=gettextRcmdr("Graphical options"), firstLabel=gettextRcmdr("Graphical options"))
  Sortie.funct(label=gettextRcmdr("Outputs"), firstLabel=gettextRcmdr("Outputs"))
  tkgrid(DilluFrame, FilluFrame, columnspan=7)
#  tkgrid(DilluFrame, FilluFrame, IilluFrame, columnspan=7)
#  tkgrid.configure(IilluFrame, column=5, columnspan=1)
  tkgrid.configure(DilluFrame, column=1, columnspan=1)
  tkgrid.configure(FilluFrame, column=3, columnspan=1)
  tkgrid.columnconfigure(IlluFrame,0, minsize=25)
  tkgrid.columnconfigure(IlluFrame,2, minsize=40)  
  tkgrid.columnconfigure(IlluFrame,4, minsize=25)  
  tkgrid(tklabel(IlluFrame, text=""))

  tkgrid(PlotFrame, SortieFrame, Reinitializ.but, columnspan=7)
  tkgrid.configure(PlotFrame, column=1, columnspan=1)
  tkgrid.configure(SortieFrame, column=3, columnspan=1)
  tkgrid.configure(Reinitializ.but, column=5, columnspan=1)      


    # création des options dans OptionFrame  
  OptionFrame<-tkframe(top, borderwidth=2, relief="groove")
  resu.lab<-tklabel(OptionFrame,text=gettextRcmdr("Name of the result object: "))
  resu.val<-tclVar("res")
  resu<-tkentry(OptionFrame,width=10,textvariable=resu.val)
  reduit.lab<-tklabel(OptionFrame,text=gettextRcmdr("Scale the quantative variables: "))
  reduit.check <- tkcheckbutton(OptionFrame)
  reduitValue <- tclVar("1")
  tkconfigure(reduit.check,variable=reduitValue)
  ncp.lab<-tklabel(OptionFrame,text=gettextRcmdr("Number of dimensions: "))
  ncp.val<-tclVar("5") 
  ncp<-tkentry(OptionFrame,width=5,textvariable=ncp.val)
  Axe.label<-tklabel(OptionFrame,text=gettextRcmdr("Select the dimensions for the graphs:"))
  Axe1<-tclVar("1")
  Axe2<-tclVar("2")
  Axe1.entry <-tkentry(OptionFrame,width="5",textvariable=Axe1)
  Axe2.entry <-tkentry(OptionFrame,width="5",textvariable=Axe2)
  
    # mise en page de OptionFrame
  tkgrid(tklabel(OptionFrame,text=gettextRcmdr("Main options"), fg = "darkred"), columnspan=8, sticky="we") 
  tkgrid(tklabel(OptionFrame,text="")) 
  tkgrid(reduit.lab,reduit.check)
  tkgrid(ncp.lab, ncp)
  tkgrid(Axe.label,Axe1.entry , Axe2.entry, sticky="w")
  tkgrid(resu.lab, resu)
  tkgrid.configure(ncp.lab, reduit.lab, resu.lab, Axe.label, column=1, columnspan=4, sticky="w")
  tkgrid.configure(ncp, resu, reduit.check, column=6, columnspan=2, sticky="e")
  tkgrid.configure(Axe1.entry, column=6, columnspan=1, sticky="w")
  tkgrid.configure(Axe2.entry, column=7, columnspan=1, sticky="e")
  tkgrid.columnconfigure(OptionFrame,0, minsize=25)
  tkgrid.columnconfigure(OptionFrame,5, minsize=40)
  tkgrid.columnconfigure(OptionFrame,8, minsize=25)

  appliquer.but<-tkbutton(top, text="Apply",width=12,command=OnAppliquer, borderwidth=3, fg="#690f96")
  OKCancelHelp(helpSubject="DMFA")

  # Mise en page de top
  tkgrid(tklabel(top, text=gettextRcmdr("Duale Multiple Factor Analysis (DMFA)"),font=fontheading), columnspan=3)
  tkgrid(tklabel(top,text=""))
  tkgrid(listFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text=""))
  tkgrid(IlluFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text=""))    
  tkgrid(OptionFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text=""))   
  tkgrid(appliquer.but, column=1, columnspan=1)
  tkgrid(tklabel(top,text=""))
  tkgrid(buttonsFrame, column=1, columnspan=1, sticky="ew" )  
}  

#############################FIN FONCTION FactoDMFA ##############################

###########################################################################################################################################################
###################################################################          FONCTION   prefpls              ################################################

Factoprefpls<-function(){
    require(tcltk)
    require(FactoMineR)
    top<-tktoplevel(borderwidth=5)
    tkwm.title(top,"Scatter plot and additional variables")
    donnee<-get(.activeDataSet)
    nomdonnee<-.activeDataSet

######## FRAMES -----------------------------------------------------------------------------------------------------------------------


descFrame <- tkframe(top)
    listdesc<-tklistbox(descFrame,selectmode="extended",yscrollcommand=function(...) tkset(scrdesc,...))
    scrdesc <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    tkgrid(listdesc, scrdesc,sticky = "nw")
    tkgrid.configure(scrdesc, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")

   vars<-colnames(donnee)
   vars.desc = NULL
   for (i in (1:ncol(donnee))){
     if (is.numeric(donnee[,i])){
       tkinsert(listdesc,"end",vars[i])
       vars.desc = c(vars.desc,vars[i])
     }
   }

interestFrame <- tkframe(top)
    col.lab<-tklabel(interestFrame,text="'x' variable: ",fg="darkgreen")
    col.val<-tclVar("")
    colj <- tkentry(interestFrame,width=15,textvariable=col.val)
    col2.lab<-tklabel(interestFrame,text="'y' variable: ",fg="darkgreen")
    col2.val<-tclVar("")
    col2j <- tkentry(interestFrame,width=15,textvariable=col2.val)
    
    tkgrid(col.lab, colj,sticky = "nw")
    tkgrid(col2.lab, col2j,sticky = "nw")

optionsFrame <- tkframe(top,relief="ridge",borderwidth=2)
    indiv.check <- tkcheckbutton(top)
    indiv.bool <- tclVar("1")
    tkconfigure(indiv.check,variable=indiv.bool)
    indiv.lab<-tklabel(optionsFrame,text="Plot the graph of the individuals:")
    ortho.check <- tkcheckbutton(top)
    ortho.bool <- tclVar("0")
    tkconfigure(ortho.check,variable=ortho.bool)
    ortho.lab<-tklabel(optionsFrame,text="Same scale for 'x' and 'y' axes:")
    variab.check <- tkcheckbutton(top)
    variab.bool <- tclVar("1")
    tkconfigure(variab.check,variable=variab.bool)
    variab.lab<-tklabel(optionsFrame,text="Plot the graph of the additional variables:")

    tkgrid(indiv.lab,indiv.check,sticky="w")
    tkgrid(ortho.lab,ortho.check,sticky="w")
    tkgrid(tklabel(optionsFrame,text=""))
    tkgrid(variab.lab,variab.check,sticky="w")

doubleclick<-function(){
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    if(col.nam==""){
      col.val <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
      tkinsert(colj,"end",col.val)}
    else{
      if(col2.nam==""){
        col2.val <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
        tkinsert(col2j,"end",col2.val)
      }
    }
}
tkbind(listdesc,"<Double-ButtonPress-1>",doubleclick)

###### Fonction principale qui lance prefpls sans fermer la fenêtre------------------------------------------------------------------------------------------------------------
App<-function(){
    variable<-vars.desc[as.numeric(tkcurselection(listdesc))+1]
    if (length(variable)<2) variable = vars.desc
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    if (col.nam=="") tkmessageBox(message="No variable selected for the 'x' axis",icon="warning",type="ok")
    if (col2.nam=="") tkmessageBox(message="No variable selected for the 'y' axis",icon="warning",type="ok")
    done = 0
    if ((col.nam!="")&(col2.nam!="")){
      done = 1
      if (!(col2.nam%in%variable)) variable <- c(col2.nam,variable)
      if (!(col.nam%in%variable)) variable <- c(col.nam,variable)
      for (i in 1:length(variable)){
        if (variable[i]==col.nam) col.pos<-i
        if (variable[i]==col2.nam) col2.pos<-i
      }
      commande.data<-paste(nomdonnee,'.aux', '<-', nomdonnee,'[, c("', paste(variable, collapse='", "'), '")]',sep='')
      justDoIt(commande.data)
      logger(commande.data)
      if (tclvalue(indiv.bool)==1){
        if (tclvalue(ortho.bool)!=1) doItAndPrint(paste('prefpls(',nomdonnee,'.aux, var1=',col.pos,', var2=',col2.pos,', choix = "ind", asp=NA)',sep=''))
        else doItAndPrint(paste('prefpls(',nomdonnee,'.aux, var1=',col.pos,', var2=',col2.pos,', choix = "ind", asp=1)',sep=''))
      }
      if (tclvalue(variab.bool)==1) doItAndPrint(paste('prefpls(',nomdonnee,'.aux, var1=',col.pos,', var2=',col2.pos,', choix = "var")',sep=''))
      justDoIt(paste('remove(',nomdonnee,'.aux)',sep=""))
      logger(paste('remove(',nomdonnee,'.aux)',sep=""))
    }
    return(done)
}


###### Fonction principale qui lance prefpls et ferme la fenêtre------------------------------------------------------------------------------------------------------------
onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}


##### Positionnement des widgets et frames sur la fenêtre 'top' ------------------------------------------------------------------------------------------
App.but <- tkbutton(top,borderwidth=3,width=12,text="Submit",command=App,fg="blue")

OKCancelHelp(helpSubject="prefpls")
tkgrid(tklabel(top, text = "Variables (double-click to select the two variables)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descFrame,interestFrame,sticky="w")
tkgrid(tklabel(top,text=""))
tkbind(listdesc,"<Double-ButtonPress-1>",doubleclick)
tkgrid(optionsFrame,sticky="w")
tkgrid(App.but,sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(buttonsFrame, columnspan=2)
tkfocus(top)
}

######################################################           FIN FONCTION prefpls         #############################################################
#########################################################################################################################################################

#############################################################################
#                FONCTION HMFA                                              #
#############################################################################
FactoHMFA<-function(){
require(tcltk)
require(FactoMineR)
top<-tktoplevel()   # Création de la fenêtre principale
tkwm.title(top,"HMFA") # Titre de la fenêtre principale
ListeFrame<-tkframe(top, borderwidth=2)

################### Initialisation ##########################
listdesc<-tklistbox(ListeFrame,selectmode="extended",yscrollcommand=function(...) tkset(scrdesc,...),width=35,height=15)# Liste vide
scrdesc<- tkscrollbar(ListeFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...)) # scrollbar associé à la liste des variables
listfact<-tklistbox(ListeFrame,selectmode="extended",yscrollcommand=function(...) tkset(scrfact,...),width=35,height=15)# Liste vide
scrfact<- tkscrollbar(ListeFrame,repeatinterval=5,command=function(...)tkyview(listfact,...)) # scrollbar associé à la liste des variables

listnom<-listnom2<-list()
type<-Rtype<-Htype<-NULL
name<-Rname.group<-groupsup<-NULL
j<-1
Rind.sup<-ind.sup<-NULL
CompteurOption<-0
Sncp<-5
Sgraph = TRUE
HHH<-1
NOM<-list()
A<-B<-C<-NULL
Htype<-list()

#################### definition de donnee ######################
donnee<-get(.activeDataSet)
vars<-colnames(donnee)
rows<-rownames(donnee)

##############  TRI DE LA TABLE EN QUALI puis QUANTI  ##########
listnomindillu<-list()
TableFin = NULL
NomDesVar<-NULL
if (length(listFactors())>0){
  listnomfact <- listFactors()
  TableFin <- donnee[,listnomfact]
  listnomfact = cbind(t(listnomfact))
  NomDesVar = listnomfact
}
if (length(listNumeric())>0){
  listnomdesc <- listNumeric()
  if (is.null(TableFin)) TableFin <- donnee[,listnomdesc]
  else TableFin <- cbind(TableFin,donnee[,listnomdesc])
  listnomdesc = cbind(t(listnomdesc))
  NomDesVar<-cbind(NomDesVar, listnomdesc)
}
for (i in (1:nrow(donnee))){
    if (is.null(listnomindillu)) listnomindillu<-list(rows[i])
    else listnomindillu<-(cbind(listnomindillu,list(rows[i])))
}

colnames(TableFin) <- NomDesVar
rownames(TableFin) <- listnomindillu

############# On renseigne les listes ##########################
vars2<-colnames(TableFin)
rows2<-rownames(TableFin)
nbfac=0
nbdesc=0
for (i in (1:ncol(TableFin))){
  if (is.numeric(TableFin[,i])){
    tkinsert(listdesc,"end",vars2[i])
    nbdesc=nbdesc+1
}}

for (i in (1:ncol(TableFin))){
  if (is.factor(TableFin[,i])){
    nbfac=nbfac+1
    tkinsert(listfact,"end",vars2[i])
}}


##################################################
  nom.correct<-function(text, liste=NULL)
  {
    text<-chartr("^\ ", "...", text)
    if(!is.null(liste)) {
      while(text %in% liste) text<-paste(text, ".bis", sep="")
    }
    return(text)  
  }
  supprimeQuanti.funct<-defmacro(label, expr=
  {
    env<-environment()
    OnSGQ<-function()
    {
      grpeActSupprime<-listQuantiAct.nom[as.numeric(tkcurselection(listQuantiAct))+1]
      if(length(grpeActSupprime)>=1) { 
        listQuantiAct.nom.tmp<-listQuantiAct.nom[-which(listQuantiAct.nom %in% grpeActSupprime)]
        assign("listQuantiAct.nom",listQuantiAct.nom.tmp, envir=env)
        tkdelete(listQuantiAct,"0","end")
        if(length(listQuantiAct.nom)>=1) {
          for (grpe in listQuantiAct.nom) tkinsert(listQuantiAct, "end", grpe)
        }  
      }
      grpeIlluSupprime<-listQuantiIllu.nom[as.numeric(tkcurselection(listQuantiIllu))+1]
      if(length(grpeIlluSupprime)>=1) { 
        listQuantiIllu.nom.tmp<-listQuantiIllu.nom[-which(listQuantiIllu.nom %in% grpeIlluSupprime)]
        assign("listQuantiIllu.nom",listQuantiIllu.nom.tmp, envir=env)
        tkdelete(listQuantiIllu,"0","end")
        if(length(listQuantiIllu.nom)>=1) {
          for (grpe in listQuantiIllu.nom) tkinsert(listQuantiIllu, "end", grpe)
        }  
      }
      nb.grpe<-length(listQuantiAct.nom) + length(listQuantiIllu.nom)
      if (nb.grpe>=1) {
        tclvalue(label.quantiFrame.var)<-paste(nb.grpe, gettextRcmdr("quantitative groups"), sep=" ")
        tkconfigure(label.quantiFrame)
      }  
      else {
        tclvalue(label.quantiFrame.var)<-paste("0", gettextRcmdr("quantitative group"), sep=" ")
        tkconfigure(label.quantiFrame)
      }        
    }
    
    SupGpeQuantiFrame<-tkframe(ListeQuantiFrame)
    SupGpeQuanti.but<-tkbutton(SupGpeQuantiFrame, textvariable=tclVar(label), command=OnSGQ, borderwidth=3)
   tkgrid(SupGpeQuanti.but, sticky="ew")  
  })


  #! Ajout d'un groupe quantitatif
  ajoutQuanti.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.GQ<-1
    .AjoutQuantiLabel<-tclVar(paste(firstLabel, "!", sep=" "))
    OnAGQ<-function()
    {
      AjoutGpeQuantiWin<-tktoplevel()
      tkwm.title(AjoutGpeQuantiWin,gettextRcmdr("Definition of a quantitatif group"))
      
      #création de la fonction AGA.OK
      AGQ.OK<-function()
      {
        assign("compteur.GQ", compteur.GQ+1, envir=env)
        nom.groupe<-nom.correct(tclvalue(nomGrpeQuanti.val), liste=c(listQuantiAct.nom,listQuantiIllu.nom))
        if (nom.groupe=="") tkmessageBox(message=gettextRcmdr("Name for the group"), icon="warning", type="ok") 
        else {
          varGroupe<-listVarQuanti.nom[as.numeric(tkcurselection(listVarQuanti))+1]
          if (length(varGroupe)>=1) {
            if(tclvalue(norm.Value)=="ok") assign(paste(nom.groupe,".var", sep=""), c("s", varGroupe), envir=env)
            if(tclvalue(norm.Value)=="nok") assign(paste(nom.groupe,".var", sep=""),c("c", varGroupe), envir=env)
            if(tclvalue(etat.Value)=="actif") {  
              tkinsert(listQuantiAct,"end",nom.groupe)
              assign("listQuantiAct.nom", c(listQuantiAct.nom, nom.groupe),envir=env)
            }  
            if(tclvalue(etat.Value)=="illu") {
              tkinsert(listQuantiIllu,"end",nom.groupe)
              assign("listQuantiIllu.nom",c(listQuantiIllu.nom, nom.groupe),envir=env)
              
            }
            tclvalue(label.quantiFrame.var)<-paste(length(listQuantiAct.nom) + length(listQuantiIllu.nom), gettextRcmdr("quantitative groups"), sep=" ")
            tkconfigure(label.quantiFrame)  
            tkdestroy(AjoutGpeQuantiWin)
          }
        }  
      }
      
      
      # choix du nom du groupe
      nomGrpeQuanti.lab<-tklabel(AjoutGpeQuantiWin,text=gettextRcmdr("Name of the group: "))
      nomGrpeQuanti.val<-tclVar(paste("G", compteur.GQ, sep=""))
      nomGrpeQuanti<-tkentry(AjoutGpeQuantiWin,width=15,textvariable=nomGrpeQuanti.val)
      
      # choix de l'état actif ou illustratif
      etat.actif.check<-tkradiobutton(AjoutGpeQuantiWin)
      etat.illu.check<-tkradiobutton(AjoutGpeQuantiWin)
      etat.Value<-tclVar("actif")
      tkconfigure(etat.actif.check,variable=etat.Value,value="actif")
      tkconfigure(etat.illu.check,variable=etat.Value, value="illu")
      
      # choix de la normalisation ou non
      norm.ok.check<-tkradiobutton(AjoutGpeQuantiWin)
      norm.nok.check<-tkradiobutton(AjoutGpeQuantiWin)
      norm.Value<-tclVar("ok")
      tkconfigure(norm.ok.check,variable=norm.Value,value="ok")
      tkconfigure(norm.nok.check,variable=norm.Value, value="nok")
            
      # création de la liste pour le choix des variables acives
      listVarQuanti<-tklistbox(AjoutGpeQuantiWin, selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrVarQuanti,...))
      scrVarQuanti<-tkscrollbar(AjoutGpeQuantiWin,repeatinterval=5,command=function(...)tkyview(listVarQuanti,...))
      listVarQuanti.nom<-NULL
      for (i in (1:ncol(donnee))) {
        if (is.numeric(donnee[,i])) {
          tkinsert(listVarQuanti,"end",vars[i])
          listVarQuanti.nom<-c(listVarQuanti.nom, vars[i])
        }
      }
      
      AGQ.but<-tkbutton(AjoutGpeQuantiWin, text="OK", width=16, command=AGQ.OK)
      
      tkgrid(nomGrpeQuanti.lab, nomGrpeQuanti)
      tkgrid.configure(nomGrpeQuanti.lab, column=0, columnspan=2, sticky="w")
      tkgrid.configure(nomGrpeQuanti, column=2, columnspan=3)      
      tkgrid(tklabel(AjoutGpeQuantiWin, text=""))      
##      tkgrid(tklabel(AjoutGpeQuantiWin, text=gettextRcmdr("Status of the group:")), tklabel(AjoutGpeQuantiWin, text="Active"),etat.actif.check, tklabel(AjoutGpeQuantiWin, text=gettextRcmdr("Supplementary")),etat.illu.check, sticky="w")
##      tkgrid(tklabel(AjoutGpeQuantiWin, text=""))      
      tkgrid(tklabel(AjoutGpeQuantiWin, text=gettextRcmdr("Scaled the variable of the group:")), tklabel(AjoutGpeQuantiWin, text=gettextRcmdr("Yes")),norm.ok.check, tklabel(AjoutGpeQuantiWin, text=gettextRcmdr("No")),norm.nok.check, sticky="w")
      tkgrid(tklabel(AjoutGpeQuantiWin, text=""))
      tkgrid(tklabel(AjoutGpeQuantiWin, text = gettextRcmdr("Select the variables for the group"), fg = "blue"), column=0, columnspan = 5, sticky = "w")
      tkgrid(listVarQuanti, scrVarQuanti, sticky = "nw")
      tkgrid.configure(scrVarQuanti, sticky = "wns", column=4,columnspan=1)
      tkgrid.configure(listVarQuanti, sticky = "ew", column=0, columnspan=4)
      tkgrid(tklabel(AjoutGpeQuantiWin, text=""))
      tkgrid(AGQ.but, column=2,columnspan=1, sticky="ew")
      tkgrid.columnconfigure(AjoutGpeQuantiWin,0, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQuantiWin,1, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQuantiWin,2, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQuantiWin,3, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQuantiWin,4, minsize=55)             
   }
   GpeQuantiFrame<-tkframe(ListeQuantiFrame)
   AjoutGpeQuanti.but<-tkbutton(GpeQuantiFrame, textvariable=.AjoutQuantiLabel, command=OnAGQ, borderwidth=3)
   tkgrid(AjoutGpeQuanti.but, sticky="ew")
  })    
  


  #! Modification d'un groupe quantitatif
  modifQuanti.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    .ModifQuantiLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnMGQ<-function()
    {
      ModifGpeQuantiWin<-tktoplevel()
      tkwm.title(ModifGpeQuantiWin,gettextRcmdr("Modification of a quantitative group"))
      
      #création de la fonction AGA.OK
      MGQ.OK<-function()
      {
        nom.groupe<-nom.correct(tclvalue(nomModifGrpeQuanti.val), liste=c(listQuantiAct.nom,listQuantiIllu.nom))
        if (nom.groupe=="") tkmessageBox(message=gettextRcmdr("Name for the group"), icon="warning", type="ok") 
        else {
          if(etat=="actif") {
            listQuantiAct.nom.tmp<-listQuantiAct.nom[-which(listQuantiAct.nom== grpeAModifier)]
            assign("listQuantiAct.nom",listQuantiAct.nom.tmp, envir=env)
            tkdelete(listQuantiAct,"0","end")
            for (grpe in listQuantiAct.nom) tkinsert(listQuantiAct, "end", grpe)
          }
          
          if(etat=="illu")  {
            listQuantiIllu.nom.tmp<-listQuantiIllu.nom[-which(listQuantiIllu.nom== grpeAModifier)]
            assign("listQuantiIllu.nom",listQuantiIllu.nom.tmp, envir=env)
            tkdelete(listQuantiIllu,"0","end")
            for (grpe in listQuantiIllu.nom) tkinsert(listQuantiIllu, "end", grpe)
          }
          
          varGroupe<-listModifVarQuanti.nom[as.numeric(tkcurselection(listModifVarQuanti))+1]
          if (length(varGroupe)>=1) {
            if(tclvalue(normModif.Value)=="ok") assign(paste(nom.groupe,".var", sep=""), c("s", varGroupe), envir=env)
            if(tclvalue(normModif.Value)=="nok") assign(paste(nom.groupe,".var", sep=""),c("c", varGroupe), envir=env)
            if(tclvalue(etatModif.Value)=="actif") {  
              tkinsert(listQuantiAct,"end",nom.groupe)
              assign("listQuantiAct.nom", c(listQuantiAct.nom, nom.groupe),envir=env)
            }  
            if(tclvalue(etatModif.Value)=="illu") {
              tkinsert(listQuantiIllu,"end",nom.groupe)
              assign("listQuantiIllu.nom",c(listQuantiIllu.nom, nom.groupe),envir=env)              
            }
              
            tkdestroy(ModifGpeQuantiWin)
          }
        }  
      }
            
      if(length(as.numeric(tkcurselection(listQuantiAct)))>=1) {
        grpeAModifier<-listQuantiAct.nom[as.numeric(tkcurselection(listQuantiAct))+1][1]
        etat<-"actif"
      }
      else if (length(as.numeric(tkcurselection(listQuantiIllu)))>=1) {
        grpeAModifier<-listQuantiIllu.nom[as.numeric(tkcurselection(listQuantiIllu))+1][1]
        etat<-"illu"
      }
      else  {
        tkdestroy(ModifGpeQuantiWin)
        return()
      }  
        
      eval(parse(text=paste("grpeAModifier.var<-",paste(grpeAModifier,".var", sep=""),sep="")))
      # choix du nom du groupe
      nomModifGrpeQuanti.lab<-tklabel(ModifGpeQuantiWin,text=gettextRcmdr("Name of the group: "))
      nomModifGrpeQuanti.val<-tclVar(grpeAModifier)
      nomModifGrpeQuanti<-tkentry(ModifGpeQuantiWin,width=15,textvariable=nomModifGrpeQuanti.val)
      
      # choix de l'état actif ou illustratif
      etatModif.actif.check<-tkradiobutton(ModifGpeQuantiWin)
      etatModif.illu.check<-tkradiobutton(ModifGpeQuantiWin)
      etatModif.Value<-tclVar(etat)
      tkconfigure(etatModif.actif.check,variable=etatModif.Value,value="actif")
        tkconfigure(etatModif.illu.check,variable=etatModif.Value, value="illu")
      
      # choix de la normalisation ou non
      normModif.ok.check<-tkradiobutton(ModifGpeQuantiWin)
      normModif.nok.check<-tkradiobutton(ModifGpeQuantiWin)
      if(grpeAModifier.var[1]=="s") normModif.Value<-tclVar("ok")
      else normModif.Value<-tclVar("nok")
      tkconfigure(normModif.ok.check,variable=normModif.Value,value="ok")
        tkconfigure(normModif.nok.check,variable=normModif.Value, value="nok")
            
      # création de la liste pour le choix des variables acives
      listModifVarQuanti<-tklistbox(ModifGpeQuantiWin, selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrModifVarQuanti,...))
      scrModifVarQuanti<-tkscrollbar(ModifGpeQuantiWin,repeatinterval=5,command=function(...)tkyview(listModifVarQuanti,...))
      listModifVarQuanti.nom<-NULL
      indice.num<-0
      for (i in (1:ncol(donnee))) {
        if (is.numeric(donnee[,i])) {
            tkinsert(listModifVarQuanti,"end",vars[i])
            listModifVarQuanti.nom<-c(listModifVarQuanti.nom, vars[i])
            if(vars[i] %in% grpeAModifier.var[-1]) tkselection.set(listModifVarQuanti, indice.num)
            indice.num<-indice.num+1
        }
      }
      
      MGQ.but<-tkbutton(ModifGpeQuantiWin, text="OK", width=16, command=MGQ.OK)
      
      tkgrid(nomModifGrpeQuanti.lab, nomModifGrpeQuanti)
      tkgrid.configure(nomModifGrpeQuanti.lab, column=0, columnspan=2, sticky="w")
      tkgrid.configure(nomModifGrpeQuanti, column=2, columnspan=3)      
      tkgrid(tklabel(ModifGpeQuantiWin, text=""))      
##      tkgrid(tklabel(ModifGpeQuantiWin, text=gettextRcmdr("Status of the group:")), tklabel(ModifGpeQuantiWin, text=gettextRcmdr("Active")),etatModif.actif.check, tklabel(ModifGpeQuantiWin, text=gettextRcmdr("Supplementary")),etatModif.illu.check, sticky="w")
##      tkgrid(tklabel(ModifGpeQuantiWin, text=""))      

      tkgrid(tklabel(ModifGpeQuantiWin, text=gettextRcmdr("Scaled the variables of the group:")), tklabel(ModifGpeQuantiWin, text="Yes"),normModif.ok.check, tklabel(ModifGpeQuantiWin, text=gettextRcmdr("No")),normModif.nok.check, sticky="w")
      tkgrid(tklabel(ModifGpeQuantiWin, text=""))
      tkgrid(tklabel(ModifGpeQuantiWin, text = gettextRcmdr("Select the variables of the group"), fg = "blue"), column=0, columnspan = 5, sticky = "w")
      tkgrid(listModifVarQuanti, scrModifVarQuanti, sticky = "nw")
      tkgrid.configure(scrModifVarQuanti, sticky = "wns", column=4,columnspan=1)
      tkgrid.configure(listModifVarQuanti, sticky = "ew", column=0, columnspan=4)
      tkgrid(tklabel(ModifGpeQuantiWin, text=""))
      tkgrid(MGQ.but, column=2,columnspan=1, sticky="ew")
      tkgrid.columnconfigure(ModifGpeQuantiWin,0, minsize=55)
      tkgrid.columnconfigure(ModifGpeQuantiWin,1, minsize=55)
      tkgrid.columnconfigure(ModifGpeQuantiWin,2, minsize=55)
      tkgrid.columnconfigure(ModifGpeQuantiWin,3, minsize=55)
      tkgrid.columnconfigure(ModifGpeQuantiWin,4, minsize=55)             
   }
   ModifGpeQuantiFrame<-tkframe(ListeQuantiFrame)
   ModifGpeQuanti.but<-tkbutton(ModifGpeQuantiFrame, textvariable=.ModifQuantiLabel, command=OnMGQ, borderwidth=3)
   tkgrid(ModifGpeQuanti.but, sticky="ew")
  })    
  
  #! Suppression de groupes qualitatifs
  supprimeQuali.funct<-defmacro(label, expr=
  {
    env<-environment()
    OnSGQl<-function()
    {
      grpeActSupprime<-listQualiAct.nom[as.numeric(tkcurselection(listQualiAct))+1]
      if(length(grpeActSupprime)>=1) { 
        listQualiAct.nom.tmp<-listQualiAct.nom[-which(listQualiAct.nom %in% grpeActSupprime)]
        assign("listQualiAct.nom",listQualiAct.nom.tmp, envir=env)
        tkdelete(listQualiAct,"0","end")
        if(length(listQualiAct.nom)>=1) {
          for (grpe in listQualiAct.nom) tkinsert(listQualiAct, "end", grpe)
        }  
      }
      grpeIlluSupprime<-listQualiIllu.nom[as.numeric(tkcurselection(listQualiIllu))+1]
      if(length(grpeIlluSupprime)>=1) { 
        listQualiIllu.nom.tmp<-listQualiIllu.nom[-which(listQualiIllu.nom %in% grpeIlluSupprime)]
        assign("listQualiIllu.nom",listQualiIllu.nom.tmp, envir=env)
        tkdelete(listQualiIllu,"0","end")
        if(length(listQualiIllu.nom)>=1) {
          for (grpe in listQualiIllu.nom) tkinsert(listQualiIllu, "end", grpe)
        }  
      }
      nb.grpe<-length(listQualiAct.nom) + length(listQualiIllu.nom)
      if (nb.grpe>=1) {
        tclvalue(label.qualiFrame.var)<-paste(nb.grpe, gettextRcmdr("qualitative groups"), sep=" ")
        tkconfigure(label.qualiFrame)
      }  
      else {
        tclvalue(label.qualiFrame.var)<-paste("0", gettextRcmdr("qualitative group"), sep=" ")
        tkconfigure(label.qualiFrame)
      }  
    }
    
    SupGpeQualiFrame<-tkframe(ListeQualiFrame)
    SupGpeQuali.but<-tkbutton(SupGpeQualiFrame, textvariable=tclVar(label), command=OnSGQl, borderwidth=3)
   tkgrid(SupGpeQuali.but, sticky="ew")  
  })

        
  #! Ajout d'un groupe qualitatif 
  ajoutQuali.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.GQl<-1
    nbGrpeQualiAct<-0
    nbGrpeQualiIllu<-0
    .AjoutQualiLabel<-tclVar(paste(firstLabel, "!", sep=" "))
    OnAGQl<-function()
    {
      AjoutGpeQualiWin<-tktoplevel()
      tkwm.title(AjoutGpeQualiWin,gettextRcmdr("Construction of a qualitative group"))
      
      #création de la fonction AGA.OK
      AGQl.OK<-function()
      {
        assign("compteur.GQl", compteur.GQl+1, envir=env)
        nom.groupe<-nom.correct(tclvalue(nomGrpeQuali.val), liste=c(listQualiAct.nom,listQualiIllu.nom))
        if (nom.groupe=="") tkmessageBox(message=gettextRcmdr("Name for the group"), icon="warning", type="ok") 
        else
        {
          varGroupe<-listVarQuali.nom[as.numeric(tkcurselection(listVarQuali))+1]
          if (length(varGroupe)>=1) {
            assign(paste(nom.groupe,".var", sep=""), c("n",varGroupe), envir=env)
            if(tclvalue(etat.Value)=="actif") {  
              tkinsert(listQualiAct,"end",nom.groupe)
              assign("listQualiAct.nom", c(listQualiAct.nom, nom.groupe),envir=env)
            }  
            if(tclvalue(etat.Value)=="illu") {
              tkinsert(listQualiIllu,"end",nom.groupe)
              assign("listQualiIllu.nom",c(listQualiIllu.nom, nom.groupe),envir=env)
            }  
            tclvalue(label.qualiFrame.var)<-paste(length(listQualiAct.nom) + length(listQualiIllu.nom), "qualitative groups", sep=" ")
            tkconfigure(label.qualiFrame) 
            tkdestroy(AjoutGpeQualiWin)
          }
        }  
      }
      
      
      # choix du nom du groupe
      nomGrpeQuali.lab<-tklabel(AjoutGpeQualiWin,text=gettextRcmdr("Name of the group: "))
      nomGrpeQuali.val<-tclVar(paste("Gq", compteur.GQl, sep=""))
      nomGrpeQuali<-tkentry(AjoutGpeQualiWin,width=15,textvariable=nomGrpeQuali.val)
      
      # choix de l'état actif ou illustratif
      etat.actif.check<-tkradiobutton(AjoutGpeQualiWin)
      etat.illu.check<-tkradiobutton(AjoutGpeQualiWin)
      etat.Value<-tclVar("actif")
      tkconfigure(etat.actif.check,variable=etat.Value,value="actif")
      tkconfigure(etat.illu.check,variable=etat.Value, value="illu")
      
      # création de la liste pour le choix des variables acives
      listVarQuali<-tklistbox(AjoutGpeQualiWin, selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrVarQuali,...))
      scrVarQuali<-tkscrollbar(AjoutGpeQualiWin,repeatinterval=5,command=function(...)tkyview(listVarQuali,...))
      listVarQuali.nom<-NULL
      for (i in (1:ncol(donnee))) {
        if (is.factor(donnee[,i])) {
          tkinsert(listVarQuali,"end",vars[i])
          listVarQuali.nom<-c(listVarQuali.nom, vars[i])
        }
      }
      
      AGQl.but<-tkbutton(AjoutGpeQualiWin, text="OK", width=16, command=AGQl.OK)
      
      tkgrid(nomGrpeQuali.lab, nomGrpeQuali)
      tkgrid.configure(nomGrpeQuali.lab, column=0, columnspan=2, sticky="w")
      tkgrid.configure(nomGrpeQuali, column=2, columnspan=3)      
      tkgrid(tklabel(AjoutGpeQualiWin, text=""))      
##      tkgrid(tklabel(AjoutGpeQualiWin, text=gettextRcmdr("Status of the group:")), tklabel(AjoutGpeQualiWin, text=gettextRcmdr("Active")),etat.actif.check, tklabel(AjoutGpeQualiWin, text=gettextRcmdr("Supplementary")),etat.illu.check, sticky="w")
##      tkgrid(tklabel(AjoutGpeQualiWin, text=""))
      tkgrid(tklabel(AjoutGpeQualiWin, text = gettextRcmdr("Select the variables of the group"), fg = "blue"), column=0, columnspan = 5, sticky = "w")
      tkgrid(listVarQuali, scrVarQuali, sticky = "nw")
      tkgrid.configure(scrVarQuali, sticky = "wns", column=4,columnspan=1)
      tkgrid.configure(listVarQuali, sticky = "ew", column=0, columnspan=4)
      tkgrid(tklabel(AjoutGpeQualiWin, text=""))
      tkgrid(AGQl.but, column=2,columnspan=1, sticky="ew")
      tkgrid.columnconfigure(AjoutGpeQualiWin,0, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQualiWin,1, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQualiWin,2, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQualiWin,3, minsize=55)
      tkgrid.columnconfigure(AjoutGpeQualiWin,4, minsize=55)              
   }
   GpeQualiFrame<-tkframe(ListeQualiFrame)
   AjoutGpeQuali.but<-tkbutton(GpeQualiFrame, textvariable=.AjoutQualiLabel, command=OnAGQl, borderwidth=3)
   tkgrid(AjoutGpeQuali.but, sticky="ew")
  })    


    #! Modification d'un groupe qualitatif
  modifQuali.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    .ModifQualiLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnMGQl<-function()
    {
      ModifGpeQualiWin<-tktoplevel()
      tkwm.title(ModifGpeQualiWin,gettextRcmdr("Modification of a qualitative group"))
      
      #création de la fonction AGA.OK
      MGQl.OK<-function()
      {
        nom.groupe<-nom.correct(tclvalue(nomModifGrpeQuali.val), liste=c(listQualiAct.nom,listQualiIllu.nom))
        if (nom.groupe=="") tkmessageBox(message=gettextRcmdr("Give a name for the group"), icon="warning", type="ok") 
        else {
          if(etat=="actif") {
            listQualiAct.nom.tmp<-listQualiAct.nom[-which(listQualiAct.nom== grpeAModifier)]
            assign("listQualiAct.nom",listQualiAct.nom.tmp, envir=env)
            tkdelete(listQualiAct,"0","end")
            for (grpe in listQualiAct.nom) tkinsert(listQualiAct, "end", grpe)
          }          
          if(etat=="illu") {
            listQualiIllu.nom.tmp<-listQualiIllu.nom[-which(listQualiIllu.nom== grpeAModifier)]
            assign("listQualiIllu.nom",listQualiIllu.nom.tmp, envir=env)
            tkdelete(listQualiIllu,"0","end")
            for (grpe in listQualiIllu.nom) tkinsert(listQualiIllu, "end", grpe)
          }          
          varGroupe<-listModifVarQuali.nom[as.numeric(tkcurselection(listModifVarQuali))+1]
          if (length(varGroupe)>=1) {
            assign(paste(nom.groupe,".var", sep=""), c("n", varGroupe), envir=env)
            if(tclvalue(etatModif.Value)=="actif") {  
              tkinsert(listQualiAct,"end",nom.groupe)
              assign("listQualiAct.nom", c(listQualiAct.nom, nom.groupe),envir=env)
            }  
            if(tclvalue(etatModif.Value)=="illu") {
              tkinsert(listQualiIllu,"end",nom.groupe)
              assign("listQualiIllu.nom",c(listQualiIllu.nom, nom.groupe),envir=env)              
            }
            tkdestroy(ModifGpeQualiWin)
          }
        }  
      }
      
      if(length(as.numeric(tkcurselection(listQualiAct)))>=1) {
        grpeAModifier<-listQualiAct.nom[as.numeric(tkcurselection(listQualiAct))+1][1]
        etat<-"actif"
      }
      else if (length(as.numeric(tkcurselection(listQualiIllu)))>=1) {
        grpeAModifier<-listQualiIllu.nom[as.numeric(tkcurselection(listQualiIllu))+1][1]
        etat<-"illu"
      }
      else  {
        tkdestroy(ModifGpeQualiWin)
        return()
      }  
        
       eval(parse(text=paste("grpeAModifier.var<-",paste(grpeAModifier,".var", sep=""),sep="")))
      # choix du nom du groupe
      nomModifGrpeQuali.lab<-tklabel(ModifGpeQualiWin,text=gettextRcmdr("Name of the group: "))
      nomModifGrpeQuali.val<-tclVar(grpeAModifier)
      nomModifGrpeQuali<-tkentry(ModifGpeQualiWin,width=15,textvariable=nomModifGrpeQuali.val)
      
      # choix de l'état actif ou illustratif
      etatModif.actif.check<-tkradiobutton(ModifGpeQualiWin)
      etatModif.illu.check<-tkradiobutton(ModifGpeQualiWin)
      etatModif.Value<-tclVar(etat)
      tkconfigure(etatModif.actif.check,variable=etatModif.Value,value="actif")
      tkconfigure(etatModif.illu.check,variable=etatModif.Value, value="illu")
      
      # création de la liste pour le choix des variables acives
      listModifVarQuali<-tklistbox(ModifGpeQualiWin, selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrModifVarQuali,...))
      scrModifVarQuali<-tkscrollbar(ModifGpeQualiWin,repeatinterval=5,command=function(...)tkyview(listModifVarQuali,...))
      listModifVarQuali.nom<-NULL
      indice.num<-0
      for (i in (1:ncol(donnee))) {
        if (is.factor(donnee[,i])) {
          tkinsert(listModifVarQuali,"end",vars[i])
          listModifVarQuali.nom<-c(listModifVarQuali.nom, vars[i])
          if(vars[i] %in% grpeAModifier.var[-1]) tkselection.set(listModifVarQuali, indice.num)
          indice.num<-indice.num+1
        }
      }
      
      MGQl.but<-tkbutton(ModifGpeQualiWin, text="OK", width=16, command=MGQl.OK)
      
      tkgrid(nomModifGrpeQuali.lab, nomModifGrpeQuali)
      tkgrid.configure(nomModifGrpeQuali.lab, column=0, columnspan=2, sticky="w")
      tkgrid.configure(nomModifGrpeQuali, column=2, columnspan=3)      
      tkgrid(tklabel(ModifGpeQualiWin, text=""))      
##      tkgrid(tklabel(ModifGpeQualiWin, text=gettextRcmdr("Status of the group:")), tklabel(ModifGpeQualiWin, text=gettextRcmdr("Active")),etatModif.actif.check, tklabel(ModifGpeQualiWin, text=gettextRcmdr("Supplementary")),etatModif.illu.check, sticky="w")
##      tkgrid(tklabel(ModifGpeQualiWin, text=""))      
      tkgrid(tklabel(ModifGpeQualiWin, text = gettextRcmdr("Select the variables of the group"), fg = "blue"), column=0, columnspan = 5, sticky = "w")
      tkgrid(listModifVarQuali, scrModifVarQuali, sticky = "nw")
      tkgrid.configure(scrModifVarQuali, sticky = "wns", column=4,columnspan=1)
      tkgrid.configure(listModifVarQuali, sticky = "ew", column=0, columnspan=4)
      tkgrid(tklabel(ModifGpeQualiWin, text=""))
      tkgrid(MGQl.but, column=2,columnspan=1, sticky="ew")
      tkgrid.columnconfigure(ModifGpeQualiWin,0, minsize=55)
      tkgrid.columnconfigure(ModifGpeQualiWin,1, minsize=55)
      tkgrid.columnconfigure(ModifGpeQualiWin,2, minsize=55)
      tkgrid.columnconfigure(ModifGpeQualiWin,3, minsize=55)
      tkgrid.columnconfigure(ModifGpeQualiWin,4, minsize=55)              
   }
   ModifGpeQualiFrame<-tkframe(ListeQualiFrame)
   ModifGpeQuali.but<-tkbutton(ModifGpeQualiFrame, textvariable=.ModifQualiLabel, command=OnMGQl, borderwidth=3)
   tkgrid(ModifGpeQuali.but, sticky="ew")
  })    
  

  #! fonction pour le choix des individus supplémentaires 
  Iillu.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    individuillu<-NULL
    .IilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
    OnIillu<-function()
    {   
      IilluWin<-tktoplevel()
      tkwm.title(IilluWin,gettextRcmdr("Select supplementary individuals"))
      #création de la fonction IOK.funct
      IOK.funct<-function()
      {
        ind.select<-rows[as.numeric(tkcurselection(listind))+1]
        if(length(ind.select)==0) {
          assign("individuillu", NULL, envir=env)
          tclvalue(.IilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(Iillu.but, fg="black")
          tkdestroy(IilluWin)
          return()
        }
        assign("individuillu", ind.select, envir=env)
        tclvalue(.IilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(Iillu.but, fg="blue")
        tkdestroy(IilluWin)
      }
      
      # création et mise en page de la fenetre Fillu
      listind<-tklistbox(IilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrind,...)) # Liste vide
      scrind <-tkscrollbar(IilluWin,repeatinterval=5,command=function(...)tkyview(listind,...)) 
      indice<-0
      for (i in (1:nrow(donnee))) {
        tkinsert(listind,"end",rows[i]) # On renseigne la liste
        if(rows[i] %in% individuillu) tkselection.set(listind, indice)
        indice<-indice+1
      }
  
      IOK.but<-tkbutton(IilluWin, text="OK", width=16,command=IOK.funct)

      tkgrid(tklabel(IilluWin, text=""))
      tkgrid(tklabel(IilluWin, text = gettextRcmdr("Select supplementary individuals"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
      tkgrid(listind, scrind, sticky = "nw")
      tkgrid.configure(scrind, sticky = "ens", columnspan=1)
      tkgrid.configure(listind, sticky = "ew", column=1, columnspan=1)
      tkgrid(tklabel(IilluWin, text=""))
      tkgrid(IOK.but, column=1,columnspan=1, sticky="ew")
      tkgrid(tklabel(IilluWin, text=""))
      tkgrid.columnconfigure(IilluWin,0, minsize=25)
      tkgrid.columnconfigure(IilluWin,2, minsize=25)
  }  

   IilluFrame<-tkframe(IlluFrame)
   Iillu.but<-tkbutton(IilluFrame, textvariable=.IilluLabel, command=OnIillu, borderwidth=3)
   tkgrid(Iillu.but, sticky="ew")
  })
    
##################################################


################################################################
ValideGroupe1.funct<-function(){
  if (HHH==2){  
     a<-c(tclvalue(tkcurselection(listgroupe)))
     a<-unlist(strsplit(a,"\\ "))
     ChoixGroupe<<-(as.numeric(tkcurselection(listgroupe))+1)
     if (is.null(B)) B<<-list(ChoixGroupe) else B<<-c(B, list(ChoixGroupe))
  }
  if (HHH==3){
     a<-c(tclvalue(tkcurselection(listinter)))
     a<-unlist(strsplit(a,"\\ "))
     ChoixInter<<-(as.numeric(tkcurselection(listinter))+1)
     if (is.null(C)) C<<-list(ChoixInter) else C<<-c(C, list(ChoixInter))
  }
}

################################################################
Next.funct<-function(){
  if (HHH>2) print("go")
  if (HHH==2){
            listinter<<-tklistbox(ListeFrame,selectmode="extended",yscrollcommand=function(...) tkset(scrinter,...),width=35,height=15)# Liste vide
            scrinter<<- tkscrollbar(ListeFrame,repeatinterval=5,command=function(...)tkyview(listinter,...)) # scrollbar associé à la liste des variables
            for (j in 1:length(B)) tkinsert(listinter,"end",paste("L2.G", j,sep=""))
            HHH<<-3
            tkdestroy(listgroupe)
            tkdestroy(scrgroupe)
            tkdestroy(Groupe.Lab)
            tkdestroy(NextFrame)

            tkgrid(Groupe2.Lab)
            tkgrid(tklabel(ListeFrame, text="           "), listinter, scrinter, tklabel(ListeFrame, text="           "), sticky = "nw")
            tkgrid.configure(scrinter, sticky = "wns")
            tkgrid.configure(listinter, sticky = "ew")

  }
  if (HHH==1){

  #### début HHH = 1 valide.group
    group<-NULL
    type<-NULL
    name.group<-NULL
    num.group.sup<-NULL
    variables<-NULL
    indice.grpe<-1
      #récupération des groupes quanti actif
    nb.GQA<-length(listQuantiAct.nom)
    if(nb.GQA>=1) {
      name.group<-c(name.group, listQuantiAct.nom)
      for(i in 1:nb.GQA) {
        eval(parse(text=paste("liste.var.GQA<-", listQuantiAct.nom[i], ".var", sep="")))
        group<-c(group, length(liste.var.GQA)-1)
        type<-c(type,liste.var.GQA[1])
        variables<-c(variables, liste.var.GQA[-1])
        indice.grpe<-indice.grpe+1
      }
    }
      #récupération des groupes quanti illustratif
    nb.GQI<-length(listQuantiIllu.nom)
    if(nb.GQI>=1) {
      name.group<-c(name.group, listQuantiIllu.nom)
      for(i in 1:nb.GQI) {
         eval(parse(text=paste("liste.var.GQI<-", listQuantiIllu.nom[i], ".var", sep="")))
        group<-c(group, length(liste.var.GQI)-1)
        type<-c(type,liste.var.GQI[1])
        variables<-c(variables, liste.var.GQI[-1])
        num.group.sup<-c(num.group.sup,indice.grpe)
        indice.grpe<-indice.grpe+1
      }
    }
      #récupération des groupes quali actif
    nb.GQlA<-length(listQualiAct.nom)
    if(nb.GQlA>=1) {
      name.group<-c(name.group, listQualiAct.nom)
      for(i in 1:nb.GQlA) {
        eval(parse(text=paste("liste.var.GQlA<-", listQualiAct.nom[i], ".var", sep="")))
        group<-c(group, length(liste.var.GQlA)-1)
        type<-c(type,liste.var.GQlA[1])
        variables<-c(variables, liste.var.GQlA[-1])
        indice.grpe<-indice.grpe+1
      }
    }
      #récupération des groupes quali illustratif
    nb.GQlI<-length(listQualiIllu.nom)
    if(nb.GQlI>=1) {
      name.group<-c(name.group, listQualiIllu.nom)
      for(i in 1:nb.GQlI) {
        eval(parse(text=paste("liste.var.GQlI<-", listQualiIllu.nom[i], ".var", sep="")))
        group<-c(group, length(liste.var.GQlI)-1)
        type<-c(type,liste.var.GQlI[1])
        variables<-c(variables, liste.var.GQlI[-1])
        num.group.sup<-c(num.group.sup,indice.grpe)
        indice.grpe<-indice.grpe+1
      }
    }
    group <<- group
    name.group <<-name.group
    variables <<- variables
    Htype <<- type
    A <<- list(1:group[1])
    for (j in 2:length(group)) A<<- c(A, list((sum(group[1:(j-1)])+1):sum(group[1:j])))

  #### Fin HHH = 1  valide.group

            listgroupe<<-tklistbox(ListeFrame,selectmode="extended",yscrollcommand=function(...) tkset(scrgroupe,...),width=35,height=15)# Liste vide
            scrgroupe<<- tkscrollbar(ListeFrame,repeatinterval=5,command=function(...)tkyview(listfact,...)) # scrollbar associé à la liste des variables
            for (j in 1:length(group)) tkinsert(listgroupe,"end",name.group[j])
            HHH<<-2
  tkdestroy(ListeQuantiFrame)
  tkdestroy(ligneB)
  tkdestroy(ListeQualiFrame)
  tkdestroy(ligneB)

tkgrid(Groupe.Lab,column=1)
tkgrid(tklabel(ListeFrame, text="           "), listgroupe, scrgroupe, tklabel(ListeFrame, text="           "), sticky = "nw")
tkgrid.configure(scrgroupe, sticky = "wns")
tkgrid.configure(listgroupe, sticky = "ew")
tkgrid(ValideGroupe1.but)
tkgrid(ValidFrame, sticky = "n",column=1)
tkgrid(NextFrame, sticky = "n",column=1)
tkgrid(BasFrame, sticky = "n",column=1)
tkgrid(Analyse.but)
tkgrid(AnalyseFrame, sticky="n",column=1)
  }
}

  #! fonction HCPC
  
  Hcpc.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    .HcpcLabel<-tclVar(paste(firstLabel, "", sep=" "))    
    compteur.hcpc<-0
    Rclassif<-0
    Rmeth <- -1
    Rconsolid<-0 
    Rgraphhcpc<-0  
    Rreshcpc<-0
    Rminhcpc<-3
    Rmaxhcpc<-10

    OnHCPC <- function()
    {

      HcpcWin<-tktoplevel()
      tkwm.title(HcpcWin, gettextRcmdr("HCPC options"))

      onOKHcpc <- function()
      {
        assign("compteur.hcpc", compteur.hcpc+1, envir=env) 
        if(compteur.hcpc>0) tclvalue(.HcpcLabel)<-paste(label, "", sep=" ")
        tkconfigure(Hcpc.but, fg="blue")
      
        if(tclvalue(methValue)=="interactive") assign("Rmeth", 0, envir=env)
        else assign("Rmeth", -1, envir=env)
        
        if(tclvalue(consolidValue)=="1") assign("Rconsolid",TRUE, envir=env)
        else assign("Rconsolid",FALSE,envir=env)
        
        if(tclvalue(graphhcpcValue)=="1") assign("Rgraphhcpc",TRUE,envir=env)
        else assign("Rgraphhcpc",FALSE,envir=env)
        
        if(tclvalue(reshcpcValue)=="1") assign("Rreshcpc",TRUE,envir=env)
        else assign("Rreshcpc",FALSE,envir=env)        
        
        assign("Rminhcpc",as.numeric(tclvalue(minhcpc)),envir=env)
        assign("Rmaxhcpc",as.numeric(tclvalue(maxhcpc)),envir=env)
        
        assign("Rclassif",TRUE,envir=env)
        tkdestroy(HcpcWin)
      }
      OKHcpc.but<-tkbutton(HcpcWin, text="OK", width=8,command=onOKHcpc)

      onCancelHcpc <- function()
      {
        assign("Rclassif",FALSE,envir=env)
        tkdestroy(HcpcWin)
      }
      CancelHcpc.but<-tkbutton(HcpcWin, text="Cancel", width=8,command=onCancelHcpc)
 
      tkgrid(tklabel(HcpcWin, text=""))
      tkgrid(tklabel(HcpcWin, text = gettextRcmdr("Hierarchical Clustering on Principal Components"), fg = "darkred"), column=1, columnspan = 8, sticky = "ew")      

      meth1 <- tkradiobutton (HcpcWin)
      meth1.lab <- tklabel(HcpcWin,text=gettextRcmdr("interactive"))
      meth2 <- tkradiobutton (HcpcWin)
      meth2.lab <- tklabel(HcpcWin,text=gettextRcmdr("automatic"))
      methValue <- tclVar("interactive")
      meth.lab <- tklabel(HcpcWin,text=gettextRcmdr("Choice of the number of clusters: "))
      tkconfigure(meth1,variable=methValue,value="interactive")
      tkconfigure(meth2,variable=methValue,value="automatic")

      minmaxhcpc.label<-tklabel(HcpcWin,text=gettextRcmdr("The optimal number of clusters is chosen between:"))

      minhcpc<-tclVar("3")
      maxhcpc<-tclVar("10")
      minhcpc.entry <-tkentry(HcpcWin,width="3",textvariable=minhcpc)
      maxhcpc.entry <-tkentry(HcpcWin,width="3",textvariable=maxhcpc)

      consolid.lab <- tklabel(HcpcWin,text=gettextRcmdr("Consolidate clusters "))
      consolid.check <- tkcheckbutton(HcpcWin)
      if(Rconsolid) consolidValue<-tclVar("1")
      else consolidValue<-tclVar("0")      
      tkconfigure(consolid.check,variable=consolidValue)
      
      graphhcpc.lab <- tklabel(HcpcWin,text=gettextRcmdr("Print graphs "))
      graphhcpc.check <- tkcheckbutton(HcpcWin)
      if(Rgraphhcpc) graphhcpcValue <- tclVar("1")
      else graphhcpcValue <- tclVar("0")
      tkconfigure(graphhcpc.check,variable=graphhcpcValue)   

      reshcpc.lab <- tklabel(HcpcWin,text=gettextRcmdr("Print results for clusters "))
      reshcpc.check <- tkcheckbutton(HcpcWin)
      if(Rreshcpc) reshcpcValue<-tclVar("1")
      else reshcpcValue <- tclVar("0")
      tkconfigure(reshcpc.check,variable=reshcpcValue)          
    
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr("Select options for the HCPC"), fg = "blue"), column=1, columnspan=8, sticky="we")
      tkgrid(tklabel(HcpcWin,text=""))
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr(paste('Clustering is performed on the first ', tclvalue(ncp.val), ' dimensions of the HMFA',sep=""))),column=1,columnspan=4,sticky="w")
      tkgrid(tklabel(HcpcWin,text=gettextRcmdr("(Change your choice in the main options to change this number)")),column=1,columnspan=4,sticky="w")
      tkgrid(tklabel(HcpcWin,text=""))  
      tkgrid(meth.lab,meth1.lab,meth1)
      tkgrid(meth2.lab,meth2)
      tkgrid(tklabel(HcpcWin,text=""))
      tkgrid(minmaxhcpc.label,minhcpc.entry , maxhcpc.entry)
      tkgrid(tklabel(HcpcWin,text=""))      
      tkgrid(consolid.lab,consolid.check)
      tkgrid(graphhcpc.lab,graphhcpc.check)
      tkgrid(reshcpc.lab,reshcpc.check) 
      tkgrid(tklabel(HcpcWin,text=""))     
      tkgrid(OKHcpc.but, CancelHcpc.but)
      tkgrid(tklabel(HcpcWin, text=""))
      
      tkgrid.configure(minmaxhcpc.label,meth.lab,consolid.lab,graphhcpc.lab,reshcpc.lab,column=1,columnspan=4,sticky="w")
      tkgrid.configure(minhcpc.entry,column=7,columnspan=1,sticky="e")
      tkgrid.configure(maxhcpc.entry,column=8,columnspan=1,sticky="w")
      tkgrid.configure(meth1,meth2,consolid.check,graphhcpc.check,reshcpc.check,column=8,sticky="e")
      tkgrid.configure(meth1.lab,column=6,columnspan=2,sticky="w")
      tkgrid.configure(meth2.lab,column=6,columnspan=2,sticky="w") 
      tkgrid.configure(OKHcpc.but,column=2,columnspan=1,sticky="w")
      tkgrid.configure(CancelHcpc.but,column=6,columnspan=1,sticky="e")

      tkgrid.columnconfigure(HcpcWin,0, minsize=3)
      tkgrid.columnconfigure(HcpcWin,5, minsize=5)
      tkgrid.columnconfigure(HcpcWin,8, minsize=3) 

}      
    Hcpc2Frame<-tkframe(HcpcFrame)
    Hcpc.but<-tkbutton(Hcpc2Frame, textvariable=.HcpcLabel, command=OnHCPC, borderwidth=3)
    tkgrid(Hcpc.but, sticky="ew")
}) 

  OptionFrame<-tkframe(top, borderwidth=2, relief="groove")
  resu.lab<-tklabel(OptionFrame,text=gettextRcmdr("Name of the result object: "))
  resu.val<-tclVar("res")
  resu<-tkentry(OptionFrame,width=10,textvariable=resu.val)
  ncp.lab<-tklabel(OptionFrame,text=gettextRcmdr("Number of dimensions: "))
  ncp.val<-tclVar("5") 
  ncp<-tkentry(OptionFrame,width=5,textvariable=ncp.val)
  graph.lab<-tklabel(OptionFrame,text=gettextRcmdr("Plot the graphs: "))
  graph.check <- tkcheckbutton(OptionFrame)
  graphValue <- tclVar("1")
  tkconfigure(graph.check,variable=graphValue)
  Axe.label<-tklabel(OptionFrame,text=gettextRcmdr("Graphical output: select the dimensions:"))
  Axe1<-tclVar("1")
  Axe2<-tclVar("2")
  Axe1.entry <-tkentry(OptionFrame,width="5",textvariable=Axe1)
  Axe2.entry <-tkentry(OptionFrame,width="5",textvariable=Axe2)
  
    # mise en page de OptionFrame
  tkgrid(tklabel(OptionFrame,text=gettextRcmdr("Main options"), fg = "darkred"), columnspan=8, sticky="we") 
  tkgrid(tklabel(OptionFrame,text="")) # Ligne de blanc
  tkgrid(resu.lab, resu)
  tkgrid(ncp.lab, ncp)
  tkgrid(graph.lab,graph.check)
  tkgrid(Axe.label,Axe1.entry , Axe2.entry, sticky="w")
  tkgrid.configure(ncp.lab, graph.lab, resu.lab, Axe.label, column=1, columnspan=4, sticky="w")
  tkgrid.configure(ncp, graph.check, resu, column=6, columnspan=2, sticky="e")
  tkgrid.configure(Axe1.entry, column=6, columnspan=1, sticky="w")
  tkgrid.configure(Axe2.entry, column=7, columnspan=1, sticky="e")
  tkgrid.columnconfigure(OptionFrame,0, minsize=25)
  tkgrid.columnconfigure(OptionFrame,5, minsize=40)
  tkgrid.columnconfigure(OptionFrame,8, minsize=25)
################################################################

  #Frame pour HCPC
  HcpcFrame<-tkframe(top, borderwidth=2)
  Hcpc.funct(label=gettextRcmdr("Perform Clustering after HMFA"), firstLabel=gettextRcmdr("Perform Clustering after HMFA")) 
  tkgrid(Hcpc2Frame, columnspan=7)
  tkgrid.configure(Hcpc2Frame,column=4, columnspan=1)
  
#################################################################  

Analyse.funct<-function(){
    Htype<-as.vector(Htype)
    nom.res<-tclvalue(resu.val)
    if (length(ls(pat=nom.res))>0) justDoIt(paste('remove (',nom.res,')'))
    Sgraph<-FALSE
    if(tclvalue(graphValue)=="1") Sgraph<-TRUE
    Sncp<-as.numeric(tclvalue(ncp.val))
    Axe<-c(as.numeric(tclvalue(Axe1)), as.numeric(tclvalue(Axe2)))
    var.aux <- NULL
    Htype.aux <- NULL
    name.aux1 <- name.aux2 <- name.aux3 <- NULL
    L3 <- L2 <- L1 <- NULL
    if (HHH==1) tkmessageBox(message="You do not make a hierarchy on the variables. You must use a MFA",icon="warning",type="ok")
    if (HHH==3){
      name.group = list(name.group,c(paste("L2.G",1:length(B),sep="")),c(paste("L3.G",1:length(C),sep="")))
      name.aux3 <- c(paste("L3.G",1:length(C),sep=""))
      for (s in 1:length(C)){
        y = C[[s]]
        if (is.null(L3)) L3<-length(y) else L3<-c(L3, length(y))
        for (j in 1:length(y)){
          if (is.null(name.aux2)) name.aux2<-name.group[[2]][y[[j]]] else name.aux2<-c(name.aux2, name.group[[2]][[y[[j]]]])
          z = B[[y[[j]]]]
          if (is.null(L2)) L2<-length(z) else L2<-c(L2, length(z))
          for (k in 1:length(z)){
            if (is.null(L1)) L1<-length(A[[z[[k]]]]) else L1<-c(L1, length(A[[z[[k]]]]))
            if (is.null(name.aux1)) name.aux1<-name.group[[1]][[z[[k]]]] else name.aux1<-c(name.aux1, name.group[[1]][[z[[k]]]])
            var.aux = c(var.aux ,variables[A[[z[[k]]]]])
            Htype.aux = c(Htype.aux,Htype[z[[k]]])
          }
        }
      }
      variables = var.aux
      Htype = Htype.aux
    }
    if (HHH==2){
      name.group = list(name.group,c(paste("L2.G",1:length(B),sep="")))
      name.aux2 <- c(paste("L2.G",1:length(B),sep=""))
      for (j in 1:length(B)){
        z = B[[j]]
        if (is.null(L2)) L2<-length(z) else L2<-c(L2, length(z))
        for (k in 1:length(z)){
          if (is.null(L1)) L1<-length(A[[z[[k]]]]) else L1<-c(L1, length(A[[z[[k]]]]))
          if (is.null(name.aux1)) name.aux1<-name.group[[1]][[z[[k]]]] else name.aux1<-c(name.aux1, name.group[[1]][[z[[k]]]])
          var.aux = c(var.aux ,variables[A[[z[[k]]]]])
          Htype.aux = c(Htype.aux,Htype[z[[k]]])
        }
      }
      variables = var.aux
      Htype = Htype.aux
    }
    
    commande.data<-paste(activeDataSet(),'.HMFA <-', activeDataSet(), '[,c("', paste(variables, collapse='", "'), '")]', sep='')
    justDoIt(commande.data)
    logger(commande.data)
    commande.hmfa<-paste(nom.res, '<-HMFA(', activeDataSet(), '.HMFA, type =c("', paste(Htype, collapse='", "'), '"), H =list(c(',paste(L1, collapse=','),')',sep='')
    if (!is.null(L2)) commande.hmfa<-paste(commande.hmfa, ',c(', paste(L2, collapse=','),')',sep='')
    if (!is.null(L3)) commande.hmfa<-paste(commande.hmfa, ',c(', paste(L3, collapse=','),')',sep='')
    commande.hmfa<-paste(commande.hmfa, ') , name.group = list(c("', paste(name.aux1, collapse='", "'),'")',sep='')
    if (!is.null(name.aux2)) commande.hmfa<-paste(commande.hmfa, ',c("', paste(name.aux2, collapse='", "'),'")',sep='')
    if (!is.null(name.aux3)) commande.hmfa<-paste(commande.hmfa, ',c("', paste(name.aux3, collapse='", "'),'")',sep='')
    commande.hmfa<-paste(commande.hmfa, '), axes=c(', paste(Axe, collapse=", "), '), ncp =', Sncp,', graph =', Sgraph,')',sep='')
    logger(commande.hmfa)
    justDoIt(commande.hmfa)
    justDoIt(paste('remove(',activeDataSet(),'.HMFA)',sep=""))
    logger(paste('remove(',activeDataSet(),'.HMFA)',sep=""))


   #Commande de la fonction HCPC

    if(Rclassif==TRUE){
      commande.hcpc<-paste(nom.res,'.hcpc', '<-HCPC(', nom.res, ' ,nb.clust=', Rmeth, ',consol=', Rconsolid,',min=', Rminhcpc,',max=',Rmaxhcpc,',graph=', Rgraphhcpc, ')', sep="")
    justDoIt(commande.hcpc)
    logger(commande.hcpc)      
      if(Rreshcpc==TRUE){
        doItAndPrint(paste(nom.res,'.hcpc$data.clust[,ncol(res.hcpc$data.clust),drop=F]', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.var', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.axes', sep=""))
        doItAndPrint(paste(nom.res,'.hcpc$desc.ind', sep=""))
      }        
    }



    if (Sfichier==""){
      if(Seig) doItAndPrint(paste( nom.res, '$eig', sep=""))
      if(Sgroup) doItAndPrint(paste( nom.res, '$group', sep=""))
      if(Sind) doItAndPrint(paste( nom.res, '$ind', sep=""))
      if(Squanti.var) doItAndPrint(paste( nom.res, '$quanti.var', sep=""))
      if(Squali.var) doItAndPrint(paste( nom.res, '$quali.var', sep=""))
      if(Spartial) doItAndPrint(paste( nom.res, '$partial', sep=""))
      if(Sdescdim) doItAndPrint( paste('dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '))', sep=""))
    }
    else {
      Fich = Sfichier
      if (substr(Fich,1,1)!='"') Fich = paste('"',Fich,sep='')
      if (substr(Fich,nchar(Fich),nchar(Fich))!='"') Fich = paste(Fich,'"',sep='')
      append = FALSE
      if(Seig){
        doItAndPrint(paste('write.infile(', nom.res, '$eig, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Sgroup){
        doItAndPrint(paste('write.infile(', nom.res, '$group, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Sind){
        doItAndPrint(paste('write.infile(', nom.res, '$ind, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Squanti.var){
        doItAndPrint(paste('write.infile(', nom.res, '$quanti.var, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Squali.var){
        doItAndPrint(paste('write.infile(', nom.res, '$quali.var, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Spartial){
        doItAndPrint(paste('write.infile(', nom.res, '$partial, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Sdescdim) doItAndPrint(paste('write.infile(dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), ')), file =',Fich,',append=',append,')', sep=""))
    }

}

    #! fonction associée au bouton OK, execute et détruit l'interface graphique
  onOK<-function()
  {
    Analyse.funct()
    # destuction de la fenêtre Top
    closeDialog(top)
  }

################################################################
Reinitializ.funct<-function(){
  tkdestroy(top)
  FactoHMFA()
}
################################################################

### Création des frames ###
BasFrame<-tkframe(top,borderwidth=2, relief="groove")
BoutonFrame<-tkframe(BasFrame,borderwidth=2)
tkgrid(BoutonFrame, sticky="n")
ValidFrame<-tkframe(top,borderwidth=2)
NextFrame<-tkframe(top,borderwidth=2)
AnalyseFrame<-tkframe(top,borderwidth=2)

LabelFrame<-tkframe(top,borderwidth=2)
Groupe.Lab<-tklabel(LabelFrame, text = gettextRcmdr("Select one group for the 2nd level of the hierarchy"), fg = "blue")
Groupe2.Lab<-tklabel(LabelFrame, text = gettextRcmdr("Select one group for the 3rd level of the hierarchy"), fg = "blue")

########## Création des fonctions associées aux boutons ########
  #! fonction pour le choix des éléments de sortie
  Sortie.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.sortie<-0
    #déclaration des variables
    Seig <- FALSE
    Sgroup <- FALSE
    Sind <- FALSE
    Squanti.var <- FALSE
    Squali.var <- FALSE
    Spartial <- FALSE
    Sdescdim <- FALSE
    Sfichier <- ""

    .SortieLabel<-tclVar(paste(firstLabel, "", sep=" "))

    OnSortie<-function()
    {
      SortieWin<-tktoplevel()
      tkwm.title(SortieWin,"Outputs")

      #création de la fonction onOKsub
      onOK.sortie<-function()
      {
        assign("compteur.sortie", compteur.sortie+1, envir=env)
        if(compteur.sortie>0) tclvalue(.SortieLabel)<-paste(label, "", sep=" ")
        tkconfigure(Sortie.but, fg="blue")
        
        if(tclvalue(eigValue)=="1") assign("Seig", TRUE, envir=env)
        else assign("Seig", FALSE, envir=env)
        
        if(tclvalue(groupValue)=="1") assign("Sgroup", TRUE, envir=env)
        else assign("Sgroup", FALSE, envir=env)
        
        if(tclvalue(indValue)=="1") assign("Sind", TRUE, envir=env)
        else assign("Sind", FALSE, envir=env)
        
        if(tclvalue(partial.Value)=="1") assign("Spartial", TRUE, envir=env)
        else assign("Spartial", FALSE, envir=env)
        
        if(tclvalue(quanti.var.Value)=="1") assign("Squanti.var", TRUE, envir=env)
        else assign("Squanti.var", FALSE, envir=env)
        
        if(tclvalue(quali.var.Value)=="1") assign("Squali.var", TRUE, envir=env)
        else assign("Squali.var", FALSE, envir=env)
        
        if(tclvalue(descdim.Value)=="1") assign("Sdescdim", TRUE, envir=env)
        else assign("Sdescdim", FALSE, envir=env)
        
        if (tclvalue(Fichier)=="") assign("Sfichier", NULL, envir=env)
        assign("Sfichier", tclvalue(Fichier), envir=env)
        
        tkdestroy(SortieWin)      
      }
      
      eig.lab <-tklabel(SortieWin, text=gettextRcmdr("Eigenvalues"))
        eig.check <- tkcheckbutton(SortieWin)
        if(Seig) eigValue <- tclVar("1")
        else eigValue <- tclVar("0")
        tkconfigure(eig.check,variable=eigValue)

      group.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for the groups"))
        group.check <- tkcheckbutton(SortieWin)
        if(Sgroup) groupValue <- tclVar("1")
        else groupValue <- tclVar("0")
        tkconfigure(group.check,variable=groupValue)

      ind.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for individuals"))
        ind.check <- tkcheckbutton(SortieWin)
        if(Sind) indValue <- tclVar("1")
        else indValue <- tclVar("0")
        tkconfigure(ind.check,variable=indValue)

      partial.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for partial representation"))
        partial.check <- tkcheckbutton(SortieWin)
        if(Spartial) partial.Value <- tclVar("1")
        else partial.Value <- tclVar("0")
        tkconfigure(partial.check,variable=partial.Value)

      quanti.var.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for quantitative variables"))
        quanti.var.check <- tkcheckbutton(SortieWin)
        if(Squanti.var) quanti.var.Value <- tclVar("1")
        else quanti.var.Value <- tclVar("0")
        tkconfigure(quanti.var.check,variable=quanti.var.Value)

      quali.var.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for qualitative variables"))
        quali.var.check <- tkcheckbutton(SortieWin)
        if(Squali.var) quali.var.Value <- tclVar("1")
        else quali.var.Value <- tclVar("0")
        tkconfigure(quali.var.check,variable=quali.var.Value)
        
      descdim.lab<-tklabel(SortieWin,text=gettextRcmdr("Description of the dimensions"))
        descdim.check <- tkcheckbutton(SortieWin)
        if(Sdescdim) descdim.Value <- tclVar("1")
        else descdim.Value <- tclVar("0")
        tkconfigure(descdim.check,variable=descdim.Value)
        
      SfichierFrame<-tkframe(SortieWin,borderwidth=2)
      if (is.null(Sfichier)) Fichier <- tclVar("")
      else Fichier<-tclVar(Sfichier)
      Fichier.entry <-tkentry(SfichierFrame,width="40",textvariable=Fichier)
      tkgrid(tklabel(SfichierFrame,text=gettextRcmdr("Print results on a 'csv' file")),Fichier.entry)

      SortieOK.but<-tkbutton(SortieWin,text="OK",width=16,command=onOK.sortie)

        tkgrid(tklabel(SortieWin, text = gettextRcmdr("Select output options"), fg ="blue"),  columnspan = 2, sticky = "w")
        tkgrid(tklabel(SortieWin, text = " "))
        tkgrid(eig.lab,eig.check,sticky="w")
        tkgrid(group.lab,group.check,sticky="w")
        tkgrid(ind.lab,ind.check,sticky="w")
        tkgrid(partial.lab,partial.check,sticky="w")
        tkgrid(quanti.var.lab,quanti.var.check,sticky="w")
        tkgrid(quali.var.lab,quali.var.check,sticky="w")
        tkgrid(descdim.lab,descdim.check,sticky="w")
        tkgrid(tklabel(SortieWin, text = " "))
        tkgrid(SfichierFrame)
        tkgrid(tklabel(SortieWin, text = " "))
        tkgrid(SortieOK.but)
   }
    
    SortieFrame<-tkframe(BoutonFrame)
    Sortie.but<-tkbutton(SortieFrame, textvariable=.SortieLabel, command=OnSortie, borderwidth=3)
    tkgrid(Sortie.but, sticky="ew")
  })

### Création des buttons ###

#Option.but <- tkbutton(BoutonFrame,text=gettextRcmdr("Outputs"),command=Option.funct, borderwidth=3)
ValideGroupe1.but <- tkbutton(ValidFrame,text=gettextRcmdr("Valid the group"),command=ValideGroupe1.funct, borderwidth=3)
Next.but <- tkbutton(NextFrame,text=gettextRcmdr("Next level of the hierarchy"),command=Next.funct, borderwidth=3)
Analyse.but <- tkbutton(AnalyseFrame,text=gettextRcmdr("Run the HMFA"),command=Analyse.funct, borderwidth=3)
OKCancelHelp(helpSubject="HMFA")
Reinitializ.but <- tkbutton(BoutonFrame,text=gettextRcmdr("Restart"),command=Reinitializ.funct)
##tkgrid(ValideGroupe1.but)
tkgrid(Next.but)
tkgrid(tklabel(top, text="   "))
  Sortie.funct(label=gettextRcmdr("Outputs"), firstLabel=gettextRcmdr("Outputs"))
  tkgrid(SortieFrame, Reinitializ.but, columnspan=7)
#tkgrid(tklabel(BoutonFrame, text="   "),Option.but,tklabel(BoutonFrame, text="   "),Reinitializ.but , tklabel(BoutonFrame, text="   "))
##tkgrid(Analyse.but)

#################################################
  font2<-tkfont.create(family="times",size=12,weight="bold")
  fontheading<-tkfont.create(family="times",size=11,weight="bold")
  ListeQuantiFrame<- tkframe(top, borderwidth=2, relief="groove")
  label.quantiFrame.var<-tclVar(gettextRcmdr("Quantitative groups"))
  label.quantiFrame<-tklabel(ListeQuantiFrame, textvariable=label.quantiFrame.var,fg = "darkred", font=fontheading)
    # liste des groupes de variables quanti Actives
  listQuantiAct<-tklistbox(ListeQuantiFrame,selectmode="extended",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(scrQuantiAct,...))
  scrQuantiAct<-tkscrollbar(ListeQuantiFrame,repeatinterval=5,command=function(...)tkyview(listQuantiAct,...))
  listQuantiAct.nom<-NULL
  
    # liste des groupes de variables quanti Actives
  listQuantiIllu<-tklistbox(ListeQuantiFrame,selectmode="extended",exportselection="FALSE", height=4, yscrollcommand=function(...)tkset(scrQuantiIllu,...))
  scrQuantiIllu<-tkscrollbar(ListeQuantiFrame,repeatinterval=5,command=function(...)tkyview(listQuantiIllu,...))
  listQuantiIllu.nom<-NULL
  
    # boutons d'action groupes quantitative
  supprimeQuanti.funct(label="Delete")  
  ajoutQuanti.funct(label=gettextRcmdr("Add quanti. group"), firstLabel=gettextRcmdr("Add quanti group"))
  modifQuanti.funct(label=gettextRcmdr("Modify 1 group"), firstLabel=gettextRcmdr("Modify 1 group"))
    # mise en forme de ListeQuantiFrame
  #tkgrid(tklabel(ListeQuantiFrame, text = "Quantitative groups",fg = "darkred"), columnspan=11, sticky = "ew")
  tkgrid(label.quantiFrame, columnspan=11, sticky = "ew")
  tkgrid(listQuantiAct, scrQuantiAct, listQuantiIllu, scrQuantiIllu)
  tkgrid.configure(scrQuantiAct, column=3, sticky="wns")
  tkgrid.configure(scrQuantiIllu, column=9, sticky="wns")  
  tkgrid.configure(listQuantiAct, sticky = "ew", column=1, columnspan=2)
  tkgrid.configure(listQuantiIllu, sticky = "ew", column=7, columnspan=2)
  tkgrid.configure(tklabel(ListeQuantiFrame, text=" "))
  tkgrid.configure(GpeQuantiFrame,ModifGpeQuantiFrame, SupGpeQuantiFrame)
  tkgrid.configure(GpeQuantiFrame, sticky = "ew", column=1, columnspan=2)
  tkgrid.configure(ModifGpeQuantiFrame, sticky = "ew", column=4, columnspan=2)
  tkgrid.configure(SupGpeQuantiFrame, sticky = "ew", column=7, columnspan=2)
  tkgrid.columnconfigure(ListeQuantiFrame,0, minsize=25)
  tkgrid.columnconfigure(ListeQuantiFrame,10, minsize=25)
  tkgrid.columnconfigure(ListeQuantiFrame,3, minsize=25)
  tkgrid.columnconfigure(ListeQuantiFrame,9, minsize=25)
  tkgrid.columnconfigure(ListeQuantiFrame,4, minsize=35)
  tkgrid.columnconfigure(ListeQuantiFrame,5, minsize=35)
  
 # création du frame contenant les listes groupes quali
  ListeQualiFrame<- tkframe(top, borderwidth=2, relief="groove")
  label.qualiFrame.var<-tclVar(gettextRcmdr("Qualitative groups"))
  label.qualiFrame<-tklabel(ListeQualiFrame, textvariable=label.qualiFrame.var,fg = "darkred", font=fontheading)
    # liste des groupes de variables quali Actives
  listQualiAct<-tklistbox(ListeQualiFrame,selectmode="extended",exportselection="TRUE", height=4, yscrollcommand=function(...)tkset(scrQualiAct,...))
  scrQualiAct<-tkscrollbar(ListeQualiFrame,repeatinterval=5,command=function(...)tkyview(listQualiAct,...))
  listQualiAct.nom<-NULL
  
  # liste des groupes de variables quali Actives
  listQualiIllu<-tklistbox(ListeQualiFrame,selectmode="extended",exportselection="TRUE", height=4, yscrollcommand=function(...)tkset(scrQualiIllu,...))
  scrQualiIllu<-tkscrollbar(ListeQualiFrame,repeatinterval=5,command=function(...)tkyview(listQualiIllu,...))
  listQualiIllu.nom<-NULL

      # boutons d'action groupes qualitatif
  supprimeQuali.funct(label="Delete")  
  ajoutQuali.funct(label=gettextRcmdr("Add quali. group"), firstLabel=gettextRcmdr("Add quali group")) 
  modifQuali.funct(label=gettextRcmdr("Modify 1 group"), firstLabel=gettextRcmdr("Modify 1 group"))
  
    # mise en forme de ListeQualiFrame
  #tkgrid(tklabel(ListeQualiFrame, text = "Qualitative groups",fg = "darkred"), columnspan=11, sticky = "ew")
  tkgrid(label.qualiFrame, columnspan=11, sticky = "ew")
  tkgrid(listQualiAct, scrQualiAct, listQualiIllu, scrQualiIllu)
  tkgrid.configure(scrQualiAct, column=3, columnspan=1, sticky="wns")
  tkgrid.configure(scrQualiIllu, column=9, columnspan=1, sticky="wns")  
  tkgrid.configure(listQualiAct, sticky = "ew", column=1, columnspan=2)
  tkgrid.configure(listQualiIllu, sticky = "ew", column=7, columnspan=2)
  tkgrid.configure(tklabel(ListeQualiFrame, text=" "))
  tkgrid.configure(GpeQualiFrame, ModifGpeQualiFrame, SupGpeQualiFrame)
  tkgrid.configure(GpeQualiFrame, sticky = "ew", column=1, columnspan=2)
  tkgrid.configure(ModifGpeQualiFrame, sticky = "ew", column=4, columnspan=2)  
  tkgrid.configure(SupGpeQualiFrame, sticky = "ew", column=7, columnspan=2)
  tkgrid.columnconfigure(ListeQualiFrame,0, minsize=25)
  tkgrid.columnconfigure(ListeQualiFrame,10, minsize=25)
  tkgrid.columnconfigure(ListeQualiFrame,3, minsize=25)
  tkgrid.columnconfigure(ListeQualiFrame,9, minsize=25)
  tkgrid.columnconfigure(ListeQualiFrame,4, minsize=35)
  tkgrid.columnconfigure(ListeQualiFrame,5, minsize=35)

####################### Fin rajout ###################################

### Affichage ###
tkfocus(top)
ligneB<-tklabel(top, text = "")

################### Déb rajout###########
  if (length(listNumeric())>0){
    tkgrid(ListeQuantiFrame, column=1, columnspan=1, sticky="ew")
    tkgrid(ligneB)    
  }
  if (length(listFactors())>0) {
    tkgrid(ListeQualiFrame, column=1, columnspan=1, sticky="ew")
    tkgrid(ligneB)    
  }
#######

tkgrid(LabelFrame, sticky="w",column=1)

tkgrid(ListeFrame, sticky = "n",column=1)
tkgrid(ligneB)
tkgrid(ValidFrame, sticky = "n",column=1)
tkgrid(NextFrame, sticky = "n",column=1)
tkgrid(ligneB)
tkgrid(tklabel(top,text=""))
tkgrid(OptionFrame, sticky = "n",column=1)
tkgrid(tklabel(top,text=""))
tkgrid(HcpcFrame, column=1, columnspan=1)
tkgrid(tklabel(top,text=""))
tkgrid(BasFrame, sticky = "n",column=1)
tkgrid(AnalyseFrame, sticky = "n",column=1)
tkgrid(tklabel(top,text=""))
tkgrid(buttonsFrame, column=1, columnspan=1, sticky="ew" )
tkgrid(tklabel(top,text="")) # Ligne de blanc
}
################################### Fin fonction HMFA##################

#######################################################################
################        Fonction FactoHCPC             ################

FactoHCPC<-function(){
    require(tcltk)
    require(FactoMineR)
    top<-tktoplevel(borderwidth=5)
    tkwm.title(top,"HCPC")

# définition des polices
  font2<-tkfont.create(family="times",size=12,weight="bold")
  fontheading<-tkfont.create(family="times",size=13,weight="bold")

recupres<-function(envir = .GlobalEnv, ...){
    classres <- ls(envir = envir, all.names = TRUE)
    if (length(classres) == 0) 
        return(classres)
    names(which(sapply(classres, function(.x) inherits(get(.x, 
        envir = envir),c("PCA","CA","MCA","MFA","HMFA")))))
}
      
tousres<-c(listDataSets(),recupres())
  
if (length(which(sapply(ls(envir = .GlobalEnv, all.names = TRUE),function(.x) inherits(get(.x,envir= .GlobalEnv),"CA"))))>0)
  resca<-TRUE 
else resca<-FALSE


objintFrame <- tkframe(top)
    listobjint<-tklistbox(objintFrame,selectmode="single",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(objintFrame,repeatinterval=5,command=function(...)tkyview(listobjint,...))
    for (i in (1:length(tousres))){
        tkinsert(listobjint,"end",tousres[i])
    }
    tkselection.set(listobjint,0)
    tkgrid(tklabel(objintFrame, text = "Select an object", fg = "blue"), columnspan = 1, sticky = "w")    
    tkgrid(listobjint, scr,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listobjint,sticky = "ew")
    tkselection.set(listobjint,0)  


HcpcFrame<-tkframe(top,relief="ridge",borderwidth=2)   
 
      tkgrid(tklabel(HcpcFrame, text = gettextRcmdr("Main options"), fg = "blue"), column=1, sticky = "w")      

      if(resca==TRUE){
        lignes <- tkradiobutton (HcpcFrame)
        lignes.lab <- tklabel(HcpcFrame,text=gettextRcmdr("rows"))
        colonnes <- tkradiobutton (HcpcFrame)
        colonnes.lab <- tklabel(HcpcFrame,text=gettextRcmdr("columns"))
      }
      else{
        lignes <- tkradiobutton (HcpcFrame,state="disabled")
        lignes.lab <- tklabel(HcpcFrame,text=gettextRcmdr("rows"))
        colonnes <- tkradiobutton (HcpcFrame,state="disabled")
        colonnes.lab <- tklabel(HcpcFrame,text=gettextRcmdr("columns"))
      }      
      classifCAValue <- tclVar("rows")
      classifCA.lab <- tklabel(HcpcFrame,text=gettextRcmdr("For the result of a CA, perform clustering on: "))
      tkconfigure(lignes,variable=classifCAValue,value="rows")
      tkconfigure(colonnes,variable=classifCAValue,value="columns")      
      
      meth1 <- tkradiobutton (HcpcFrame)
      meth1.lab <- tklabel(HcpcFrame,text=gettextRcmdr("interactive"))
      meth2 <- tkradiobutton (HcpcFrame)
      meth2.lab <- tklabel(HcpcFrame,text=gettextRcmdr("automatic"))
      methValue <- tclVar("0")
      meth.lab <- tklabel(HcpcFrame,text=gettextRcmdr("Choice of the number of clusters: "))
      tkconfigure(meth1,variable=methValue,value="0")
      tkconfigure(meth2,variable=methValue,value="-1")
      
      minmaxhcpc.label<-tklabel(HcpcFrame,text=gettextRcmdr("The optimal number of clusters is chosen between:"))
      minhcpc<-tclVar("3")
      maxhcpc<-tclVar("10")
      minhcpc.entry <-tkentry(HcpcFrame,width="3",textvariable=minhcpc)
      maxhcpc.entry <-tkentry(HcpcFrame,width="3",textvariable=maxhcpc)

      consolid.lab <- tklabel(HcpcFrame,text=gettextRcmdr("Consolidate clusters "))
      consolid.check <- tkcheckbutton(HcpcFrame)
      consolidValue<-tclVar("0")    
      tkconfigure(consolid.check,variable=consolidValue)
      
      graphhcpc.lab <- tklabel(HcpcFrame,text=gettextRcmdr("Print graphs "))
      graphhcpc.check <- tkcheckbutton(HcpcFrame)
      graphhcpcValue <- tclVar("1")
      tkconfigure(graphhcpc.check,variable=graphhcpcValue) 
      
      resu.val<-tclVar("results")
      resu<-tkentry(HcpcFrame,width=6,textvariable=resu.val)
      resu.lab<-tklabel(HcpcFrame,text="Keep the results in: ")                 

      tkgrid(classifCA.lab,lignes.lab,lignes)
      tkgrid(colonnes.lab,colonnes) 
      tkgrid(tklabel(HcpcFrame,text=""))           
      tkgrid(meth.lab,meth1.lab,meth1)
      tkgrid(meth2.lab,meth2)
      tkgrid(tklabel(HcpcFrame,text=""))
      tkgrid(minmaxhcpc.label,minhcpc.entry , maxhcpc.entry)
      tkgrid(tklabel(HcpcFrame,text=""))      
      tkgrid(consolid.lab,consolid.check)
      tkgrid(graphhcpc.lab,graphhcpc.check) 
      tkgrid(resu.lab,resu)
      tkgrid(tklabel(HcpcFrame,text=""))     
      
      tkgrid.configure(minmaxhcpc.label,meth.lab,classifCA.lab,consolid.lab,graphhcpc.lab,resu.lab,column=1,columnspan=4,sticky="w")
      tkgrid.configure(meth1,meth2,lignes,colonnes,consolid.check,graphhcpc.check,column=8,sticky="w")
      tkgrid.configure(meth1.lab,lignes.lab,column=7,columnspan=1,sticky="w")
      tkgrid.configure(meth2.lab,colonnes.lab,column=7,columnspan=1,sticky="w") 
      tkgrid.configure(minhcpc.entry,column=7,columnspan=1,sticky="e")
      tkgrid.configure(maxhcpc.entry,column=8,columnspan=1,sticky="w")
      tkgrid.configure(resu,column=8,columnspan=1,sticky="w")
      
      tkgrid.columnconfigure(HcpcFrame,0, minsize=5)
      tkgrid.columnconfigure(HcpcFrame,5, minsize=5)
      tkgrid.columnconfigure(HcpcFrame,8, minsize=5) 

#Fonction outputs
    env<-environment()
    Gclust <- TRUE
    Gdescvar <- TRUE
    Gdescaxes <- TRUE
    Gdescind <- TRUE

    OnHCPCSorties<-function()
    {
      SortiesWin<- tktoplevel()
      tkwm.title(SortiesWin, gettextRcmdr("Outputs options"))

      onOKSorties<-function()
      {
        if(tclvalue(clust.bool)=="1") assign("Gclust", TRUE, envir=env)
        else assign("Gclust", FALSE, envir=env)
        
        if(tclvalue(descvar.bool)=="1") assign("Gdescvar", TRUE, envir=env)
        else assign("Gdescvar", FALSE, envir=env)
        
        if(tclvalue(descaxes.bool)=="1") assign("Gdescaxes", TRUE, envir=env)
        else assign("Gdescaxes", FALSE, envir=env)
        
        if(tclvalue(descind.bool)=="1") assign("Gdescind", TRUE, envir=env)
        else assign("Gdescind", FALSE, envir=env)        

        tkdestroy(SortiesWin)
      }
      OKSorties.but<-tkbutton(SortiesWin, text="OK", width=8,command=onOKSorties)

      tkgrid(tklabel(SortiesWin, text=""))
      tkgrid(tklabel(SortiesWin, text = gettextRcmdr("Select outputs options"), fg = "red",font=font2), column=1, columnspan = 8, sticky = "w")

      clust.lab <- tklabel(SortiesWin,text=gettextRcmdr("Print clusters individuals belong to "))
      clust.check <- tkcheckbutton(SortiesWin)
      if(Gclust) clust.bool<-tclVar("1")
      else clust.bool<-tclVar("0")      
      tkconfigure(clust.check,variable=clust.bool)
      
      descvar.lab <- tklabel(SortiesWin,text=gettextRcmdr("Print description of clusters by continuous variables "))
      descvar.check <- tkcheckbutton(SortiesWin)
      if(Gdescvar) descvar.bool<-tclVar("1")
      else descvar.bool<-tclVar("0")      
      tkconfigure(descvar.check,variable=descvar.bool)
      
      descaxes.lab <- tklabel(SortiesWin,text=gettextRcmdr("Print description of clusters by factors"))
      descaxes.check <- tkcheckbutton(SortiesWin)
      if(Gdescaxes) descaxes.bool<-tclVar("1")
      else descaxes.bool<-tclVar("0")      
      tkconfigure(descaxes.check,variable=descaxes.bool)
      
      descind.lab <- tklabel(SortiesWin,text=gettextRcmdr("Print parangons and most typical individuals for each cluster"))
      descind.check <- tkcheckbutton(SortiesWin)
      if(Gdescind) descind.bool<-tclVar("1")
      else descind.bool<-tclVar("0")      
      tkconfigure(descind.check,variable=descind.bool)                  

      tkgrid(tklabel(SortiesWin,text=""))
      tkgrid(clust.lab,clust.check)
      tkgrid(descvar.lab,descvar.check)
      tkgrid(descaxes.lab,descaxes.check)
      tkgrid(descind.lab,descind.check)
      tkgrid(tklabel(SortiesWin,text=""))     
      tkgrid(OKSorties.but)
      tkgrid(tklabel(SortiesWin,text=""))
      tkfocus(SortiesWin)
      
      tkgrid.configure(clust.lab,descvar.lab,descaxes.lab,descind.lab,column=1,sticky="w")
      tkgrid.configure(clust.check,descvar.check,descaxes.check,descind.check,column=8,sticky="w")
      tkgrid.configure(OKSorties.but,column=4,columnspan=2)
    }
    
    #SortiesFrame<-tkframe(HcpcFrame)
    HcpcSorties.but<-tkbutton(HcpcFrame, text="Outputs", command=OnHCPCSorties, width=13,fg="darkred")
    tkgrid(HcpcSorties.but)
    tkgrid.configure(HcpcSorties.but, column=8, columnspan=1,sticky="w")
    

#Fonction qui lance l'HCPC
 App<-function(){
    objint<-c(tclvalue(tkcurselection(listobjint)))
    obj<-tousres[as.numeric(tkcurselection(listobjint))+1]
    resultat<-tclvalue(resu.val)
    done = 1 
    
    commande.hcpc<-paste(resultat, '<-HCPC(', obj, ' ,nb.clust=', tclvalue(methValue), ',consol=', tclvalue(consolidValue),',min=', as.numeric(tclvalue(minhcpc)),',max=',as.numeric(tclvalue(maxhcpc)),',cluster.CA="',tclvalue(classifCAValue),'",graph=', tclvalue(graphhcpcValue), ')', sep="")
    justDoIt(commande.hcpc)
    logger(commande.hcpc) 
    
    if(Gclust) doItAndPrint(paste(resultat,'$data.clust[,ncol(',resultat,'$data.clust),drop=F]', sep=""))     
    if(Gdescvar) doItAndPrint(paste(resultat,'$desc.var', sep=""))
    if(Gdescaxes) doItAndPrint(paste(resultat,'$desc.axes', sep=""))
    if(Gdescind) doItAndPrint(paste(resultat,'$desc.ind', sep=""))

    return(done)
 }
 
# Fonction principale qui lance HCPC et ferme la fenêtre------------------------------------------------------------------------------------------------------------
onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}

 
#Mise en place des frames
App.but <- tkbutton(top,borderwidth=3,width=12,text="Submit",command=App,fg="blue")
OKCancelHelp(helpSubject="HCPC")
titre<-tklabel(top, text=gettextRcmdr("Hierarchical Clustering on Principal Components (HCPC)"),font=fontheading)
tkgrid.configure(titre,column=1,columnspan=3)
tkgrid(tklabel(top,text=""))
tkgrid(objintFrame,HcpcFrame,sticky="w")
tkgrid.configure(objintFrame,column=1,columnspan=1,sticky="w")
tkgrid.configure(HcpcFrame,column=2,columnspan=2,sticky="w")
tkgrid(tklabel(top,text="")) 
tkgrid(App.but)
tkgrid.configure(App.but,column=1,columnspan=1,sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(buttonsFrame)
tkgrid.configure(buttonsFrame, column=1,columnspan=2)
tkfocus(top)

tkgrid.columnconfigure(top,0, minsize=10)
tkgrid.columnconfigure(top,8, minsize=10) 
      
}      
######################### Fin fonction FactoHCPC #####################################
