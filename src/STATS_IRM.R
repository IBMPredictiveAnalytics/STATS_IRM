#/***********************************************************************
# * Licensed Materials - Property of IBM 
# *
# * IBM SPSS Products: Statistics Common
# *
# * (C) Copyright IBM Corp. 1989, 2014
# *
# * US Government Users Restricted Rights - Use, duplication or disclosure
# * restricted by GSA ADP Schedule Contract with IBM Corp. 
# ************************************************************************/

# Author: Alex Reutter, IBM SPSS
# Version 1.1.2
# History
# 21-Dec-2011 fix failure to create dataset on repeated use
#
helptext="The STATS IRM command requires the R Integration Plug-in
and the R ltm package.

STATS IRM 
  /VARIABLES ITEMS=variable list
  [/OPTIONS [MISSING={LISTWISE**  }]  [EXECUTE={TRUE**}] ]
                     {ALLAVAILABLE}            {FALSE }
  [/PRINT [SUMMARY] [ITEMFIT]]
  [/PLOT [DESCRIPTIVES] [FACTORSCORES] [ICC] [IIC]]
  [/SAVE [PROGRAMFILE=filespec]
 		 [PERSONFITDATASET=datasetname]]

Split files and weight are not honored by this command.

STATS IRM /HELP prints this information and does nothing else.

Example:
STATS IRM 
	/VARIABLES ITEMS=item1 item2 item3 item4 item5.

Executes the tpm function from the R ltm package.
The variable list specifies the items.  It is assumed that
the values of these variables is 0,1.

/OPTIONS MISSING=LISTWISE causes listwise deletion of missing values.  ALLAVAILABLE
causes all available information to be used; i.e., cases with missing values
are still used.

EXECUTE=FALSE runs the command syntax without calling the R function. 
This is mainly useful in combination with SAVE PROGRAMFILE.

/PRINT SUMMARY displays the Akaike Information Criterion (AIC) and Bayesian Information
Criterion (BIC) for the model.

ITEMFIT displays the item fit statistics and p-values for each item.

/PLOT DESCRIPTIVES displays a plot of the proportion of correct responses for each 
item versus the total correct.

FACTORSCORES displays a plot of kernel density estimates for the factor scores 
(people parameters).

ICC displays a plot of the item characteristic curves for the model.

IIC displays a plot of the iterm information curves for the model.

/SAVE PROGRAMFILE causes the R code that implements the procedure to be 
written to the specified file. Since the R function has features not 
exposed in this extension command, the generated program can be a useful 
starting point for additional specifications.

PERSONFITDATASET creates a new dataset that contains a case for each observed 
response pattern, variables that describe the response patterns (one for each
item), and variables containing the Levine and Rubin person-fit statistic, 
Drasgow person-fit statistic, P-value for Drasgow person-fit statistic,
Observed frequency, Expected frequency, Factor scores, Factor scores standard errors,
and Residuals.
"

run_irm<-function(items, missing="listwise", print_summary=FALSE, print_itemfit=FALSE, plot_descriptives=FALSE,
		   plot_factorscores=FALSE, plot_icc=FALSE, plot_iic=FALSE, personfitdataset=NULL)
{
    domain<-"STATS_IRM"
	setuplocalization(domain)
    tryCatch(library(ltm), error=function(e){
        stop(gettextf("The R %s package is required but could not be loaded.","ltm",domain=domain),call.=FALSE)
        }
    )

    if (identical(missing,"listwise")) {missing<-na.exclude} else {missing<-NULL}
    dta<-spssdata.GetDataFromSPSS(items,missingValueToNA=TRUE)

    res <- tryCatch(
            tpm(data=dta,na.action=missing),
            error=function(e) {return(c(gettext("ERROR:",domain=domain),e))}
           )
      
    if (!is.null(res$message)) {print(res$message)} else {
        miss<-ifelse(identical(missing,na.exclude),"na.exclude","NULL")
		
		# Start sending output to Viewer
        spsspkg.StartProcedure(gettext("Three Parameter Item Respose Model",domain=domain))

		# Summary table
		if ( print_summary ) {
			information_criteria <- matrix( c(summary(res)$AIC,summary(res)$BIC), ncol=1 )
			dimnames(information_criteria)[[1]] <- c("Akaike Information Criterion (AIC)","Bayesian Information Criterion (BIC)")
			dimnames(information_criteria)[[2]] <- c("Value")
			spsspivottable.Display(information_criteria, 
				title=gettext("Summary",domain=domain),
				templateName="STATSIRMSummary",
				isSplit=FALSE)
		}
		
		# Coefficients table
        coeff <- summary(res)$coefficients
		coeff_table <- spss.BasePivotTable("Coefficients", "coefficients")
		rowdim1=BasePivotTable.Append(coeff_table,Dimension.Place.row,"Parameter")
		rowdim2=BasePivotTable.Append(coeff_table,Dimension.Place.row,"Item")
		coldim=BasePivotTable.Append(coeff_table,Dimension.Place.column,"Statistic")
		statistic_cat <- list(spss.CellText.String("Value"),spss.CellText.String("Standard Error"),spss.CellText.String("Z"))
		param_cat <- list(spss.CellText.String("Guessing"),spss.CellText.String("Difficulty"),spss.CellText.String("Discrimination"))
		BasePivotTable.SetCategories(coeff_table,coldim,statistic_cat)
		BasePivotTable.SetCategories(coeff_table,rowdim1,param_cat)
		rowdim2_cat <- NULL
		for ( i in 1:length(coefficients(res)[,1]) ) {
			rowdim2_cat <- c(rowdim2_cat, spss.CellText.String(dimnames(coefficients(res))[[1]][i]))
		}
		item_cat <- as.list(rowdim2_cat)
		BasePivotTable.SetCategories(coeff_table,rowdim2,item_cat)
		for ( i in 1:length(param_cat) ) {
			for ( j in 1:length(item_cat) ) {
				for ( k in 1:length(statistic_cat) ) {
					if ( !is.nan(coeff[((i-1)*length(item_cat)+j),k]) ) {
						BasePivotTable.SetCellValue(coeff_table,list(param_cat[[i]],item_cat[[j]],statistic_cat[[k]]),spss.CellText.Number(coeff[((i-1)*length(item_cat)+j),k]))
					}
				}
			}
		}

			
		# Item fit
		if ( print_itemfit ) {
			item_fit <- matrix( c(item.fit(res)$Tobs,item.fit(res)$p.value), ncol=2, byrow=F )
			dimnames(item_fit)[[1]] <- c(names(item.fit(res)$Tobs))
			dimnames(item_fit)[[2]] <- c("Chi-square","Sig.")
			spsspivottable.Display(item_fit, 
				title=gettext("Item Fit Statistics",domain=domain),
				templateName="STATSIRMItemFit",
				isSplit=FALSE)
		}
		
		# Descriptives plot
		if ( plot_descriptives ) {
			plot(descript(dta))
		}

		# Factor scores plot
		if ( plot_factorscores ) {
			plot(factor.scores(res))
		}

		# Item characteristic curves plot
		if ( plot_icc ) {
			plot(res, type="ICC")
		}

		# Descriptives plot
		if ( plot_iic ) {
			plot(res, type="IIC")
		}

		spsspkg.EndProcedure()

		# Save person fit statistics to new dataset
        if (!is.null(personfitdataset)){
            personfitdict = spssdictionary.CreateSPSSDictionary(c("pattern", gettext("Response pattern",domain=domain), 0, "F4.0", "ordinal"))
            for (i in 1:length(item_cat)){
                personfitdict<-spssdictionary.CreateSPSSDictionary(personfitdict, c(paste("Item_",i,sep=""), gettextf("Item %s",i,domain=domain), 0, "F4.0", "scale"))
            }
			personfitdict<-spssdictionary.CreateSPSSDictionary(personfitdict, 
															   c("L0", gettextf("Levine and Rubin person-fit statistic",domain=domain), 0, "F8.2", "scale"),
															   c("Lz", gettextf("Drasgow person-fit statistic",domain=domain), 0, "F8.2", "scale"),
															   c("p_Lz", gettextf("P-value for Drasgow person-fit statistic",domain=domain), 0, "F8.2", "scale"),
															   c("obs", gettextf("Observed frequency",domain=domain), 0, "F4.0", "scale"),
															   c("exp", gettextf("Expected frequency",domain=domain), 0, "F8.2", "scale"),
															   c("fscores", gettextf("Factor scores",domain=domain), 0, "F8.2", "scale"),
															   c("fscores_se", gettextf("Factor scores standard errors",domain=domain), 0, "F8.2", "scale"),
															   c("residuals", gettextf("Residuals",domain=domain), 0, "F8.2", "scale")
															   )
            tryCatch({
                spssdictionary.SetDictionaryToSPSS(personfitdataset, personfitdict)
                personfitdata <- list()
				personfitdata[[1]] <- seq(1,length(person.fit(res)$resp.patterns[,1]))
                for (i in 2:(length(person.fit(res)$resp.patterns[1,])+1)) {personfitdata[[i]] <- person.fit(res)$resp.patterns[,(i-1)]}
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+2]] <- person.fit(res)$Tobs[,1]
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+3]] <- person.fit(res)$Tobs[,2]
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+4]] <- person.fit(res)$p.values[,1]
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+5]] <- factor.scores(res)$score.dat["Obs"][,1]
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+6]] <- factor.scores(res)$score.dat["Exp"][,1]
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+7]] <- factor.scores(res)$score.dat["z1"][,1]
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+8]] <- factor.scores(res)$score.dat["se.z1"][,1]
				resid <- residuals(res)
				for (i in 1:length(person.fit(res)$resp.patterns[1,])) { resid <- resid[order(resid[,(length(person.fit(res)$resp.patterns[1,])-i+1)]),] }
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+9]] <- resid[,(length(person.fit(res)$resp.patterns[1,])+3)]			
				
                personfitdata = data.frame(personfitdata)
                spssdata.SetDataToSPSS(personfitdataset, personfitdata)
				spssdictionary.EndDataStep()   # 12/21/11
                }, 
                error=function(e) {print(e)
                cat(gettext("Failed to create person fit statistics dataset. Dataset name must not already exist: ",domain=domain),personfitdataset)
                }
            )
        }

	}

    res <- tryCatch(rm(list=ls()),warning=function(e){return(NULL)})
    
}

caller<-function(items, missing="listwise", programfile=NULL, execute="true", print_summary=FALSE, print_itemfit=FALSE, plot_descriptives=FALSE,
		   plot_factorscores=FALSE, plot_icc=FALSE, plot_iic=FALSE, personfitdataset=NULL){
    
    if(!is.null(programfile)){
        title<-"# STATS IRM\n"

		if ( !is.null(personfitdataset) ) {
            pfit <- paste("personfitdataset<-",dQuote(personfitdataset),sep="")
		} else {
            pfit <- "personfitdataset<-NULL"			
		}
        lines<-c(title,
            "run_irm<-",
            attr(run_irm,"source"),
            paste("items<-",deparse(items),sep=""),
            paste("missing<-",dQuote(missing),sep=""),
            paste("print_summary<-",print_summary,sep=""),
            paste("print_itemfit<-",print_itemfit,sep=""),
            paste("plot_descriptives<-",plot_descriptives,sep=""),
            paste("plot_factorscores<-",plot_factorscores,sep=""),
            paste("plot_icc<-",plot_icc,sep=""),
            paste("plot_iic<-",plot_iic,sep=""),
			pfit,
			"run_irm(items,missing,print_summary,print_itemfit,plot_descriptives,plot_factorscores,plot_icc,plot_iic,personfitdataset)")
        f<-file(description=programfile,open="wb",encoding="UTF-8")
        writeLines(lines,con=f)
        close(f)
    }
    
    if (execute=="true") run_irm(items,missing,print_summary,print_itemfit,plot_descriptives,plot_factorscores,plot_icc,plot_iic,personfitdataset)
    
}
setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 

Run<-function(args){
    cmdname = args[[1]]
    args <- args[[2]]
    oobj<-spsspkg.Syntax(templ=list(
                spsspkg.Template("ITEMS", subc="VARIABLES",  ktype="existingvarlist", var="items", islist=TRUE),
                spsspkg.Template("MISSING", subc="OPTIONS",ktype="str", var="missing"),
                spsspkg.Template("EXECUTE", subc="OPTIONS", ktype="str", var="execute"),
                spsspkg.Template("SUMMARY", subc="PRINT", ktype="bool", var="print_summary"),
                spsspkg.Template("ITEMFIT", subc="PRINT", ktype="bool", var="print_itemfit"),
                spsspkg.Template("DESCRIPTIVES", subc="PLOT", ktype="bool", var="plot_descriptives"),
                spsspkg.Template("FACTORSCORES", subc="PLOT", ktype="bool", var="plot_factorscores"),
                spsspkg.Template("ICC", subc="PLOT", ktype="bool", var="plot_icc"),
                spsspkg.Template("IIC", subc="PLOT", ktype="bool", var="plot_iic"),
                spsspkg.Template("PROGRAMFILE", subc="SAVE", ktype="literal", var="programfile"),
                spsspkg.Template("PERSONFITDATASET", subc="SAVE", ktype="literal", var="personfitdataset")
                ))

    if ("HELP" %in% attr(args,"names"))
        #writeLines(helptext)
        helper(cmdname)
    else
        res <- spsspkg.processcmd(oobj,args,"caller")
        
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}