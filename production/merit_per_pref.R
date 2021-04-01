# Meritocracy/non-meritocracy perceptions and preferences ISSP

# Juan Carlos Castillo, June 2017 - jc-castillo.com

setwd("/media/ntfs/Dropbox/proyectos y papers compartidos/Meritocracy and preferences/meritocracy_pref_international/loc_merit_meassure") # jc

# 0.DATA & LIBRARIES ---------------------- -

# 0.1 Libraries ------------------------- ####

  # Libraries
    pacman::p_load(sjPlot,
     sjmisc, # several functions,ej read_stata
     descr,
     memisc,  # codebook
     psych, # cor2latex
     corrplot, # graph correlations
     lavaan,
     stargazer,
     semTools,
     plyr, # count
     dplyr,
     car, # recode
     ggplot2,
		 Hmisc,
		 tidyverse,
		 data.table,
  	 stringr,
  	 countrycode,
  	 lme4,
     texreg,
     mice,
     ggrepel, # for scatter labels in sjp.scatter
     semPlot,
		 haven,
		 sjlabelled)

# 0.2 Original data preparation --------- ####

    #issp2009=read_stata("https://www.dropbox.com/s/gkq8lue7nnl9joy/ZA5400_v4-0-0.dta") # takes a while but it works
    issp2009=read_dta("../data/issp2009/ZA5400_v4-0-0.dta")
    names(issp2009) <- tolower(names(issp2009)) # change to small

  # Check data
    names(issp2009)

  # Check & generate IDs (v4: respondent, v5: country)
    tapply(issp2009$v4, issp2009$v5, mean, na.rm=TRUE)
    # Algunos paises (ej:826) tienen ids raros ...

  # Generar nueva id secuencial individuos por país para merges
    issp2009$id <- sequence(tabulate(issp2009$v5))
    tapply(issp2009$id, issp2009$v5, mean, na.rm=TRUE) # ok
    summary(issp2009$id)
    issp2009$idtem=issp2009$v5*10000
    issp2009$id=issp2009$idtem + issp2009$id

# End sec 0 -----------------------------

# 1. Measuring perceptions (getting-ahead) --

# 1.1 Data preparation, gen getahead.rda  #####

  names(issp2009)
  get_label(issp2009[8:18])

  #  v6: wealthy family (c 8)
  #  v7: educated parents (c 9)
  #  v9: ambition c11
  # v10: hard work c12
  # v11: right people
  # v12: political connections
  # v14: race c16
  # v16: gender c18
  # IDs: v5 (country) & id (individual id)

  # Recodes & relabels

    # Rename
    names(issp2009)
    getahead=issp2009[,c("v6","v7","v9", "v10","v11","v12",
             "v14","v16", # get ahead
    	  		 "v5","id")]

    getahead$wealthy=getahead$v6
    getahead$pareduc=getahead$v7
    getahead$ambition= getahead$v9
    getahead$hwork=getahead$v10
    getahead$people=getahead$v11
    getahead$polcone=getahead$v12
    getahead$race=getahead$v14
    getahead$gender=getahead$v16

    names(getahead)
    getahead=getahead[-c(1:8)]


  # recode values 8 9 to NA
    freq(getahead$wealthy)
    getahead[3:10][getahead[3:10] > 5] <- NA
    freq(getahead$gender) # check ok
    summary(getahead)

  # adjust labels
    get_labels(getahead$wealthy)
    set_na(getahead[ , 3:10]) <- c(8, 9)
    get_labels(getahead[3:10]) # check

  # Dataset with original code for sjp.likert graph
    getahead2=getahead

  # Reverse code for SEM models
    getahead[3:10]=6-getahead[3:10]

  # Generate variables with 3 values for getahead mess model categoric
    names(getahead)
    getahead_temp=subset(getahead[c(2,3:10)])
    names(getahead_temp)
    getahead_temp[2:9][getahead_temp[2:9] <  4] <- 3
    freq(getahead_temp$hwork)
    freq(getahead$hwork)
    colnames(getahead_temp)=paste("c3", colnames(getahead_temp),
    	sep="")
    names(getahead_temp)[names(getahead_temp)=="c3id"] <- "id"
    getahead_temp2=merge(getahead,getahead_temp, by=c("id"))
  	names(getahead_temp2) # ok
  	getahead=getahead_temp2

	# Save object
    save(getahead, file="./objects/getahead.rda")

# 1.2 Descriptives ---------------------- #####

    load("./objects/getahead.rda")

  # Summary table
    names(getahead)
    stargazer(getahead[,-c(1,2,11:18)], type = "text")

  # Summary table to latex
    sink("./results/desctab_ga.txt")
    stargazer(getahead[,-c(1,2,9:14)]);  sink()

  # Simple freq graph
    sjp.frq(getahead2$wealthy, show.values = TRUE,
    	coord.flip = TRUE)

  # Likert graph
    sjp.likert((getahead2[,-c(1,2)])) # ajustar

    # short var labels
      items <- c("Wealthy family", "Educated parents",
                 "Ambition", "Hard work", "Race",
                 "Gender","Know people", "Pol. connect." )

    # cambiar orden para la leyenda
      labels= c("Essential", "Very important",
        "Not very important",
        "Not important at all", "(neutral)")

    # ajustes globales sjplot

      sjp.setTheme(theme = "blank",legend.inside = TRUE,
        legend.pos = "bottom right", legend.size = 0.8,
        geom.outline.size=0, geom.label.size =3)

    # Gráfico final

      sjp.likert((getahead2[,-c(1,2)]),
        axis.labels   = items,
        legend.labels = labels,
        cat.neutral   = 3, # identifica a indiferentes
        geom.colors   = c("#9ecae1", "#6baed6","#4292c6",
                       "#2171b5"), # colorbrewer2.org para temas
        sort.frq      = "neg.asc", # sort descending)
        title         = "How important is for getting ahead ...",
        intercept.line.color = "white", # middle line white
        expand.grid   = FALSE, # no inner margins in plot
        show.n        = FALSE,    # hide N's in axis labels
        grid.range    = 1.3) # expand axis from 130% to 130%

      # Save
      dev.copy(png,"./results/likert_ga.png"); dev.off()

  # Correlations

    # Matrices & plot

      cormat_ga=cor(getahead[c(5,6,3,4,7:10)],
      	use="pairwise.complete.obs")
      corrplot.mixed(cormat_ga)
      dev.copy(png,"./results/corplot_ga.png", width=600, height=600); dev.off()

    # Correlation table a latex
         sink("./results/cortab_ga.txt")
      cor2latex(getahead[2:10],
            stars   = TRUE, adjust = "none",
            apa     = TRUE,heading = " ",
            caption ="Reasons to get ahead"); sink()

# 1.3 Measurement models--- ------------- #####

		load("../objects/getahead.rda")
    names(getahead)

	# Estimación del modelo de 1er order
	  cfa_1 <- '
	  # latent variables
	  merit =~ hwork + ambition
	  parent=~ wealthy + pareduc
	  backg=~ race + gender
		network=~ people + polcone
	  '
	  fit_1 <- cfa(cfa_1,data=getahead, missing = "ML")
	  summary(fit_1, fit.measures=TRUE,standardized=TRUE)

	  # Path model
	  semPaths(fit_1,layout = "tree2", rotation = 2,
	  	intercepts = FALSE, style = "lisrel", curvePivot = TRUE,
	  	cardinal = TRUE)
    dev.copy(png,"./results/path_cfa1.png",width=750,height=600)
    dev.off()

		# Estimación categorical 3 levels (5 levels convergence prob.)

	    cfa_1c <- '
		  # latent variables
		  merit=~ c3hwork + c3ambition
		  parent=~ c3wealthy + c3pareduc
		  backg=~ c3race + c3gender
			network=~ people + polcone
		  '
		  fit_1c <- cfa(cfa_1c,data=getahead,ordered = c("c3hwork",
		  	"c3ambition", "c3wealthy", "c3pareduc", "c3race",
		  	"c3gender",	"c3people", "c3polcone"))
		  summary(fit_1c, fit.measures=TRUE,standardized=TRUE) # works
		  mod=modificationindices(fit_1c)
		  subset(mod, mi>3.84, select=lhs:mi)

  # Estimación del modelo 2nd order

	  cfa_2 <- '
	  # latent variables
	  merit =~ hwork + ambition
	  parent=~ wealthy + pareduc
	  backg=~ race + gender
		network=~ people + polcone
		unmerit =~parent + backg + network
	  '
	  fit_2 <- cfa(cfa_2,data=getahead, missing = "ML")
	  summary(fit_2, fit.measures=TRUE,standardized=TRUE)

	  # Path
		  semPaths(fit_2,layout = "tree", rotation = 2,
		  	intercepts = FALSE,	style = "lisrel", curvePivot = TRUE,
		  	cardinal = FALSE)
		  dev.copy(png,"./results/path_cfa2.png",width=750,height=600)
	    dev.off()

# End sec 1 -----------------------------

# 2. MEASURING BELIEFS (detearn) ---------- -

# 2.1 Data preparation, detearn.rda ----- #####

		names(issp2009)
	  detearn=issp2009[,c("v47", "v48", "v49", "v50", "v51",
	   	"v52", "v5","id")]

    detearn$respons 	= detearn$v47
    detearn$yeduc  	  = detearn$v48
    detearn$family    = detearn$v49
    detearn$child     = detearn$v50
    detearn$welljob   = detearn$v51
    detearn$hardjob   = detearn$v52

	  names(detearn)
    detearn=detearn[-c(1:6)]

  # recode values 8 9 to NA
    summary(detearn)
    freq(detearn$respons)
    detearn[3:8][detearn[3:8] > 5] <- NA
    freq(detearn$respons) # check ok

  # Out Portugal (no answer in well job item)
    detearn[which(detearn$v5==620),]=NA

  # adjust labels ###PROB PORTUCAL!!!
    get_labels(detearn)
    set_na(detearn[ , 3:8]) <- c(8, 9)
    set_na(detearn$welljob)=c(1)
    summary(detearn)
    freq(detearn$welljob) ## prob portugal
    detearn$welljob[detearn$welljob ==0] = NA
    get_labels(detearn$welljob)
    labels(detearn$welljob)=labels(detearn$respons)### PROB NOT SOLVED

  # Dataset with original code for sjp.likert graph
    detearn2=detearn

  # Reverse code for SEM models
    detearn[3:8]=6-detearn[3:8]
    freq(detearn$respons)

	# Save object
    save(detearn, file="detearn.rda")

# 2.2 Descriptives ---------------------- ######

	load("objects/detearn.rda")

  # Summary table
    names(detearn)
    stargazer(detearn[,-c(1,2)], type = "text")

  # Summary table to latex
    stargazer(detearn[,-c(1,2)])

  # Simple freq graph
    sjp.frq(detearn2$respons, show.values = TRUE, coord.flip = TRUE)

  # Likert graph (uses detearn2 object, with original coding)

    sjp.likert((detearn2[,-c(1,2)])) # ajustar

    # short var labels
      items <- c("Responsibility", "Years education",
                 "Family", "Children", "Well job",
                 "Hard job")

    # cambiar orden para la leyenda
      labels= c("Essential", "Very important","(neutral)"
                ,"Not very important","Not important at all")

    # ajustes globales sjplot
      sjp.setTheme(theme = "blank",legend.inside = TRUE,
        legend.pos = "bottom right", legend.size = 0.8,
        geom.outline.size=0, geom.label.size =3)

      sjp.likert((detearn2[,-c(1,2)]),
        axis.labels   = items,
        legend.labels = labels,
        cat.neutral   = 3, # identifica a indiferentes
        geom.colors   = c("#9ecae1", "#6baed6","#4292c6",
                       "#2171b5"), # colorbrewer2.org para temas
        sort.frq      = "neg.asc", # sort descending)
        title         = "In deciding how much people ought to earn, how important should each of these things be...",
        intercept.line.color = "white", # middle line white
        expand.grid   = FALSE, # no inner margins in plot
        show.n        = FALSE,    # hide N's in axis labels
        grid.range    = 1.3) # expand axis from 130% to 130%

      # Save
      	dev.copy(png,"./results/likert_detearn.png"); dev.off()


      # freq(detearn2$pareduc)   # check educated parents, ok

  # Correlations
    cormat=cor(detearn[5:8], use="pairwise.complete.obs")
    corrplot.mixed(cormat)

    #save
     dev.copy(png,"./results/corplot_detearn.png",width=600,
      	height=600); dev.off()

# 2.3 Measurement models ---------------- ######

  # Continuous model

	  cfa_1 <- '
	  # latent variables
	  bmerit =~ welljob + hardjob
	  need=~ family + child
	  '
	  fit_1 <- cfa(cfa_1,data=detearn, missing = "ML")
	  summary(fit_1, fit.measures=TRUE,standardized=TRUE)

	   # Path model
		  semPaths(fit_1,"std",layout = "tree2", rotation = 2,
	  	intercepts = FALSE,	style = "lisrel", curvePivot = TRUE,
	  	cardinal = TRUE, edge.color = "black", residScale = 10)
    dev.copy(png,"./results/path_detearn1.png",width=750,height=600)
    dev.off()



	# Categorical model

    fit_1c <- cfa(cfa_1,data=detearn, ordered = c("welljob",
    	"hardjob","family","child"))
    summary(fit_1c, fit.measures=TRUE,standardized=TRUE)  #probs

# End sec 2 -----------------------------

# 3. MEASURING BELIEFS & PERCEPTIONS ------ -

# 3.1 Define object --------------------- -------------------------------

			load("./objects/getahead.rda")
			load("./objects/detearn.rda")
			names(getahead)
			names(detearn)

			merit_per_pref=merge(getahead,detearn, by=c("id","v5"))
			names(merit_per_pref)
# 3.2 Measurement continuous ------------ ------------------

	# Estimación del modelo simple
	  cfa_perpref1 <- '
	  # latent variables
	  merit =~ hwork + ambition
	  parent=~ wealthy + pareduc
	  backg=~ race + gender
		network=~ people + polcone
	  bmerit =~ welljob + hardjob
	  need=~ family + child
	  '
	  fit_perpref1 <- cfa(cfa_perpref1,data=merit_per_pref,
	  	missing = "ML")
	  summary(fit_perpref1, fit.measures=TRUE,standardized=TRUE)

 	  # Path model
		  semPaths(fit_perpref1,layout = "tree2", rotation = 2,
		  	intercepts = FALSE, style = "lisrel", curvePivot = TRUE,
		  	sizeMan = 3.5, sizeLat = 6)

	    dev.copy(png,"./results/path_perpref1.png",width=800,
	    	height=650);  dev.off()


  # Estimación del modelo c/ unmerit 2do orden
	  cfa_perpref2 <- '
	  # latent variables
	  merit =~ hwork + ambition
	  parent=~ wealthy + pareduc
	  backg=~ race + gender
		network=~ people + polcone
		unmerit=~ parent + backg + network # 2nd order
	  bmerit =~ welljob + hardjob
	  need=~ family + child
	  '
	  fit_perpref2 <- cfa(cfa_perpref2,data=merit_per_pref,
	  	missing = "ML")
	  summary(fit_perpref2, fit.measures=TRUE,standardized=TRUE)

 	  # Path model
		  semPaths(fit_perpref2,layout = "tree", rotation = 2,
		  	intercepts = FALSE, style = "lisrel", curvePivot = TRUE,
		  	sizeMan = 3.5, sizeLat = 6)

	    dev.copy(png,"./results/path_perpref2.png",width=800,
	    	height=650);  dev.off()

# 3.3 Measurement categorical -pendiente- #####

	# Estimación del modelo simple categórico
	  fit_1c <- cfa(cfa_1,data=merit_per_pref, ordered=c("wealthy",
	  	"ambition","hwork","people","polcone","race","gender",
	  	"pareduc","welljob","hardjob","family","child"))
	  summary(fit_1c, fit.measures=TRUE,standardized=TRUE)

	# Estimación del modelo 2do orden categórico
	  fit_2c <- cfa(cfa_2,data=merit_per_pref, ordered=c("wealthy",
	  	"ambition","hwork","people","polcone","race","gender",
	  	"pareduc","welljob","hardjob","family","child"))
	  summary(fit_2c, fit.measures=TRUE,standardized=TRUE)
# 3.4. Save factor scores --------------- #####

	# Factor scores fit_perpref2

		# Multiple imputation (para recuperar missings en scores) - REVIEW THIS, command lavPredict from 2017 should predict with missing values therefore saving from multiple imputation

		  # generate 5 multiple complete datasets
			out <- mice(merit_per_pref[,c(3:10,19:24)], m=5)
			D1 <- complete(out, 1)
			D2 <- complete(out, 2)
			D3 <- complete(out, 3)
			D4 <- complete(out, 4)
			D5 <- complete(out, 5)

			# fit model for each complete dataset
			fit1 <- cfa(cfa_perpref2, data=D1)
			fit2 <- cfa(cfa_perpref2, data=D2)
			fit3 <- cfa(cfa_perpref2, data=D3)
			fit4 <- cfa(cfa_perpref2, data=D4)
			fit5 <- cfa(cfa_perpref2, data=D5)

		# predict scores for all models
			p1 <- predict(fit1)
			p2 <- predict(fit2)
			p3 <- predict(fit3)
			p4 <- predict(fit4)
			p5 <- predict(fit5)

		# compute 'average' across 5 sets of scores:
			scores <- (p1 + p2 + p3 + p4 + p5)/5

	  # Generate factor scores
	    scores_dat=as.data.frame(scores)
	    dim(scores_dat)
	    str(scores_dat)
	    summary(scores)

		# Generate object with factor scores
	    dim(merit_per_pref)
	    dim(scores_dat)
		  merit_per_pref_sco=cbind(merit_per_pref,scores)

	# Save object with factor scores
		names(merit_per_pref_sco)
		merit_per_pref_sco=merit_per_pref_sco[,c(1:2,25:31)]
	  save(merit_per_pref_sco,
	  	file="./objects/merit_per_pref_sco.rda")

 	# Descriptives factor scores
	  load("./objects/merit_per_pref_sco.rda")
	   # Correlations
	  names(merit_per_pref_sco)
    cormat=cor(merit_per_pref_sco[c(3,7:9)],
    	use="pairwise.complete.obs")
    corrplot.mixed(cormat)
      #save
     dev.copy(png,"./results/corplot_scores.png",width=600,
      	height=600); dev.off()

  # Descriptive scatter countries
     load("./objects/merit_per_pref_sco.rda")
     agg_merit=merit_per_pref_sco %>% group_by(v5) %>% summarise_all(funs(mean))
     
     agg_merit$v5c=countrycode(agg_merit$v5,
       'iso3n','iso3c')

     set_label(agg_merit$merit)="Meritocratic perception"
     set_label(agg_merit$bmerit)="Meritocratic preferences"
     
     sjp.scatter(agg_merit$merit,agg_merit$bmerit, 
                 dot.labels = agg_merit$v5c,
                 fit.line = TRUE,
                 show.ci = TRUE)
     
     dev.copy(png,"./results/mperc_mprefs.png",width=600,
              height=600); dev.off()
     
     sjp.scatter(agg_merit$merit,agg_merit$unmerit, 
                 dot.labels = agg_merit$v5c,
                 fit.line = TRUE,
                 show.ci = TRUE)
     
     dev.copy(png,"./results/mperc_munperc.png",width=600,
              height=600); dev.off()
     
     sjp.scatter(agg_merit$bmerit,agg_merit$need, 
                 dot.labels = agg_merit$v5c,
                 fit.line = TRUE,
                 show.ci = TRUE)
     
     dev.copy(png,"./results/mperc_munperc.png",width=600,
              height=600); dev.off()
     
# 3.5 Invariance full model ---pendiente- --------

	fit.indices=c("chisq", "df", "cfi", "tli", "rmsea")

	# Continuous first order
	  fit.conf1=cfa(cfa_1, data=merit_per_pref, group="v5",std.lv = TRUE)
		fitMeasures(fit.conf1,fit.indices)
		summary(fit.conf1)
	  inspect(fit.conf1,"theta")
	  inspect(fit.conf1,"cov.lv")
	  partialInvariance(fit.conf1, type = "strict", p.adjust = "holm")

# End section 3 -------------------------
