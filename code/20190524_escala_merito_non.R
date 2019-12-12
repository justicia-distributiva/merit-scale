# Merit scale -------------------------------------------------------------

# install.packages("haven","dplyr","sjPlot","psych","lavaan","here")

library(haven)
library(dplyr)
library(sjPlot)
library(psych)
library(lavaan)
library(here)
rm(list=ls())
setwd(here())

source("code/recode_tratamientos.R") # recode original dataset 
load(file = "data/pob_04.RData")

#Nota: Quedarme solamente con los datos completos para los ítems de la Escala

# -99 y -999 == NA --------------------------------------------------------


for (i in 1:ncol(data)) {
data[,i][data[,i] == c(-99)]  <- NA #Missing 
data[,i][data[,i] == c(-999)] <- NA #Missing  
}

summary(data)
names(data)

sjPlot::view_df(data)

# Nombres sustantivos variables -------------------------------------------
data <- rename(data,
               per_esfuerzo =merit_perc_pref_1,
               per_talento  =merit_perc_pref_2,
               per_famrica  =merit_perc_pref_3,
               per_contact  =merit_perc_pref_4,
               pref_esfuerzo=merit_perc_pref_5,
               pref_talento =merit_perc_pref_13,
               pref_famrica =merit_perc_pref_14,
               pref_contact =merit_perc_pref_15,
               reduc=pref_redis_taxB_1,
               rsalu=pref_redis_taxB_2,
               rpens=pref_redis_taxB_3,
               rdesi=pref_redis_taxB_4)

data$per_esfuerzo <- as.numeric(data$per_esfuerzo )
data$per_talento  <- as.numeric(data$per_talento  )
data$per_famrica  <- as.numeric(data$per_famrica  )
data$per_contact  <- as.numeric(data$per_contact  )
data$pref_esfuerzo<- as.numeric(data$pref_esfuerzo)
data$pref_talento <- as.numeric(data$pref_talento )
data$pref_famrica <- as.numeric(data$pref_famrica )
data$pref_contact <- as.numeric(data$pref_contact )

# Brechas -----------------------------------------------------------------
data$gap_per <- as.numeric(data$salary_perc_1/data$salary_perc_2)
data$gap_jus <- as.numeric(data$salary_just_1/data$salary_just_2)
data$lngap_per <- as.numeric(log(data$salary_perc_1/data$salary_perc_2))
data$lngap_jus <- as.numeric(log(data$salary_just_1/data$salary_just_2))

# Redistribución  ---------------------------------------------------------

data$reduc <- as.numeric(data$reduc)
data$rsalu <- as.numeric(data$rsalu)
data$rpens <- as.numeric(data$rpens)
data$rdesi <- as.numeric(data$rdesi)

# Correlacion items escala meritocracia -----------------------------------

cor(x = select(data, c(per_esfuerzo,per_talento,per_famrica,per_contact,pref_esfuerzo,pref_talento,pref_famrica,pref_contact)),use = "complete.obs")
poly1 <- psych::polychoric(select(.data = data, c(per_esfuerzo,per_talento,per_famrica,per_contact,pref_esfuerzo,pref_talento,pref_famrica,pref_contact)),na.rm = TRUE)

corrplot::corrplot(poly1$rho,order = "AOE", type = "lower")

# EFA ---------------------------------------------------------------------

fa1 <- fa(r = poly1$rho,nfactors = 4,rotate = "varimax",fm = "ml")
fa1$loadings

# CFA ---------------------------------------------------------------------
select(data, c(per_esfuerzo,per_talento,per_famrica,per_contact,pref_esfuerzo,pref_talento,pref_famrica,pref_contact))

model1<- ' 
perc    =~ per_esfuerzo  + per_talento 
pref    =~ pref_esfuerzo + pref_talento
no_perc =~ per_famrica   + per_contact  
no_pref =~ pref_famrica  + pref_contact 
no_perc~~no_pref
'

fit1 <- cfa(model = model1,data = data, ordered = c("per_esfuerzo","per_talento",
                                                    "pref_esfuerzo","pref_talento",
                                                    "per_famrica","per_contact" ,
                                                    "pref_famrica","pref_contact"))

summary(fit1, standardized =TRUE,fit.measures=TRUE)

fitMeasures(fit1,c("chisq","df","pvalue","cfi","tli","rmsea"))

modelp1 <- semPlot::semPaths(fit1,
                             what = "std",
                             layout = "tree2", 
                             style = "lisrel",
                             curvature = 3,
                             residuals = TRUE,
                             intercepts = FALSE,
                             thresholds = FALSE)


