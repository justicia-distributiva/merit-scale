#Recode Tratamientos

library(dplyr)
library(sjlabelled)
library(here)
setwd(here())

pob_03 <- haven::read_sav("data/dat1.sav")
data <- pob_03  #502 obs
  

data$TRATAMIENTO <- NA
data$TRATAMIENTO[data$FL_9_DO_Control==1]                  <- 1 #control cigarros
data$TRATAMIENTO[data$FL_9_DO_Tratamiento    ==1]          <- 2 #pobreza
data$TRATAMIENTO[data$FL_9_DO_Tratamiento_desigualdad ==1] <- 3 #Desigualdad


data$POBREZA <- NA
data$POBREZA[data$FL_9_DO_Control==1]                      <- 0 #control cigarros
data$POBREZA[data$FL_9_DO_Tratamiento ==1]                 <- 1 #pobreza

data$DESIGUALDAD <- NA
data$DESIGUALDAD[data$FL_9_DO_Control==1]                  <- 0 #control cigarros
data$DESIGUALDAD[data$FL_9_DO_Tratamiento_desigualdad==1]  <- 1 #Desigualdad


data$TRATAMIENTO <- factor(x = data$TRATAMIENTO,levels = c(1,2,3),labels = c("Control", 
                                                                             "Tratamiento pobreza",
                                                                             "Tratamiento desigualdad"))
data$sexo <- factor(data$sexo,levels = c(1,2), labels = c("Hombre", "Mujer"))
data$edad_anios <- as_factor(data$edad_anios)
data$educat <- as_factor(data$educat)




bye <- 
  c("time_t1a_First_Click",
    "time_t1a_Last_Click",
    "time_t1a_Page_Submit",
    "time_t1a_Click_Count",
    "time_t1b_First_Click",
    "time_t1b_Last_Click",
    "time_t1b_Page_Submit",
    "time_t1b_Click_Count",
    "time_t1c_First_Click",
    "time_t1c_Last_Click",
    "time_t1c_Page_Submit",
    "time_t1c_Click_Count",
    "time_t1d_First_Click",
    "time_t1d_Last_Click",
    "time_t1d_Page_Submit",
    "time_t1d_Click_Count",
    "time_ctrla_First_Click",
    "time_ctrla_Last_Click",
    "time_ctrla_Page_Submit",
    "time_ctrla_Click_Count",
    "time_ctrlb_First_Click",
    "time_ctrlb_Last_Click",
    "time_ctrlb_Page_Submit",
    "time_ctrlb_Click_Count",
    "time_ctrlc_First_Click",
    "time_ctrlc_Last_Click",
    "time_ctrlc_Page_Submit",
    "time_ctrlc_Click_Count",
    "time_ctrld_First_Click",
    "time_ctrld_Last_Click",
    "time_ctrld_Page_Submit",
    "time_ctrld_Click_Count",
    "time_t2a_First_Click",
    "time_t2a_Last_Click",
    "time_t2a_Page_Submit",
    "time_t2a_Click_Count",
    "time_t2b_First_Click",
    "time_t2b_Last_Click",
    "time_t2b_Page_Submit",
    "time_t2b_Click_Count",
    "get_ah_DO_get_ah_1",
    "get_ah_DO_get_ah_2",
    "get_ah_DO_get_ah_3",
    "get_ah_DO_get_ah_4",
    "get_ah_DO_get_ah_5",
    "pref_redis_taxB_DO_1",
    "pref_redis_taxB_DO_2",
    "pref_redis_taxB_DO_3",
    "pref_redis_taxB_DO_4",
    "get_ahB_DO_1",
    "get_ahB_DO_2",
    "get_ahB_DO_3",
    "get_ahB_DO_4",
    "get_ahB_DO_5",
    "get_ahB_DO_6",
    "merit_perc_pref_DO_merit_perc_pref_1",
    "merit_perc_pref_DO_merit_perc_pref_2",
    "merit_perc_pref_DO_merit_perc_pref_3",
    "merit_perc_pref_DO_merit_perc_pref_4",
    "merit_perc_pref_DO_merit_perc_pref_5",
    "merit_perc_pref_DO_merit_perc_pref_13",
    "merit_perc_pref_DO_merit_perc_pref_14",
    "merit_perc_pref_DO_merit_perc_pref_15",
    "egal_DO_egal_1",
    "egal_DO_egal_2",
    "egal_DO_egal_3",
    "egal_DO_egal_4",
    "egal_DO_egal_5",
    "egal_DO_egal_6",
    "egal_DO_egal_7",
    "egal_DO_egal_8",
    "UserLanguage",
    "RecipientLastName",
    "RecipientFirstName")

data <- data %>% select(-bye)
remove(list = c("pob_03","bye"))

save(data,file = "data/pob_04.RData")
rm(list=ls())
