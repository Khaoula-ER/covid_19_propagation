#' ********************************************************************************
#' @title 	            data importation.R                                       
#' @author              Khaoula Aroui                                            
#' @description 	      
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' ********************************************************************************

#Vider l'environement ----
# set the working directory ----
setwd("C:/Users/khaou/Documents/Master SEP/5- Analyse des données et data mining_GUATHERAT/projet individuel")

# Importation des données ----
path_data=paste(getwd(),"/Data/analysis_data", sep="")
data_overall=read.csv(paste(path_data, "/Final_data.csv", sep=""))
data_10_av=read.csv(paste(path_data, "/data_10_avril.csv", sep=""))
data_12_dec=read.csv(paste(path_data, "/data_12_dec.csv", sep=""))
