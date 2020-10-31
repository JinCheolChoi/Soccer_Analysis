#********************
#
# empty the workspace
#
#********************
rm(list=ls())

#*******************
#
# set directory path
#
#*******************
# CODE.dir.1="C:/Users/JinCheol Choi/Desktop/R/Functions/"
# CODE.dir.2="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/"
# data.dir="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/Data/england/"
# rdata.dir="C:/Users/JinCheol Choi/Desktop/R/Soccer_Analysis/Rdata/"

CODE.dir.1="C:/Users/jchoi02/Desktop/R/Functions/"
CODE.dir.2="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/"
data.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Data/england/"
rdata.dir="C:/Users/jchoi02/Desktop/R/Soccer_Analysis/Rdata/"

#***************
#
# import library
#
#***************
source(paste0(CODE.dir.1, "Functions.R"))
lapply(c("dplyr", 
         "data.table",
         "profvis",
         "magrittr",
         
         "ggplot2"
         
), 
checkpackages)

#************
#
# import data
#
#************
Data_set=fread(paste0(data.dir, "Combined.csv"))

#*************
#
# Data process
#
#*************
source(paste0(CODE.dir.2, "Data_Process.R"))




