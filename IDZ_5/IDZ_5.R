# @project IDZ_5
# @author Nikita Fiodorov 
# @date 22.04.2017
# @link https://www.dropbox.com/sh/lnrmixhxjlvq72g/AABfBdX0y8r2f_BOAn3ffcI-a?dl=0
# @data 

list.of.packages <- c("kolmim","rmarkdown","revealjs","dplyr","tidyr","xtable","ggvis","ggplot2", "Rcpp","fitdistrplus")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(dplyr)
library(tidyr)
library(xtable)
library(ggvis)
library(ggplot2)
library(fitdistrplus)
AnnualDiameter<<-as.data.frame(read.csv("annual-changes-in-the-earths-rot1.csv",col.names = c("AnnualDiameter")));
str(AnnualDiameter)
