#' ---
#' title: "ADS501-01: Assignment 4.2 Dataset Visualization"
#' author: "Gabi Rivera"
#' date: "29Sep2022"
#' ---


#Libraries
library(tidyverse)
library(skimr)
library(corrplot)
library(ggplot2)
library(ggpubr)

#Distribution of various petal lengths or sepal lengths of each species.
irs = read.csv("Iris.csv", header = TRUE, sep = ",")
head(irs)

#General informations
skim(irs)
