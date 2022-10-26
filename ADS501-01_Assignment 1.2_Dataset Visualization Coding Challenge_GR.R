#' ---
#' title: "ADS501-01: Assignment 1.2 Dataset Visualization"
#' author: "Gabi Rivera"
#' date: "10Sep2022"
#' ---

#Libraries
library(tidyverse)
library(skimr)
library(corrplot)
library(ggplot2)
library(ggpubr)

##Part I
#Create a scatter plot using the Iris Dataset comparing 
  #Petal Length to Petal Width by Species or 
  #Sepal Length to Sepal Width by species. 

#Import data
irs = read.csv("Iris.csv", header = TRUE, sep = ",")
head(irs)

#General informations
skim(irs)

#Correlation
n_iris = subset(irs, select = c("SepalLengthCm", "SepalWidthCm", 
                                 "PetalLengthCm", "PetalWidthCm"))
c_iris = cor(n_iris, y = NULL, use = "complete.obs", method = "pearson")
corrplot.mixed(c_iris, title = "Correlation Matrix",
         tl.col = "Black", tl.srt =40, mar=c(1,0,2,0), 
         tl.pos = 'lt', diag = 'l')

#Petal Length to Petal Width by Species
ggplot(irs, aes(PetalLengthCm, PetalWidthCm, color = Species)) + 
       geom_point() + labs(title = "Petal Length to Petal Width by Species")

  #Petal cumulative linear regression
  ggplot(irs, aes(PetalLengthCm, PetalWidthCm)) + 
    geom_point() + stat_smooth(method = 'lm') +
    stat_regline_equation(label.y = 2.2, aes(label = ..eq.label..)) +
    stat_regline_equation(label.y = 2.1, aes(label = ..rr.label..))

  #Petal species linear regression
  ggscatter(irs, x = "PetalLengthCm", y = "PetalWidthCm",color = "Species",
    add = "reg.line") + facet_wrap(~Species) + stat_cor(label.y = 2.3) +
    stat_regline_equation(label.y = 2.5)


#Sepal Length to Sepal Width by species
ggplot(irs, aes(SepalLengthCm, SepalWidthCm, color = Species)) + 
  geom_point() + labs(title = "Sepal Length to Sepal Width by Species")

  #Sepal cumulative linear regression
  ggplot(irs, aes(SepalLengthCm, SepalWidthCm)) + 
    geom_point() + stat_smooth(method = 'lm') +
    stat_regline_equation(label.y = 4.5, aes(label = ..eq.label..)) +
    stat_regline_equation(label.y = 4.4, aes(label = ..rr.label..))
  
  #Sepal species linear regression
  ggscatter(irs, x = "SepalLengthCm", y = "SepalWidthCm", color = "Species",
    add = "reg.line") + facet_wrap(~Species) + stat_cor(label.y = 4.5) +
    stat_regline_equation(label.y = 4.6)


##Part II
#Interpret your scatter plot. 
  #At a minimum your interpretation should describe 
  #if there is any relationship between the variables and amongst the species.  
  #Identify how strong the relationship appears, 
  #if there are any major outliers, and if there are any notable findings.

#Petal Length to Petal Width by Species:
  #Petal length and petal width both measured in centimeters have a correlation 
  #score of 96%. The r-squared of petal length against petal width is at 93%. 
  #All these suggest a strong positive correlation between the two variables. 
  #The cumulative linear regression visually support the calculated relationship 
  #as well. The individual dots are close to regression line. As a whole, petal 
  #length has the propensity to predict the petal width. 
  #Looking at individual species however, it seems that only Iris-versicolor 
  #score a high positive correlation at 79% r-squared with 0.5 alpha. The other 
  #two species are at less than 35% r-squared. Iris-setosa and Iris-virginica 
  #seems to have a weak correlation pattern at greater than 30 observations. 
  #Visually, the samples of both species have wider spread along 
  #each of their regression line compared to Iris-versicolor. But noticeably,
  #each cluster among their own species precisely. The clustering can be used 
  #for categorical differentiation across or within species. 
  

#Sepal Length to Sepal Width by Species:  
  #Sepal length and sepal width both measured in centimeters have a negative 
  #correlation score of -11%. The r-squared of sepal length against sepal width
  #is at 1.2%. All these suggest a weak negative correlation between the two 
  #variables.The cumulative linear regression visually support the calculated 
  #relationship with the individual dots widely spread along the regression line. 
  #There doesn't seem to be any pattern as a whole. 
  #Within species however, shows relatively stronger relationships between sepal
  #length and sepal width compared to the cumulative observation. Iris-setosa
  #has 75% r-squared followed by Iris-versicolor at 53% then of Iris-virginica
  #at 46%. Just like in petals, the sepal clustering within species can be used 
  #to differentiate each species using the two variables. 

  
  
  
  
  
  
  
  
  

