#HOTMIC: Micropastic Summer School 2022
#Date Start: 23.09.2022
#Author: Nora Klasen (nora.sophie.klasen@gmail.com)

#Title: Visualize and analyse data obtained from the experiment:
#Effects of new and aged micro PVC on Mytilus edulis

# Session information:
# R version 4.1.2 (2021-11-01)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)

# attached base packages: stats, graphics, grDevices, utils, datasets, methods, base

#libraries
library(readxl)# loading data directly from Excel
library(dplyr) #grouping categories, aggregate() etc. 
library(ggplot2) #plotting & visualisation


# Clear workspace and set working directory
rm(list = ls())
# rm() to remove objects
setwd("/Users/nklasen/Documents/Code/github/HOTMIC_Microplastic_2022")
Data_microplastic_summer_school <- read_excel("/Users/nklasen/Documents/Code/github/HOTMIC_Microplastic_2022/Data_microplastic_summer_school.xlsx")

data<- Data_microplastic_summer_school
str(data)
sapply(data, class)
type <- as.factor(data$Particle_quality)
levels(type)
table(type)

#plotting
dotchart(data$Particle_water, groups = factor(type), color = type, 
         xlab = 'MP frequency', ylab = 'Type')

# Remove outlier in particle water
max(data$Particle_water)
data.rm <- data[ data$Particle_water <339, ]
#it is now unbalanced

# re-plot data 
type.rm <- as.factor(data.rm$Particle_quality)
levels(type.rm)
dotchart(data.rm$Particle_water, groups = factor(type.rm), color = type.rm, 
         xlab = 'MP frequency', ylab = 'Type')

#Boxplot
plot(data.rm$Particle_water ~type.rm)
hist(data.rm$Particle_water)

data.rm$Particle_quality <- c(rep("New PVC with Biofilm", 5), rep("New PVC without Biofilm", 6), rep("Aged PVC without Biofilm", 6))
figure1<-ggplot(data.rm, aes(Particle_quality, Particle_water))
figure1+
  geom_boxplot()+
  geom_point(size=3, col="blue")+
  labs(x="Particle Quality", y="Number of Particles in 1.5 L")+
  
  theme(panel.background = element_blank(), #removes background
        panel.grid=element_blank(), #no grid lines
        axis.line=element_line(colour = "black"), #black axes
        axis.title = element_text(size = 16, colour = "black"),
        axis.text = element_text(size = 12))
        
#Modelling

model<-aov(data.rm$Particle_water ~ type.rm)

#Diagnostics

hist(resid(model))
#Variances Homogeneity
plot(resid(model)~fitted(model))
#fligner test for the homogeneity of variances
fligner.test(data.rm$Particle_water ~ type.rm)
# p=0.3961 insignificant that the variances are homogeneous


#Variance is large. therefore
#Welch test which doesn't assume homogeneity
# applying Welch correction -> non-parametric ANOVA -> Welch ANOVA
oneway.test(data.rm$Particle_water~type.rm)
#  significant p= 0.0285

# Check post-hoc test
#pair-wise t-test, equal variances, between groups
pairwise.t.test(data.rm$Particle_water, type.rm, p.adj="bonferroni")

