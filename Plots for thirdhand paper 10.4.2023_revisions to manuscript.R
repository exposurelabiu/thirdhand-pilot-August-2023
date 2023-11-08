#Revisions for thirdhand manuscript

setwd("C:/Users/scommod/Desktop/Exposure Laboratory - EL@IU/Thirdhand towel only experiment")

dir()

library(readxl)
data <- read_excel("thirdhand masterdata 8-24-23.xlsx", 
                   sheet = "masterdata", skip = 0) 
View(data)
str(data)

#Change columns that need to be numeric to numbers

num.cols <- c("mouse_weight_5", "mouse_weight_6", "Nicotine", "Cotinine", "three_OH_Cotinine", "Cotinine_N_Oxide", "serum_IL_2", "BALF_GM_CSF", "BALF_IL_17A", "BALF_LIX", "Macrophage_Count_2","Neutrophil_Count_2","Eosinophil_Count_2","Lymphocyte_Count_2","Macrophage_Count_3", "Neutrophil_Count_3", "Eosinophil_Count_3", "Lymphocyte_Count_3", "Dividing_Macrophage_3")
data[num.cols] <- sapply(data[num.cols], as.numeric)
str(data)

#Load needed packages

library(labelled)   # labeling data
library(rstatix)    # summary statistics
library(ggpubr)     # convenient summary statistics and plots
library(GGally)     # advanced plot
library(car)        # useful for anova/wald test
library(Epi)        # easy getting CI for model coef/pred
library(lme4)       # linear mixed-effects models
library(lmerTest)   # test for linear mixed-effects models
library(emmeans)    # marginal means
library(multcomp)   # CI for linear combinations of model coef
library(geepack)    # generalized estimating equations
library(ggeffects)  # marginal effects, adjusted predictions
library(gt)         # nice tables

library(tidyverse)  # for everything (data manipulation, visualization, coding, and more)
theme_set(theme_minimal() + theme(legend.position = "bottom")) # theme for ggplot

#Calculate age of mice

#mouse age in days
data$mouse.age = with(data, as.POSIXct(data$Sacrifice_Date, "%Y-%m-%d") - as.POSIXct(data$DOB, "%Y-%m-%d"))

#make exposure 0 vs 1
data$label<-ifelse(data$Exposure_status=="Exposed",1,0)

head(data)
str(data)
######################################################

#Cytokine data
names(data)
#Save data

write.csv(data, "C:/Users/scommod/Desktop/Exposure Laboratory - EL@IU/Thirdhand towel only experiment/Data sets/PM data/thirdhand_mouse_R_data.csv", row.names=FALSE)

#create subset of cytokines in serum and balf
serum_cyto<-data[,c(4:5, 21:38, 58, 79:80)]
balf_cyto<-data[,c(4:5, 39:56, 58, 79:80)]
balf_cyto2<-data[,c(4:5, 40:56, 58, 79:80)]
all_cyto<-data[,c(4:5, 21:56, 57:58, 79:80)]
head(serum_cyto)
head(balf_cyto)

s <-serum_cyto %>% pivot_longer(cols=c('serum_GM_CSF', 'serum_IFN_gamma', 'serum_IL_1_alpha', 'serum_IL_1_beta',  'serum_IL_2', 'serum_IL_4', 'serum_IL_5', 'serum_IL_6', 'serum_IL_7',    'serum_IL_10', 'serum_IL_12p70', 'serum_IL_13', 'serum_IL_17A', 'serum_KC',        
                                       'serum_LIX', 'serum_MCP_1', 'serum_MIP_2', 'serum_TNF_alpha'),
                                names_to='protein',
                                values_to='value')

#Using balf_cyto2 data with no BALF_GM_CSF due to all NAs
b <-balf_cyto2 %>% pivot_longer(cols=c('BALF_IFN_gamma', 'BALF_IL_1_alpha', 'BALF_IL_1_beta',  'BALF_IL_2', 'BALF_IL_4', 'BALF_IL_5', 'BALF_IL_6', 'BALF_IL_7',    'BALF_IL_10', 'BALF_IL_12p70', 'BALF_IL_13', 'BALF_IL_17A', 'BALF_KC',        
                                       'BALF_LIX', 'BALF_MCP_1', 'BALF_MIP_2', 'BALF_TNF_alpha'),
                                names_to='protein',
                                values_to='value')
######################################################################
#order by exposure status (label)
#TABLE 2
#Summary stats
library(dplyr)
#All mice in each exposure category
#SERUM
ddply(s, c("label", "protein"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

#BALF 
ddply(b, c("label", "protein"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

######################################################################
#by sex

#SERUM
ddply(s, c("label", "protein", "Sex"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

#BALF 
ddply(b, c("label", "protein", "Sex"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

######################################################################
#by Age

#SERUM
ddply(s, c("label", "protein", "mouse.age"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

#BALF 
ddply(b, c("label", "protein", "mouse.age"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

