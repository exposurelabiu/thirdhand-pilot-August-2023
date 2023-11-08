
#Cell count data
#Add cell count data from BALF 
#Using only Macrophage_Count, Neutrophil_Count, Eosinophil_Count and Lymphocyte_Count                "Lymphocyte_Count_1" 
counts<-data[,c(3:5, 59, 60,67,72, 61:64, 68:71, 73:76, 65,77, 79:81)]
names(counts)
counts$Mean_tot_cell <- rowMeans(counts[,5:7], na.rm = TRUE)
counts$Mean_Macrophage_Count <- rowMeans(counts[,c(8,12,16)], na.rm = TRUE)
counts$Mean_Neutrophil_Count <- rowMeans(counts[,c(9,13,17)], na.rm = TRUE)
counts$Mean_Eosinophil_Count <- rowMeans(counts[,c(10,14,18)], na.rm = TRUE)
counts$Mean_Lymphocyte_Count <- rowMeans(counts[,c(11,15,19)], na.rm = TRUE)
#############
counts$Macrophage<-(counts$Mean_Macrophage_Count/counts$Mean_tot_cell)*counts$BAL_Total_Cells_ml
counts$Neutrophil<-(counts$Mean_Neutrophil_Count/counts$Mean_tot_cell)*counts$BAL_Total_Cells_ml
counts$Eosinophil<-(counts$Mean_Eosinophil_Count/counts$Mean_tot_cell)*counts$BAL_Total_Cells_ml
counts$Lymphocyte<-(counts$Mean_Lymphocyte_Count/counts$Mean_tot_cell)*counts$BAL_Total_Cells_ml

############

#Get 5 variables needed for Table 4

counts1<-counts[,c(2:4, 22:24, 30:33)]
names(counts1)

cc <-counts1 %>% pivot_longer(cols=c('BAL_Total_Cells_ml', 'Macrophage', 'Neutrophil', 'Eosinophil', 'Lymphocyte'),
                                names_to='celltype',
                                values_to='value')


#########################
#Summary stats
library(dplyr)
#All mice in each exposure category

ddply(cc, c("label", "celltype"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

######################################################################
#by sex

ddply(cc, c("label", "celltype", "Sex"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

######################################################################
#by Age

ddply(cc, c("label", "celltype", "mouse.age"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

###################################################################### 



############
#Fit one way anova models to see if exposure differs by cell counts

res_aov <- aov(log(BAL_Total_Cells_ml) ~ Exposure_status,
               data = counts)
summary(res_aov)
#######

res_aov <- aov((Macrophage) ~ Exposure_status,
               data = counts)
summary(res_aov)

####
res_aov <- aov((Neutrophil) ~ Exposure_status,
               data = counts)
summary(res_aov)
####
res_aov <- aov((Eosinophil) ~ Exposure_status,
               data = counts)
summary(res_aov)
####
res_aov <- aov((Lymphocyte) ~ Exposure_status,
               data = counts)
summary(res_aov)
###########
