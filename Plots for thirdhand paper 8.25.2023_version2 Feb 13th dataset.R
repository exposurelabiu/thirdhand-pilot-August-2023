##################################################
##################################################
##################################################

####Real time and chemical data####

#Set working directory (tell R where to get files)

input.dir<-("C:/Users/scommod/Desktop/Exposure Laboratory - EL@IU/HomeHealthBox vaping data") #change this to where ever the data are located on your pc

setwd(input.dir)
dir()


# get data file
library(readr)

#Exposure data
edata <- read_csv("PSP00096_LOG_2023-02-13T15_24_09UTC_---------------_----------.txt", 
         skip = 102)
View(edata)

#Control data
cdata <- read_csv("PSP00050_LOG_2023-02-13T15_24_31UTC_---------------_----------.txt", 
                  skip = 102)
View(cdata)

#tells me which are numbers, integers, characters/words in the columns
str(edata)

#tells me names of the columns
names(edata)

#Pick columns I want
edata1 <- edata[c(5,61, 63, 65, 67, 111, 112)]
#Change names of columns

colnames(edata1) <- c("DT", "PM1", "PM2.5", "PM4", "PM10", "VOCs", "NOx")
names(edata1)

###################################################################
#Get control data

cdata1 <- cdata[c(5,61, 63, 65, 67, 111, 112)]
names(cdata1)
#Change names of columns

colnames(cdata1) <- c("DT", "PM1", "PM2.5", "PM4", "PM10", "VOCs", "NOx")

str(edata1)
str(cdata1)

#write new data set and look at times for actual one hour exposures (11:25am)
write.csv(cdata1, file="cdata.csv", row.names = F)
write.csv(edata1, file="edata.csv", row.names = F)
dir()

#############
library(ggplot2)
library(dplyr)

########################
#Setting up, just in case I want to look at hourly data


#exposure
edata1$DateTime <- (format(as.POSIXct(edata1$DT), format = "%Y-%m-%d %H"))
head(edata1)
str(edata1)


#control
cdata1$DateTime <- (format(as.POSIXct(cdata1$DT), format = "%Y-%m-%d %H"))
head(cdata1)
str(cdata1)

#Not worthwhile!

###########################
#Minute by minute data
#exposure plots

p1 <- ggplot(edata1, aes(x=DT, y=PM1)) +
  geom_line() + 
  xlab("Time") + 
  ylab("PM1, ug/m3")
p1

p2 <- ggplot(edata1, aes(x=DT, y=PM2.5)) +
  geom_line() + 
  xlab("Time") + 
  ylab("PM2.5, ug/m3")
p2

p3 <- ggplot(edata1, aes(x=DT, y=PM10)) +
  geom_line() + 
  xlab("Time") + 
  ylab("PM10, ug/m3")
p3

###############################################
c1 <- ggplot(cdata1, aes(x=DT, y=PM1)) +
  geom_line() + 
  xlab("Time") + 
  ylab("PM1, ug/m3")
c1

c2 <- ggplot(cdata1, aes(x=DT, y=PM2.5)) +
  geom_line() + 
  xlab("Time") + 
  ylab("PM2.5, ug/m3")
c2

c3 <- ggplot(cdata1, aes(x=DT, y=PM10)) +
  geom_line() + 
  xlab("Time") + 
  ylab("PM10, ug/m3")
c3
#############################################################
#merge PM datasets
#create exposure vs control before merging

exposure_status = "Control"
cdata1$exposure_status <- exposure_status

exposure_status = "Exposed"
edata1$exposure_status <- exposure_status

all_exp_data <- merge(cdata1, edata1, by = 'DT')
#############################################################


library(ggplot2)
library(ggpubr)
library(gridExtra)
#Control "#00AFBB", 
#Exposed "#FC4E07"
#####################################################################
#Get just PM10 and PM2.5 data and make plots

#create subset, first for PM2.5
names(all_exp_data)
only_pm2.5<-all_exp_data[,c(1,3,10)]
head(only_pm2.5)

#reshape the data
library(tidyr)
only_pm2.5a <-only_pm2.5 %>% pivot_longer(cols=c('PM2.5.x',  'PM2.5.y'),
                                          names_to='measurement',
                                          values_to='value')
only_pm2.5a$exposure_status<-ifelse(only_pm2.5a$measurement=="PM2.5.y","Exposed","Control")
head(only_pm2.5a)

#####################################################################
#Create subset for PM10 only
names(all_exp_data)
only_pm10<-all_exp_data[,c(1,5,12)]
head(only_pm10)

#reshape the data
library(tidyr)
only_pm10a <-only_pm10 %>% pivot_longer(cols=c('PM10.x',  'PM10.y'),
                                          names_to='measurement',
                                          values_to='value')
only_pm10a$exposure_status<-ifelse(only_pm10a$measurement=="PM10.y","Exposed","Control")
head(only_pm10a)

#####################################################################
library("ggpubr")
p2.5b<-ggboxplot(only_pm2.5a, x = "exposure_status", y = "value", 
                 color = "exposure_status", palette = c("#00AFBB", "#FC4E07"),
                 order = c("Control", "Exposed"),
                 ylab = "PM2.5, ug/m3", xlab = "")
#####

library("ggpubr")
p10b<-ggboxplot(only_pm10a, x = "exposure_status", y = "value", 
                 color = "exposure_status", palette = c("#00AFBB", "#FC4E07"),
                 order = c("Control", "Exposed"),
                 ylab = "PM10, ug/m3", xlab = "")
#ranked sum test

res1 <- wilcox.test(value ~ exposure_status, data = only_pm10a,
                   exact = FALSE)
res1
######
res2 <- wilcox.test(value ~ exposure_status, data = only_pm2.5a,
                    exact = FALSE)
res2

####
#Get summary stats 
library(plyr)
#
ddply(only_pm10a, c("exposure_status"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE), min = min(value, na.rm=TRUE), max = max(value, na.rm=TRUE),
      `10%`=quantile(value, probs=0.10, na.rm=TRUE),
      `50%`=quantile(value, probs=0.5, na.rm=TRUE),
       IQR = IQR(value, na.rm = TRUE),
      `90%`=quantile(value, probs=0.90, na.rm=TRUE), `99%`=quantile(value, probs=0.99, na.rm=TRUE), n=length(value))
#
ddply(only_pm2.5a, c("exposure_status"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE), min = min(value, na.rm=TRUE), max = max(value, na.rm=TRUE),
      `10%`=quantile(value, probs=0.10, na.rm=TRUE),
      `50%`=quantile(value, probs=0.5, na.rm=TRUE),
       IQR = IQR(value, na.rm = TRUE),
      `90%`=quantile(value, probs=0.90, na.rm=TRUE), `99%`=quantile(value, probs=0.99, na.rm=TRUE), n=length(value))

#################
# Area plot
p10l<-ggplot(only_pm10a, aes(x = DT, y = value)) + 
  geom_area(aes(color = exposure_status, fill = exposure_status), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + labs(title = "PM10 levels before and during towel exposure", x = "Time", y = "PM10, ug/m3")

####
p2.5l<-ggplot(only_pm2.5a, aes(x = DT, y = value)) + 
  geom_area(aes(color = exposure_status, fill = exposure_status), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + labs(title = "PM2.5 levels before and during towel exposure", x = "Time", y = "PM2.5, ug/m3")

#####

#Create figure 2
ggarrange(
  p10l, p2.5l, labels = c("A", "B"),
  common.legend = TRUE, legend = "bottom"
)
##############################
#get mass spec data
library(readxl)
msdata <- read_excel("mass_spec_ratio_data_8.16.23.xlsx", 
                       sheet = "ratios")

names(msdata)
colnames(msdata) <-c('sample_id','exposure_status','two_aminocyanoacetamide','methylene_chloride','two_methyl_propanal','methacrolein','Diazene','two_methyl_2_pentene','two_3_dihydrofuran','three_methyl_butanal','four_methyl_1_2_dioxolane','two_3_pentadione','four_methyl_cyclohexanol','two_5_dimethylfuran','two_4_dimethylfuran','one_methyl_1H_pyrrole','dimethyldisulfide','three_4_pentadienal','hexanal','two_ethyl_4_methyl_1_3_dioxolane','one_octene','furfural','two_4_Dihydroxy_2_5_dimethyl_3_2H_furan_3_one','benzyl_chloride','one_chlorooctane','two_5_6_trimethyldecane','five_ethyl_decane','nicotine')
names(msdata)

#remove last row since it has vape juice info
library(dplyr)
msdata <- msdata %>% filter(row_number() <= n()-1)

#get boxplots of two significant compunds from metaboanalyst results

#two_5_dimethylfuran
library("ggpubr")
comp1<-ggboxplot(msdata, x = "exposure_status", y = "two_5_dimethylfuran", 
          color = "exposure_status", palette = c("#00AFBB", "#FC4E07"),
          order = c("Control", "Exposed")) +
  labs(title = "Relative abundance of 2,5-dimethylfuran \n in control and exposed towels", x = "Type of towel", y = "Ratio of 2,5-dimethylfuran")

#four_methyl_1_2_dioxolane
library("ggpubr")
comp2<-ggboxplot(msdata, x = "exposure_status", y = "four_methyl_1_2_dioxolane", 
          color = "exposure_status", palette = c("#00AFBB", "#FC4E07"),
          order = c("Control", "Exposed")) +
labs(title = "Relative abundance of 4-methyl-1,2-dioxolane \n in control and exposed towels", x = "Type of towel", y = "Ratio of 4-methyl-1,2-dioxolane")

#Figure 3 is from the MetaboAnalyst download file
#Create figure 4
ggarrange(
  comp1, comp2, labels = c("A", "B"),
  common.legend = TRUE, legend = "bottom"
)
#############################################################
#############################################################
#############################################################

####Cytokine data####

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
data$mouse.age = with(data, as.Date(data$Sacrifice_Date, "%Y-%m-%d") - as.Date(data$DOB, "%Y-%m-%d"))

#make exposure 0 vs 1
data$label<-ifelse(data$Exposure_status=="Exposed",1,0)

head(data)
str(data)
######################################################

#Cytokine data
names(data)

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
bb <-balf_cyto %>% pivot_longer(cols=c('BALF_GM_CSF', 'BALF_IFN_gamma', 'BALF_IL_1_alpha', 'BALF_IL_1_beta',  'BALF_IL_2', 'BALF_IL_4', 'BALF_IL_5', 'BALF_IL_6', 'BALF_IL_7',    'BALF_IL_10', 'BALF_IL_12p70', 'BALF_IL_13', 'BALF_IL_17A', 'BALF_KC',        
                                       'BALF_LIX', 'BALF_MCP_1', 'BALF_MIP_2', 'BALF_TNF_alpha'),
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
#SERUM
ddply(s, c("label", "protein"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE), min = min(value, na.rm=TRUE), max = max(value, na.rm=TRUE),
      `10%`=quantile(value, probs=0.10, na.rm=TRUE),
      `50%`=quantile(value, probs=0.5, na.rm=TRUE),
      IQR = IQR(value, na.rm = TRUE),
      `90%`=quantile(value, probs=0.90, na.rm=TRUE), `99%`=quantile(value, probs=0.99, na.rm=TRUE), n=length(value))

#BALF (n code taken out since sample sizes vary)
ddply(b, c("label", "protein"), summarise,
            mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE), min = min(value, na.rm=TRUE), max = max(value, na.rm=TRUE),
            `10%`=quantile(value, probs=0.10, na.rm=TRUE),
            `50%`=quantile(value, probs=0.5, na.rm=TRUE),
            IQR = IQR(value, na.rm = TRUE),
            `90%`=quantile(value, probs=0.90, na.rm=TRUE), `99%`=quantile(value, probs=0.99, na.rm=TRUE))

######################################################################
#These models were run individually for all serum and BALF and a table was created (TABLE 3)

#18_serum
res_aov <- aov(log(serum_TNF_alpha) ~ Exposure_status,
               data = all_cyto)
summary(res_aov)
#18_balf
res_aov <- aov(log(BALF_TNF_alpha) ~ Exposure_status,
               data = all_cyto)
summary(res_aov)

#####################
#Then based on Table 3 results, further models were run with age and sex of mouse
#IL-7
res_aov <- aov(log(serum_IL_7) ~ Exposure_status + Sex + mouse.age + Exposure_status*mouse.age*Sex,
               data = all_cyto)
summary(res_aov)

#IL-13
res_aov <- aov(log(BALF_IL_13) ~ Exposure_status + Sex + mouse.age + Exposure_status*mouse.age*Sex,
               data = all_cyto)
summary(res_aov)

#BALF_IL_1_beta
res_aov <- aov(log(BALF_IL_1_beta) ~ Exposure_status + Sex + mouse.age + Exposure_status*mouse.age*Sex,
               data = all_cyto)
summary(res_aov)

#BALF_IL_12p70
res_aov <- aov(log(BALF_IL_12p70) ~ Exposure_status + Sex + mouse.age + Exposure_status*mouse.age*Sex,
               data = all_cyto)
summary(res_aov)
#
#####################
#box plots created for IL-7 in serum and IL-13, IL-1B and IL-12p70 in balf samples


all_cyto$Exposure_status <- factor(all_cyto$Exposure_status, levels=c("Exposed", "Control"))

a<-ggplot(all_cyto, aes(Exposure_status, log(BALF_IL_13) , fill =Exposure_status)) +
  geom_boxplot() +
  labs(x = "", y = "log BALF IL-13 in pg/ml", fill = "") +
  facet_wrap(~mouse.age*Sex, scales = "free") + ggtitle("IL-13 BALF concentrations by sex (M or F) \n and age (48 days or 75 days) [logscale]")

b<-ggplot(all_cyto, aes(Exposure_status, BALF_IL_13 , fill =Exposure_status)) +
  geom_boxplot() +
  labs(x = "", y = "BALF IL-13 in pg/ml", fill = "") +
  facet_wrap(~mouse.age*Sex, scales = "free") + ggtitle("IL-13 BALF concentrations by sex (M or F) \n and age (48 days or 75 days) [original concentrations]")
####

c<-ggplot(all_cyto, aes(Exposure_status, log(BALF_IL_1_beta) , fill =Exposure_status)) +
  geom_boxplot() +
  labs(x = "", y = "log of BALF IL-1B in pg/ml", fill = "") +
  facet_wrap(~mouse.age*Sex, scales = "free") + ggtitle("IL-1B concentrations in BALF by sex (M or F) \n and age (48 days or 75 days) [logscale]")

d<-ggplot(all_cyto, aes(Exposure_status, BALF_IL_1_beta , fill =Exposure_status)) +
  geom_boxplot() +
  labs(x = "", y = "BALF IL-1B in pg/ml", fill = "") +
  facet_wrap(~mouse.age*Sex, scales = "free") + ggtitle("IL-1B concentrations in BALF by sex (M or F) \n and age (48 days or 75 days) [original concentrations]")

####

e<-ggplot(all_cyto, aes(Exposure_status, log(BALF_IL_12p70) , fill =Exposure_status)) +
  geom_boxplot() +
  labs(x = "", y = "log BALF IL-12p70 in pg/ml", fill = "") +
  facet_wrap(~mouse.age*Sex, scales = "free") + ggtitle("IL-12p70 concentrations in BALF by sex (M or F) \n and age (48 days or 75 days) [logscale]")

f<-ggplot(all_cyto, aes(Exposure_status, BALF_IL_12p70 , fill =Exposure_status)) +
  geom_boxplot() +
  labs(x = "", y = "BALF IL-12p70 in pg/ml", fill = "") +
  facet_wrap(~mouse.age*Sex, scales = "free") + ggtitle("IL-12p70 concentrations in BALF by sex (M or F) \n and age (48 days or 75 days) [original concentrations]")

######
g<-ggplot(all_cyto, aes(Exposure_status, log(serum_IL_7) , fill =Exposure_status)) +
  geom_boxplot() +
  labs(x = "", y = "log serum IL-7 in pg/ml", fill = "") +
  facet_wrap(~mouse.age*Sex, scales = "free") + ggtitle("IL-7 concentrations in serum by sex (M or F) \n and age (48 days or 75 days) [logscale]")

h<-ggplot(all_cyto, aes(Exposure_status, serum_IL_7 , fill =Exposure_status)) +
  geom_boxplot() +
  labs(x = "", y = "Serum IL-7 in pg/ml", fill = "") +
  facet_wrap(~mouse.age*Sex, scales = "free") + ggtitle("IL-7 concentrations in serum by sex (M or F) \n and age (48 days or 75 days) [original concentrations]")

######

#Create figure 5
ggarrange(
  h, b, d, f, labels = c("A", "B","C", "D"),
  common.legend = TRUE, legend = "bottom"
)

#Create graphical abstract figure

one<-ggplot(all_cyto, aes(Exposure_status, (BALF_IL_13) , fill =Exposure_status)) +
  geom_boxplot() +
  labs(x = "", y = "BALF IL-13 in pg/ml", fill = "") +
   ggtitle("IL-13 concentrations in BALF")
two<-ggplot(all_cyto, aes(Exposure_status, log(serum_IL_7) , fill =Exposure_status)) +
  geom_boxplot() +
  labs(x = "", y = "serum IL-7 in pg/ml", fill = "") +
   ggtitle("IL-7 concentrations in serum")

######
#graphical abstract
ggarrange(
  one, two, labels = c("i)", "ii)"),
  common.legend = TRUE, legend = "bottom"
)
############################################################################
#Organ weights

org_weights<-data[,c(3:5, 7:9, 79:80)]
names(org_weights)
#Lung
res_aov <- aov((lung_weight) ~ Exposure_status,
               data = org_weights)
summary(res_aov)
#Liver
res_aov <- aov((liver_weight) ~ Exposure_status,
               data = org_weights)
summary(res_aov)
#Brain
res_aov <- aov((brain_weight) ~ Exposure_status,
               data = org_weights)
summary(res_aov)

#############

#logscale

#Lung
res_aov <- aov(log(lung_weight) ~ Exposure_status,
               data = org_weights)
summary(res_aov)
#Liver
res_aov <- aov(log(liver_weight) ~ Exposure_status,
               data = org_weights)
summary(res_aov)
#Brain
res_aov <- aov(log(brain_weight) ~ Exposure_status,
               data = org_weights)
summary(res_aov)
############################################################################
#Mouse weights ###

mouse_weight<-data[,c(3:5, 10:16, 1,2, 79:80)]
names(mouse_weight)
head(mouse_weight)

w <-mouse_weight %>% pivot_longer(cols=c('mouse_weight_0','mouse_weight_1',  'mouse_weight_2',  'mouse_weight_3',  'mouse_weight_4', 'mouse_weight_5',  'mouse_weight_6'),
                                names_to='weight_time',
                                values_to='value')
library(lme4)
#this model did not converge
#lmer(value~Exposure_status + Sex + weight_time + Exposure_status*weight_time + (1 | Mouse_ID / Cage_Number / DOB),data=w,REML=TRUE) 


lm<-lmer(value~Exposure_status + Sex + weight_time + (1 | Mouse_ID / Cage_Number / DOB),data=w) 
summary(lm)
plot(lm)

#########
#Model in use in thirdhand ENDS current paper
#just id as random effects
lm_id<-lmer(value~Exposure_status + Sex + weight_time + Exposure_status*weight_time + (1 | Mouse_ID),data=w,REML=TRUE) 
summary(lm_id)

library(car)
car::Anova(lm, type = 3)
car::Anova(lm_id, type = 3)

############################################################################
#Add cell count data from BALF 
#Using only Macrophage_Count, Neutrophil_Count, Eosinophil_Count and Lymphocyte_Count                "Lymphocyte_Count_1" 
counts<-data[,c(3:5, 79:80, 59, 60,67,72, 61:64, 68:71, 73:76, 65,77)]
names(counts)
counts$Mean_tot_cell <- rowMeans(counts[,6:8], na.rm = TRUE)
counts$Mean_Macrophage_Count <- rowMeans(counts[,c(9,13,17)], na.rm = TRUE)
counts$Mean_Neutrophil_Count <- rowMeans(counts[,c(10,14,18)], na.rm = TRUE)
counts$Mean_Eosinophil_Count <- rowMeans(counts[,c(11,15,19)], na.rm = TRUE)
counts$Mean_Lymphocyte_Count <- rowMeans(counts[,c(12,16,20)], na.rm = TRUE)
counts$Mean_DM_Count <- rowMeans(counts[,c(21,22)], na.rm = TRUE)
############
#BAL_Total_Cells_ml
res_aov <- aov((BAL_Total_Cells_ml) ~ Exposure_status,
               data = counts)
summary(res_aov)
#log transformation
res_aov <- aov(log(BAL_Total_Cells_ml) ~ Exposure_status,
               data = counts)
summary(res_aov)

##############
cell<-ggplot(counts, aes(Exposure_status, log(BAL_Total_Cells_ml) , fill =Exposure_status)) +
  geom_boxplot() +
  labs(x = "", y = "Total cells x 10,000 in BALF per ml", fill = "") +
  facet_wrap(~mouse.age*Sex, scales = "free") + ggtitle("Total cells in BALF by sex (M or F) and age (48 days or 75 days) [logscale]")
cell

############
cell2<-ggplot(counts, aes(Exposure_status, (BAL_Total_Cells_ml) , fill =Exposure_status)) +
  geom_boxplot() +
  labs(x = "", y = "Total cells in BALF per ml", fill = "") +
  facet_wrap(~mouse.age*Sex, scales = "free") + ggtitle("Total cells in BALF by sex (M or F) and age (48 days or 75 days)")
cell2

####
#Summary of cell counts

tapply(counts$BAL_Total_Cells_ml, counts$Exposure_status, summary)
##############
#Fit one way anova models to see if exposure differs by BAL cell counts
res_aov <- aov((Mean_Macrophage_Count) ~ Exposure_status,
               data = counts)
summary(res_aov)
###
#Dividing macrophages
res_aov <- aov((Mean_DM_Count) ~ Exposure_status,
               data = counts)
summary(res_aov)


####
res_aov <- aov((Mean_Neutrophil_Count) ~ Exposure_status,
               data = counts)
summary(res_aov)
####
res_aov <- aov((Mean_Eosinophil_Count) ~ Exposure_status,
               data = counts)
summary(res_aov)
####
res_aov <- aov((Mean_Lymphocyte_Count) ~ Exposure_status,
               data = counts)
summary(res_aov)
###########
