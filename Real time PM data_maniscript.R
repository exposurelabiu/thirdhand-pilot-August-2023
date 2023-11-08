#Set working directory (tell R where to get files)

input.dir<-("C:/Users/scommod/Desktop/Exposure Laboratory - EL@IU/Thirdhand towel only experiment/Data sets/PM data") #change this to where ever the data are located on your pc

setwd(input.dir)
dir()


# get data file
library(readr)
library(tidyverse)

#Exposure data
#UPAS serial with PSP00096

edata1 <- read_csv("PSP00096_LOG_2023-02-09T15_34_17UTC_---------------_----------.txt", 
                  skip = 102)
edata2 <- read_csv("PSP00096_LOG_2023-02-13T15_24_09UTC_---------------_----------.txt", 
                   skip = 102)
edata3 <- read_csv("PSP00096_LOG_2023-03-07T15_41_18UTC_---------------_.txt", 
                   skip = 102)

edata<-bind_rows(edata1, edata2, edata3)

View(edata)

#############################################

#Control data
#UPAS serial with PSP00050
cdata1 <- read_csv("PSP00050_LOG_2023-02-09T15_33_14UTC_---------------_----------.txt", 
                   skip = 102)
cdata2 <- read_csv("PSP00050_LOG_2023-02-13T15_24_31UTC_---------------_----------.txt", 
                   skip = 102)
cdata3 <- read_csv("PSP00050_LOG_2023-03-07T15_40_44UTC_---------------_----------.txt", 
                   skip = 102)

cdata<-bind_rows(cdata1, cdata2, cdata3)
View(cdata)

#tells you which are numbers, integers, characters/words in the columns
str(edata)
str(cdata)
#tells you names of the columns
names(edata)
names(cdata)

#Pick columns needed
edata_one <- edata[c(5,61, 63, 65, 67, 111, 112)]
names(edata_one)
colnames(edata_one) <- c("DT", "PM1", "PM2.5", "PM4", "PM10", "VOCs", "NOx")
names(edata_one)
###################################################################
#Get control data

cdata_one <- cdata[c(5,61, 63, 65, 67, 111, 112)]
names(cdata_one)
#Change names of columns

colnames(cdata_one) <- c("DT", "PM1", "PM2.5", "PM4", "PM10", "VOCs", "NOx")

str(edata_one)
str(cdata_one)

#Parse data into date and hour, etc
#Exposure data
edata_one$DATE<-as.Date(edata_one$DT, format="%m/%d/%Y")
edata_one$MON<-format.Date(edata_one$DT, "%m")
edata_one$DAY<-format.Date(edata_one$DT, "%d")
edata_one$HR<-format.Date(edata_one$DT, "%H")

#Control data
cdata_one$DATE<-as.Date(cdata_one$DT, format="%m/%d/%Y")
cdata_one$MON<-format.Date(cdata_one$DT, "%m")
cdata_one$DAY<-format.Date(cdata_one$DT, "%d")
cdata_one$HR<-format.Date(cdata_one$DT, "%H")

#Create summaries
#Exposure data

#Pm2.5
PM2.5_summary_exposure_data <-edata_one %>%
  group_by(DATE, HR) %>%
  summarise(Mean_PM2.5 = mean(PM2.5), sd_PM2.5 = sd(PM2.5))

#Pm10

PM10_summary_exposure_data <-edata_one %>%
  group_by(DATE, HR) %>%
  summarise(Mean_PM10 = mean(PM10), sd_PM10 = sd(PM10))

exp_PM_sum <-cbind(PM2.5_summary_exposure_data, PM10_summary_exposure_data)

#Control data


#Pm2.5
PM2.5_summary_control_data <-cdata_one %>%
  group_by(DATE, HR) %>%
  summarise(Mean_PM2.5 = mean(PM2.5), sd_PM2.5 = sd(PM2.5))

#Pm10

PM10_summary_control_data <-cdata_one %>%
  group_by(DATE, HR) %>%
  summarise(Mean_PM10 = mean(PM10), sd_PM10 = sd(PM10))

#check hourly data to confirm dates of animal introduction to towels
control_PM_sum <-cbind(PM2.5_summary_control_data, PM10_summary_control_data)

#subset by dates
#2/9/2023: 12-1pm
#2/13/2023: 11-12pm
#3/7/2023: 11-12pm
#3/10/2023: 12-1pm

#subtract 5 to give est

exp1 <- subset(edata_one, DT >= as.POSIXct('2023-02-09 06:33:31') &
                 DT <= as.POSIXct('2023-02-09 07:33:31'))
exp2 <- subset(edata_one, DT >= as.POSIXct('2023-02-13 06:00:00') &
                 DT <= as.POSIXct('2023-02-13 07:00:00'))
exp3 <- subset(edata_one, DT >= as.POSIXct('2023-03-07 06:00:00') &
                 DT <= as.POSIXct('2023-03-07 07:00:00'))
exp4 <- subset(edata_one, DT >= as.POSIXct('2023-03-10 07:00:00') &
                 DT <= as.POSIXct('2023-03-10 08:00:00'))
#####

#all real time exposure data
exp_PM_data<-do.call(rbind, list(exp1, exp2, exp3, exp4))

#subtract 5 to give est

con1 <- subset(cdata_one, DT >= as.POSIXct('2023-02-09 06:33:31') &
                 DT <= as.POSIXct('2023-02-09 07:33:31'))
con2 <- subset(cdata_one, DT >= as.POSIXct('2023-02-13 06:00:00') &
                 DT <= as.POSIXct('2023-02-13 07:00:00'))
con3 <- subset(cdata_one, DT >= as.POSIXct('2023-03-07 06:00:00') &
                 DT <= as.POSIXct('2023-03-07 07:00:00'))
con4 <- subset(cdata_one, DT >= as.POSIXct('2023-03-10 07:00:00') &
                 DT <= as.POSIXct('2023-03-10 08:00:00'))
#all real time control data
con_PM_data<-do.call(rbind, list(con1, con2, con3, con4))

####################################################################
#Summary for control data
summary(con_PM_data)
summarize(con_PM_data, PM2.5_mean_control = mean(PM2.5),
          PM2.5_sd_control   = sd(PM2.5),
          PM10_mean_control = mean(PM10),
          PM10_sd_control   = sd(PM10)
          )
###################################################################
summary(exp_PM_data)
summarize(exp_PM_data, PM2.5_mean_exp = mean(PM2.5),
          PM2.5_sd_exp   = sd(PM2.5),
          PM10_mean_exp = mean(PM10),
          PM10_sd_exp   = sd(PM10)
)
####################################################################
#merge data

all_data <- merge(con_PM_data, exp_PM_data, by = 'DT')
names(all_data)
#############################################################


library(ggplot2)
library(ggpubr)
library(gridExtra)
#Control "#00AFBB", 
#Exposed "#FC4E07"
#####################################################################
#Get just PM10 and PM2.5 data and make plots

#create subset, first for PM2.5
names(all_data)
only_pm2.5<-all_data[,c(1,3,13)]
head(only_pm2.5)

only_pm10<-all_data[,c(1,5,15)]
head(only_pm10)

#reshape the data
library(tidyr)
only_pm2.5a <-only_pm2.5 %>% pivot_longer(cols=c('PM2.5.x',  'PM2.5.y'),
                                          names_to='measurement',
                                          values_to='value')
only_pm2.5a$exposure_status<-ifelse(only_pm2.5a$measurement=="PM2.5.y","Exposed","Control")
head(only_pm2.5a)
##

only_pm10a <-only_pm10 %>% pivot_longer(cols=c('PM10.x',  'PM10.y'),
                                          names_to='measurement',
                                          values_to='value')
only_pm10a$exposure_status<-ifelse(only_pm10a$measurement=="PM10.y","Exposed","Control")
head(only_pm10a)
#boxplots

library("ggpubr")
p2.5b<-ggboxplot(only_pm2.5a, x = "exposure_status", y = "value", 
                 color = "exposure_status", palette = c("#00AFBB", "#FC4E07"),
                 order = c("Control", "Exposed"),
                 ylab = "PM2.5, ug/m3", xlab = "")
p2.5b + theme_grey(base_size = 18)

p10b<-ggboxplot(only_pm10a, x = "exposure_status", y = "value", 
                 color = "exposure_status", palette = c("#00AFBB", "#FC4E07"),
                 order = c("Control", "Exposed"),
                 ylab = "PM10, ug/m3", xlab = "")
p10b + theme_grey(base_size = 18)

##
kruskal.test(value ~ exposure_status, data = only_pm2.5a)
kruskal.test(value ~ exposure_status, data = only_pm10a)

pairwise.wilcox.test(only_pm2.5a$value, only_pm2.5a$exposure_status,
                     p.adjust.method = "BH")
#####################################################################
#Wilcox
res1 <- wilcox.test(value ~ exposure_status, data = only_pm10a,
                    exact = FALSE)
res1
######
res2 <- wilcox.test(value ~ exposure_status, data = only_pm2.5a,
                    exact = FALSE)
res2
#####
