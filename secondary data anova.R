######################################################################
#Secondary data variables

library(readr)
weight <- read_csv("co-authors/revisions/start and end mouse weights n=13.csv")

#Get secondary outcome data and merge for anova and subsequent analysis
sec_out1<-data[,c(3, 79)]
names(sec_out1)
sec_out<-merge(sec_out1, weight)
head(sec_out)

w <-sec_out %>% pivot_longer(cols=c('mouse_weight_start', 'mouse_weight_end'),
                                names_to='weight',
                                values_to='value')


#########################
#Summary stats
library(dplyr)
#All mice in each exposure category

ddply(w, c("Label", "weight"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

######################################################################
#by sex

ddply(w, c("Label", "weight", "Sex"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

######################################################################
#by Age

ddply(w, c("Label", "weight", "mouse.age"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

######################################################################

res_aov <- aov(log(mouse_weight_start) ~ Exposure_status,
               data = sec_out)
summary(res_aov)

res_aov <- aov(log(mouse_weight_end) ~ Exposure_status,
               data = sec_out)
summary(res_aov)
########################
#T test
t.test(log(serum_IL_10) ~ Exposure_status, all_cyto, var.equal = TRUE)
t.test(log(BALF_IL_10) ~ Exposure_status, all_cyto, var.equal = TRUE)
#########################


ggplot(df, aes(x=Category, y=Mean, fill=Quality)) + 
  geom_bar(position=position_dodge(), stat="identity", 
           colour='black') + 
  geom_errorbar(aes(ymin=Mean-sd, ymax=Mean+sd), width=.2)

ggplot(df, aes(x=Category, y=Mean, fill=Quality)) + 
  geom_bar(position=position_dodge(), stat="identity", 
           colour='black') + 
  geom_errorbar(aes(ymin=Mean-sd, ymax=Mean+sd), width=.2)

#####
#assign sex as male and female
all_cyto$Sex2<-ifelse(all_cyto$Sex=="M","MALE","FEMALE")

#Create separate data set for IL-7 (serum) AND, IL-13, IL-1B and IL-12p70 (BALF)
#Then create bar plots with sd

h<-ggplot(all_cyto, aes(Exposure_status, serum_IL_7 , fill =Exposure_status)) +
  geom_bar()
  labs(x = "", y = "Mean serum IL-7 +/- sd (pg/ml)", fill = "") +
  facet_wrap(~mouse.age*Sex2, scales = "free") + ggtitle("IL-7 concentrations in serum by sex (M or F) \n and age (48 days or 75 days) ")

  h<-ggplot(all_cyto, aes(serum_IL_7 , fill =Exposure_status)) +
    geom_bar()
  labs(x = "", y = "Mean serum IL-7 +/- sd (pg/ml)", fill = "") +
    facet_wrap(~mouse.age*Sex2, scales = "free") + ggtitle("IL-7 concentrations in serum by sex (M or F) \n and age (48 days or 75 days) ")
  
