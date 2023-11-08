#mean +/- sd for all 4 'markers'
data$sex<-ifelse(data$Sex=="M","MALE","FEMALE")
data$age<-ifelse(data$mouse.age=="75","75 days","48 days")
il.7<-data[,c(4:5, 29, 79:83)]

names(il.7)

il.72<-ddply(il.7, c("Exposure_status","label", "mouse.age", "sex", "age"), summarise,
      mean = mean(serum_IL_7, na.rm=TRUE), sd = sd(serum_IL_7, na.rm=TRUE))

str(il.72)
il.72$Exposure_status <- as.factor(il.72$Exposure_status)
il.72$age <- as.factor(il.72$age)
il.72$sex <- as.factor(il.72$sex)
str(il.72)

h<-ggplot(il.72, aes(x=Exposure_status, y=mean)) +
  geom_bar(position=position_dodge(), stat="identity")
labs(x = "", y = "Mean serum IL-7 +/- sd (pg/ml)", fill = "sex") +
  facet_wrap(~age*sex, scales = "free") + ggtitle("IL-7 concentrations in serum by sex (M or F) \n and age (48 days or 75 days) ") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)

#####

library(tidyverse)
aa<-il.72 %>% 
  mutate(Exposure_status = fct_rev(Exposure_status)) %>% 
ggplot(aes(x=Exposure_status, y=mean, fill = Exposure_status))+
  geom_bar(stat="identity", position = position_dodge(),  width = .9)+
  facet_grid(age~sex,  space="free_x") +
  geom_errorbar(aes(ymin= mean - sd, ymax = mean + sd), width = 0.2, color = "BLACK", position=position_dodge())+
  theme(legend.title = element_blank())

aa + 
  xlab("Exposure status") +
  ylab("Mean serum IL-7 +/- sd (pg/ml)") +
  ggtitle("A. Serum IL-7 by age and sex")+ theme_grey(base_size = 18) +
  theme(legend.position="none") 

############
b_il.13<-data[,c(4:5, 50, 79:83)]

names(b_il.13)

b_il.132<-ddply(b_il.13, c("Exposure_status","label", "mouse.age", "sex", "age"), summarise,
             mean = mean(BALF_IL_13, na.rm=TRUE), sd = sd(BALF_IL_13, na.rm=TRUE))

str(b_il.132)
b_il.132$Exposure_status <- as.factor(b_il.132$Exposure_status)
b_il.132$age <- as.factor(b_il.132$age)
b_il.132$sex <- as.factor(b_il.132$sex)
str(b_il.132)

bb<-b_il.132 %>% 
  mutate(Exposure_status = fct_rev(Exposure_status)) %>% 
  ggplot(aes(x=Exposure_status, y=mean, fill = Exposure_status))+
  geom_bar(stat="identity", position = position_dodge(),  width = .9)+
  facet_grid(age~sex,  space="free_x") +
  geom_errorbar(aes(ymin= mean - sd, ymax = mean + sd), width = 0.2, color = "BLACK", position=position_dodge())+
  theme(legend.title = element_blank())

bb + 
  xlab("Exposure status") +
  ylab("Mean BALF IL-13 +/- sd (pg/ml)") +
  ggtitle("B. BALF IL-13 by age and sex")+ theme_grey(base_size = 18) +
  theme(legend.position="none") 

############

b_il.1b<-data[,c(4:5, 42, 79:83)]

names(b_il.1b)

b_il.1b2<-ddply(b_il.1b, c("Exposure_status","label", "mouse.age", "sex", "age"), summarise,
                mean = mean(BALF_IL_1_beta, na.rm=TRUE), sd = sd(BALF_IL_1_beta, na.rm=TRUE))

str(b_il.1b2)
b_il.1b2$Exposure_status <- as.factor(b_il.1b2$Exposure_status)
b_il.1b2$age <- as.factor(b_il.1b2$age)
b_il.1b2$sex <- as.factor(b_il.1b2$sex)
str(b_il.1b2)

ee<-b_il.1b2 %>% 
  mutate(Exposure_status = fct_rev(Exposure_status)) %>% 
  ggplot(aes(x=Exposure_status, y=mean, fill = Exposure_status))+
  geom_bar(stat="identity", position = position_dodge(),  width = .9)+
  facet_grid(age~sex,  space="free_x") +
  geom_errorbar(aes(ymin= mean - sd, ymax = mean + sd), width = 0.2, color = "BLACK", position=position_dodge())+
  theme(legend.title = element_blank())

ee + 
  xlab("Exposure status") +
  ylab("Mean BALF IL-1B +/- sd (pg/ml)") +
  ggtitle("C. BALF IL-1B by age and sex") + theme_grey(base_size = 18) +
  theme(legend.position="none") 
######

b_il.12<-data[,c(4:5, 49, 79:83)]

names(b_il.12)

b_il.122<-ddply(b_il.12, c("Exposure_status","label", "mouse.age", "sex", "age"), summarise,
                mean = mean(BALF_IL_12p70, na.rm=TRUE), sd = sd(BALF_IL_12p70, na.rm=TRUE))

str(b_il.122)
b_il.122$Exposure_status <- as.factor(b_il.122$Exposure_status)
b_il.122$age <- as.factor(b_il.122$age)
b_il.122$sex <- as.factor(b_il.122$sex)
str(b_il.122)

ff<-b_il.122 %>% 
  mutate(Exposure_status = fct_rev(Exposure_status)) %>% 
  ggplot(aes(x=Exposure_status, y=mean, fill = Exposure_status))+
  geom_bar(stat="identity", position = position_dodge(),  width = .9)+
  facet_grid(age~sex,  space="free_x") +
  geom_errorbar(aes(ymin= mean - sd, ymax = mean + sd), width = 0.2, color = "BLACK", position=position_dodge())+
  theme(legend.title = element_blank())

ff + 
  xlab("Exposure status") +
  ylab("Mean BALF IL-12p70 +/- sd (pg/ml)") +
  ggtitle("D. BALF IL-12p70 by age and sex") + theme_grey(base_size = 17.5) +
  theme(legend.position="none") 