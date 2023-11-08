

data$Sex2<-ifelse(data$Sex=="M","MALE","FEMALE")
names(data)
dd<-data[,c(4:5, 7:9, 17:20, 79:81)]
names(dd)

d <-dd %>% pivot_longer(cols=c('lung_weight', 'liver_weight', 'brain_weight', 'Nicotine', 'Cotinine', 'three_OH_Cotinine', 'Cotinine_N_Oxide'),
                             names_to='measure',
                             values_to='value')

#########################
#Cell count data
dd<-data[,c(4:5, 7:9, 17:20, 79:81)]
names(dd)

#########################
#Summary stats
library(dplyr)
#All mice in each exposure category

ddply(d, c("label", "measure"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

######################################################################
#by sex

ddply(d, c("label", "measure", "Sex"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

######################################################################
#by Age

ddply(d, c("label", "measure", "mouse.age"), summarise,
      mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

######################################################################

res_aov <- aov(log(lung_weight) ~ Exposure_status,
               data = dd)
summary(res_aov)

res_aov <- aov(log(liver_weight) ~ Exposure_status,
               data = dd)
summary(res_aov)

res_aov <- aov(log(brain_weight) ~ Exposure_status,
               data = dd)
summary(res_aov)
########################