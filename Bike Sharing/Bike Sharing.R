library(ggplot2)
library(dplyr)
#Read files

train_bike<-read.csv('train.csv')
test_bike<-read.csv('test.csv')

"Number of column should be same in train and test. Our target is to find total count which is registered+casual.
 So we can build Model to find count directly insted of finding registered+casual."


#Remove registered and causal from training set and then Add count column in test and combine both dataset.

test_bike$count<-NA

train_bike<-select(train_bike,-registered,-casual)

bike<-rbind(train_bike,test_bike)


############################### DateTime VS Count  ############################################

class(bike$datetime) #factor

bike$datetime<-as.POSIXct(bike$datetime)

class(bike$datetime)

ggplot(bike,aes(datetime,count))+geom_point(aes(color=temp),alpha=0.3)+scale_color_continuous(low = 'yellow',high = 'red')+theme_bw()
# over the time demand is incresing.


#############################  Working day vs Demand #################################################

bike$hour <-sapply(bike$datetime,function(x){format(x,"%H")})



ggplot(filter(bike,workingday==1),aes(hour,count))+geom_point(aes(color=temp),position = position_jitter(width = 1,height = 0),alpha=0.5)+scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))+theme_bw()
# Peak hour:6-9 ,12-15,16-19

class(bike$hour) #Character

bike$hour<-as.numeric(bike$hour)

bike$daypart<-0

bike$daypart[bike$hour>=6 & bike$hour<=9]<-1
bike$daypart[bike$hour>=12 & bike$hour<=15]<-1
bike$daypart[bike$hour>=16 & bike$hour<=19]<-1

bike$daypart<-as.factor(bike$daypart)


#############################  holiday vs Demand #################################################

ggplot(filter(bike,workingday==0),aes(hour,count))+geom_point(aes(color=temp),position = position_jitter(width = 1,height = 0),alpha=0.5)+scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))+theme_bw()


#########################################  weekday VS Demand ####################################


bike$weekday<-weekdays(as.Date(bike$datetime))


#ggplot(bike,aes(windspeed,count))+geom_point()

bike$season<-as.factor(bike$season)
bike$holiday<-as.factor(bike$holiday)
bike$workingday<-as.factor(bike$workingday)
bike$weather<-as.factor(bike$weather)
bike$hour<-as.factor(bike$hour)

############################## Splitting Dataset ############################

test<-filter(bike,is.na(count))
train<-filter(bike,!is.na(count))


