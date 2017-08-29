library(ggplot2)
library(dplyr)

adult<-read.csv('adult_sal.csv')
adult<-select(adult,-X)

table(adult$type_employer)

ggplot(adult,aes(type_employer))+geom_bar(aes(fill=income))

# Feature Engineering Combine employer.

employer<-function(job){
  
  job<-as.character(job)
  if(job=='Never-worked' | job=='Without-pay')
    return('Unemployed')
  else if(job=='Local-gov' | job=='State-gov')
    return('LS-gov')
  else if(job=='Self-emp-inc' | job=='Self-emp-not-inc')
    return('Self-employed')
  else
    return(job)
  
 }

adult$type_employer<-sapply(adult$type_employer,employer)

table(adult$type_employer)

ggplot(adult,aes(type_employer))+geom_bar(aes(fill=income))

adult$type_employer<-factor(adult$type_employer)

###########################################################################################

# Feature Engineering Marital status

table(adult$marital)
ggplot(adult,aes(marital))+geom_bar(aes(fill=income))

marital_status<-function(status){
  status<-as.character(status)
  if(status=='Divorced' | status=='Separated' | status=='Widowed')
    return('Not Married')
  else if(status=='Never-married')
    return(status)
  else
    return('Married')
}

adult$marital<-sapply(adult$marital,marital_status)

table(adult$marital)

adult$marital<-factor(adult$marital)

######################################## Feature Engineering: Country ######################################################

table(adult$country)



Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)
adult$country<-factor(adult$country)
ggplot(adult,aes(country))+geom_bar(aes(fill=income))


################################## Feature Engineering Education ####################################################

ggplot(adult,aes(education))+geom_bar(aes(fill=income))

school<-c('10th','11th','12th','1st-4th','5th-6th','7th-8th','9th','Preschool')

specialisation<- c('Bachelors','Doctorate','Masters','Prof-school')

education<-function(edu){
  
  if(edu %in% school)
    return('school')
  else if(edu %in% specialisation)
    return('specialisation')
  else
    return('highschool')
}

adult$education<-sapply(adult$education,education)

table(adult$education)






########################### Remove Missing Data ####################################################

adult[adult=='?']<-NA

adult<-na.omit(adult)


########################################## plots #########################################

ggplot(adult,aes(age))+geom_histogram(aes(fill=income),color='black',binwidth = 1)+theme_bw()


#********************** Logistic Regression ********************************************************

library(caTools)

set.seed(101)

sample<-sample.split(adult$income,SplitRatio = 0.7)

train<-subset(adult,sample==T)
test<-subset(adult,sample==F)

model<-glm(income~.,family = binomial(link ="logit" ),data=train)

summary(model)


#step.model<-step(model)

#summary(step.model)

#predict.income<-predict(model,newdata=test,type='response')

test$predict.income<-predict(model,newdata=test,type='response')


table(test$income,test$predict.income>0.5)

accuracy<-(6372+1423)/(6372+1423+548+872)

accuracy #84.6%


ggplot(test,aes(age))+geom_histogram(aes(fill=predict.income>0.5),color='black',binwidth = 1)+theme_bw()

























