library(ggplot2)

#install.packages('Rcpp')
#install.packages('dplyr')
library(dplyr)

dply#Step 1: Load the data.

train<-read.csv('train.csv',stringsAsFactors = FALSE)
test<-read.csv('test.csv',stringsAsFactors = FALSE)


# train has 12 variables but test has 11 variables and to combbine both datasets,number of column should be same.
#So add Survived column with NA value in test dataset.

test$Survived<-NA

#Combine both datasets.

full<-rbind(train,test)
str(full)

# Feature engineering with Name.

head(full$Name)

#Take out titles.

strsplit(full$Name,split = '[,.]')[[1]][2]

full$Title<-sapply(full$Name,FUN = function(x){strsplit(x,split = '[,.]')[[1]][2]})

#There is blank space before title which needs to be removed.

full$Title<-sub(" ","",full$Title)

table(full$Title,full$Sex)

Rare_Title<-c('Capt','Col','Don','Dona','Dr','Jonkheer','Lady','Major','Rev','Sir','the Countess')

full$Title[full$Title=='Mlle' | full$Title=='Ms']<-'Miss'
full$Title[full$Title=='Mme']<-'Mrs'
full$Title[full$Title %in% Rare_Title]<-'Rare_Title'

table(full$Title,full$Sex)


#******************************************** Family Size *************************************************

full$Fsize<-full$Parch+full$SibSp+1

ggplot()+geom_bar(data = full[1:891,],aes(x=Fsize,fill=factor(Survived)),position = 'dodge')

full$Ftype[full$Fsize==1]<-'Singleton'
full$Ftype[full$Fsize>1 & full$Fsize<5]<-'Small'
full$Ftype[full$Fsize>4]<-'Large'

ggplot()+geom_bar(data = full[1:891,],aes(x=Ftype,fill=factor(Survived)),position = 'dodge')


#******************************************** Cabin *****************************************************
head(full$Cabin[2])
strsplit(full$Cabin,NULL)[[1]][1]

full$deck<-sapply(full$Cabin,FUN = function(x){strsplit(x,NULL)[[1]][1]})



#******************************************* Missing Value : Embarked ***********************************

head(full$Embarked)


which(full$Embarked=="")

full[c(62,830),c(3,10,16)]

full[(full$Pclass==1 & full$deck=="B" & full$Embarked=="C"),c(3,10,16,12)]

#pclass<-train[train$Pclass==1 & train$Embarked!="",c(10,12) ]

#y_pred=lm(formula=Embarked ~ Fare,data =pclass)


full %>% filter(Pclass==1) %>%group_by(Pclass,Embarked)%>% summarise(mfare = median(Fare,na.rm=TRUE),n = n())

full$Embarked[c(62,830)]<-'C'

#*********************************************** Missing Value : Fare *****************************************

summary(full$Fare)

which(is.na(full$Fare))

full[1044,]

full %>% filter(Pclass=='3' & Embarked=='S') %>% summarise(median(Fare, na.rm = TRUE)) 

full$Fare[1044]<-8.05

#********************************************  Missing Value : Age **********************************************
library(rpart)

summary(full$Age)

pred <- rpart(Age ~Pclass+SibSp+Embarked+Title,data = full[!is.na(full$Age),])

summary(pred)

y_pred<-predict(pred,newdata =full[is.na(full$Age),] )

summary(y_pred)

full$Age[is.na(full$Age)]<-predict(pred,newdata =full[is.na(full$Age),] )

#**************************************************************************

train <- full[1:891,]
test <- full[892:1309,]

set.seed(123)
train$Sex<-factor(train$Sex)
train$Embarked<-factor(train$Embarked)
train$Title<-factor(train$Title)
train$Ftype<-factor(train$Ftype)

test$Sex<-factor(test$Sex)
test$Embarked<-factor(test$Embarked)
test$Title<-factor(test$Title)
test$Ftype<-factor(test$Ftype)

#rf_model<-randomForest(factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Fsize+Ftype,data = train)

rf_model<-randomForest(factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Fsize+Ftype,data = train)

table(rf_model)  #740 right 

pred<-predict(rf_model,test)

solution <- data.frame(PassengerID = test$PassengerId, Survived = pred)

# Write the solution to file
write.csv(solution, file = 'titanic_2.csv', row.names = F)



