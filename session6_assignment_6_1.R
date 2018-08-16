#session6_assignment_6.1

#1. Import the Titanic Dataset from the link Titanic Data Set. 
#Perform the following: 

#a. Preprocess the passenger names to come up with a list of titles that represent families 
#and represent using appropriate visualization graph.


library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(mice)
library(randomForest)

getwd()
path<-"C:/Users/gopikrishna/Documents/R files test"
setwd(path)
library(readr)
train<-read.csv("C:/Users/gopikrishna/Documents/R files test/train.csv", header=T, sep = ",")
View(train)
test<-read.csv("C:/Users/gopikrishna/Documents/R files test/tesst.csv", header=T, sep = ",")

full<-bind_rows(train,test)

full$Fsize <- full$SibSp + full$Parch + 1

strsplit(full$Name, split = '[.,]')[[1]]
full$surname<-sapply(full$Name, FUN = function(x){strsplit(x,split = '[,.]')[[1]][1]}) #list of titles that represents families
full$Family <- paste(full$surname,full$Fsize, sep='_') #family members
histogram(full$Fsize,full$surname)
hist(unique(full$Fsize, incomparables = TRUE))

#b. Represent the proportion of people survived from the family size using a graph. 

     # this incuding the survived list of NA from test data, full$survived is binded data of train and test data

full$Survived<-factor(full$Survived,levels = c(0,1),labels = c("yes","no"))

gplot <- ggplot(data = full,mapping = aes(x = Fsize, fill=full$Survived )) +
  geom_bar(position = "dodge", width = 1) +	       
  ylab("Number of passengers") + xlab("size") +	        
  theme_bw() 


      #considering the samily size in numeric

ggplot(full, mapping=aes(x = Fsize, fill = factor(Survived))) +theme(legend.title = element_blank())+
  geom_bar(width = 3,fill="blue", color="green") +
  ylab("passengers Survived") +
  xlab('Family Size') +ggtitle("family size & survival")
theme_few()

         #deviding the family sizes in 3 parts

full$FsizeD[full$Fsize == 1] <- 'single'
full$FsizeD[full$Fsize <5 & full$Fsize>1] <- 'small'
full$FsizeD[full$Fsize >5] <- 'large'

mosaicplot(table(full$FsizeD , full$Survived), xlim=2,main ='Family Size by Survival', shade=TRUE)


#c. Impute the missing values in Age variable using Mice Library, create two different 
#graphs showing Age distribution before and after imputation. 

summary(full$Age) #The missing age is not randomly distributed across all classes, 
hist(full$Age)      #but is rather concentrated amongst the passengers from 3rd class,
g <-ggplot(full,aes(Age))+theme_minimal()+geom_histogram(binwidth=5)

library(mice)
imp<-mice(as.dataframe(full$Age))
full$AgeClass = ifelse(full$Age<=10,1,
                       ifelse(full$Age>10 & full$Age<=20,2,
                              ifelse(full$Age>20 & full$Age<=35,3,4)))
full$AgeClass = as.factor(full$AgeClass)

md.pattern(full[,!names(full) %in% c("Survived", "Name", "PassengerId", "Ticket", "AgeClass")])

marginplot(data.frame(full$Age, full$Pclass))

table(full[is.na(full$Age), "Pclass"])

imp$Age <- mice(full[, !names(full) %in% c("Survived", "Name", "PassengerId", "Ticket", "AgeClass", "Cabin", "FamilyName")],m=8,maxit=8,meth='pmm',seed=251863)

hist(imp$Age)


