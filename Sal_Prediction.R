# Reading Adult Sal. data
library(readr)
adult_sal <- read_csv("~/Desktop/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/adult_sal.csv")
View(adult_sal)
# elminating repeated index
library(dplyr)
adult_sal <- select(adult_sal,-X1)
head(adult_sal)
str(adult_sal)
summary(adult_sal)
table(adult_sal$type_employer)
# Number of unknown employer type.
table(adult_sal$type_employer == '?')
# combining unembloyed empolyees
unemp <- function(job){
  job <- as.character(job)
  if(job=='Without-pay'|job=="Never-worked"){
    return('Unemployed')
  }else{
    return(job)
  }
}
adult_sal$type_employer <- sapply(adult_sal$type_employer,unemp)
table(adult_sal$type_employer)
# Marital Column
table(adult_sal$marital)
# reducing to 3 groups (Married, not married, never married)

Marit_stat <- function(m_stat){
  m_stat <- as.character(m_stat)
  if(m_stat == 'Divorced'|m_stat == 'Separated'| m_stat == 'Widowed'){
   return('Not-Married') 
  }else if(m_stat== 'Married-AF-spouse'| m_stat =='Married-civ-spouse'|m_stat=='Married-spouse-absent'){
    return ('Married') 
  } else{
   return('Never-married') 
  }
}
adult_sal$marital <- sapply(adult_sal$marital, Marit_stat)
table(adult_sal$marital)
# education
# Categories: Doctorate, Masters, Bachelors, Some college, HighSchool, Preschool
table(adult_sal$education)
Education_group <- function(education){
  education <- as.character(education)
  if(education == 'Doctorate'){
    return(education)
  }else if(education == 'Masters'){
    return(education)
  }else if(education == 'Bachelors'){
    return(education)
  }else if (education == 'Some-college' | education == 'HS-grad'){
    return('HS-Grad/Some college')
  }else if(education == 'Assoc-acdm'| education =='Assoc-voc'|education == 'Prof-school'){
    return('Some prof.School')
  }else{
    return('LessThan HS')
  }
}

adult_sal$education <- sapply(adult_sal$education,Education_group)
table(adult_sal$education)
# Country column
table(adult_sal$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North_America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin_and_South_America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

# group_country
group_country <- function(country){
  if(country %in% Asia){
    return('Asia')
  } else if(country %in% North_America){
    return('North America')
  }else if (country %in% Europe){
    return('Europe')
  }else if (country %in% Latin_and_South_America ){
    return('Latin & Sounth America')
  }else {
    return('Other')
  }
}
# converting to factor
adult_sal$country <- sapply(adult_sal$country, group_country)
table(adult_sal$country)
str(adult_sal$country)
adult_sal$type_employer <- sapply(adult_sal$type_employer, factor)
adult_sal$marital <- sapply(adult_sal$marital, factor)
adult_sal$country <- sapply(adult_sal$country, factor)
adult_sal$occupation <- sapply(adult_sal$occupation,factor)
adult_sal$race<- sapply(adult_sal$race,factor)
adult_sal$sex <- sapply(adult_sal$sex, factor)
adult_sal$relationship <- sapply(adult_sal$relationship, factor)
adult_sal$income <- sapply(adult_sal$income, factor)
adult_sal$education <-sapply(adult_sal$education, factor)
str(adult_sal)

adult_sal[adult_sal == '?'] <- NA
adult_sal$type_employer

table(adult_sal$type_employer)
library(Amelia)
missmap(adult_sal)

adult_sal$type_employer

missmap(adult_sal,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

str(adult_sal)

library(ggplot2)
library(dplyr)
# income by age
ggplot(adult_sal, aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1)
# income by race
ggplot(adult_sal, aes(race)) + geom_bar(aes(fill =income),color='black')
# income by sex
ggplot(adult_sal, aes(sex)) + geom_bar(aes(fill =income),color='black')
# income by Marital status
ggplot(adult_sal, aes(marital)) + geom_bar(aes(fill =income),color='black')
# income by country
ggplot(adult_sal, aes(country)) + geom_bar(aes(fill =income),color='black')
# income by hr_per_week 
ggplot(adult_sal, aes(hr_per_week)) + geom_bar(aes(fill =income),color='black')
# income by education
ggplot(adult_sal, aes(education)) + geom_bar(aes(fill =income),color='black')

# split data using caTools
library(caTools)

set.seed(101)
# training sample = 70% and Testing Data = 30%
sample <- sample.split(adult_sal$income, SplitRatio = 0.7)
# training Data
train = subset(adult_sal, sample == TRUE)
# Testing data
 test = subset(adult_sal, sample == FALSE)

# Building a model
model = glm(income ~ ., family = binomial(logit), data = train)

summary(model)

new_step_model <- step(model)

summary(new_step_model)

test$predicted_income = predict(model,newdata = test, type = 'response')
table(test$income, test$predicted_income > 0.5)

# model Accuracy

(6398 + 1378) / (6398 +1378+548+916)
# recall
6398 / (6398 + 525)
# Precision
6398 / (6398 + 916)