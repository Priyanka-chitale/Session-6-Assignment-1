##a.  Preprocess the passenger names to come up with a list of titles that represent families 
getwd()
setwd("D:/acadgilds/rlecture")
library(readr)
library(ggplot2)
library(dplyr)
train= read_csv("train.csv", col_names = TRUE)
test= read_csv("test.csv", col_names = TRUE)
train = bind_rows(train,test)
b1 = gsub("\\, .*","",train$Name, fixed=FALSE)
b1 = as.character(b1)
##  represent using appropriate visualization graph
library(plyr)
family_size = count(b1)
hist(family_size$freq, main = "Histogram of family size vs freq", xlab = "Family size")

##b. Represent the proportion of people survived from the family size using a graph. 
Fsize <- train$SibSp + train$Parch + 1
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size')
##c.  Impute the missing values in Age variable using Mice Library, create two different graphs showing Age distribution before and after imputation.
library(mice)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(train[, !names(train) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(train$Age, freq=F, xlab = "Age", main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, xlab= "Age", main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
