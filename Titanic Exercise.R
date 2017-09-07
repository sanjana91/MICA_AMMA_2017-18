setwd("C:/Users/hp hp/Documents/GitHub/MICA_AMMA_2017-18/DATA/data_2017")

titanic <- read.csv('train.csv')

str(titanic)

ncol(titanic)
View(titanic)
nrow(titanic)
head(titanic)
titanic_final= titanic[c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare")]
View(titanic_final)
?str
str(titanic_final)
?is.na
titanic_final$Age[is.na(titanic_final$Age)]=mean(titanic_final$Age,na.rm = TRUE)
?mean

set.seed(1234)
titanic_final$rand <- runif(nrow(titanic_final))
titanic_final.train <- titanic_final[titanic_final$rand <= 0.7,]
titanic_final.test <- titanic_final[titanic_final$rand > 0.7,]
nrow(titanic_final.train)
nrow(titanic_final.test)
?CrossTable
hist(titanic_final$Age)
CrossTable(titanic_final$Survived)
CrossTable(titanic_final.test$Survived)
CrossTable(titanic_final.train$Survived)

full.model <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, data = titanic_final.train, family = binomial)
summary(full.model)

fit <- lm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, data = titanic_final.train)
vif(fit)

backward <- step(full.model, direction = 'backward')
summary(backward)

full.model <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch, data = titanic_final.train, family = binomial)
summary(full.model)

full.model <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp, data = titanic_final.train, family = binomial)
summary(full.model)

?predict

titanic_final.train$prob = predict(full.model, type = c("response"))
class(titanic_final.train)
nrow(titanic_final.train)

titanic_final.train$ypred = ifelse(titanic_final.train$prob>=.5,1,0)
table(titanic_final.train$ypred,titanic_final.train$Survived)

titanic_final.test$prob = predict(full.model, newdata = titanic_final.test, type = c("response"))
titanic_final.test$ypred = ifelse(titanic_final.test$prob>=.5,1,0)
table(titanic_final.test$ypred,titanic_final.test$Survived)

newrow = c("1","Female","22.0","1")
View(titanic_final.train)

#End of 19th August

install.packages("randomForest")
library(randomForest)
randomForest(formula = Survived ~ Pclass + Sex + Age + SibSp +Parch + Fare, data = titanic_final)

install.packages("e1071")
library(e1071)
