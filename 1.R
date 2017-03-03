#import Data
train <- read.csv("e://Workspace/University/R/titanic/train.csv")
test <- read.csv("e://Workspace/University/R/titanic/test.csv")

#add Libraries
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)

#check train data
str(train)

#Passengers survived vs passengers passed away ; with respect to Female and Male
prop.table(table(train$Sex,train$Survived),margin=1)

all<- merge(x = train,y = test, all = TRUE)



#add family size property
all$family_size <- all$SibSp+all$Parch+1

#add title property
all$Title <- gsub('(.*, )|(\\..*)', '', all$Name)
all$Title <- sub(' ', '', all$Title)
all$Title[all$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
all$Title[all$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
all$Title[all$Title %in% c('Dona', 'Lady', 'theCountess', 'Jonkheer')] <- 'Lady'
all$Title <- factor(all$Title)

all$Embarked[c(62, 830)] <- "S"
all$Embarked <- factor(all$Embarked)
all$Fare[1044] <- median(all$Fare, na.rm = TRUE)

# We make a prediction of a passengers Age using the other variables and a decision tree model.
# This time you give method = "anova" since you are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title ,
                       data = all[!is.na(all$Age),], method = "anova")
all$Age[is.na(all$Age)] <- predict(predicted_age, all[is.na(all$Age),])

#add child property
all$Child <- NA
all$Child[all$Age<18] <- 1
all$Child[all$Age>=18] <- 0
all$Child <- factor(all$Child)

# Split the data back into a train set and a test set
train <- all[1:891,]
test <- all[892:1309,]

# Set seed for reproducibility
set.seed(111)

# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare 
	+ Embarked + Title + Child + family_size,
                          data = train, importance=TRUE, ntree=1000)

my_prediction <- predict(my_forest, test)

my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

#write result
write.csv(my_solution, file="9231036_solution.csv", row.names=FALSE)