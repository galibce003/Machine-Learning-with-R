# Loading Dataset
data <- read.csv('C:/Users/Mehedi Hassan Galib/Desktop/Python/datas/tt.csv')
head(data)


# Remove the unnecessry columns**
data$PassengerId <- NULL
data$Name <- NULL
data$SibSp <- NULL
data$Parch <- NULL
data$Ticket <- NULL
data$Cabin <- NULL


# Checking NA values
colSums(is.na(data))


# Filing up missing values with mean
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}


# Categorical to Factor
data$sex <- factor(data$Sex)
data$embarked <- factor(data$Embarked)
data$survived <- factor(data$Survived)


# Remove the previous categorical columns
data$Sex <- NULL
data$Embarked <- NULL
data$Survived <- NULL
str(data)


# train Test split
set.seed(1234)
p_data <-  sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
train <- data[p_data==1,]
test <- data[p_data==2,]


# Loading "Party"
library(party)


# Decision Tree
tree <- ctree(survived ~ Pclass+Age+sex+Fare+embarked, data = train,
              controls = ctree_control(mincriterion = 0.999))
plot(tree)


# Model Evaluation (Train)

### Confussion Matrix
error_table<- table(predict(tree),train$survived)
print(error_table)

### Misclassification Error
1-sum(diag(error_table))/sum(error_table)



# Model Evaluation (Test)

### Confussion Matrix
test_ev <- predict(tree,test)
error_table_test <- table(test_ev, test$survived)
print(error_table_test)

### Misclassification Error
1-sum(diag(error_table_test))/sum(error_table_test)