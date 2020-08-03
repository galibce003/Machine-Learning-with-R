# Loading Dataset
airquality <- data('airquality')
head(airquality)



# Total missing values
sum(is.na(airquality))

  

# Count missing values for each column
summary(airquality)
# Or :
colSums(is.na(airquality))



# Remove the rows which contain missing values
# na.omit(airquality)



# Filling up missing values with mean
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}