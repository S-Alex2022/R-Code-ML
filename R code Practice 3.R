library()

install.packages(c("ggplot2", "ggthemes", "scales", "dplyr", "mice", "randomForest"))

setwd("C:\\Users\\Alex Sunny\\Downloads\\")

titanicData <-read.csv("titanic.csv", header=T, na.strings=c(""), stringsAsFactors = T)
?read.csv
summary(titanicData)



titanicData$Survived <-as.factor(titanicData$Survived)
titanicData$Pclass <-as.factor(titanicData$Pclass)
str(titanicData)

is.null()

sapply(titanicData, function(x) sum(is.na(x)))

install.packages("data.table")

install.packages(c("Amelia"))
library(Amelia)

?missmap
missmap(titanicData, main = "Missing values vs Observed")

titanicData <- titanicData[!is.na(titanicData$Embarked), ]

titanicData <- titanicData[titanicData$Embarked, ]

?paste
paste("PassengerID: ", titanicData[is.na(titanicData$Embarked), 1], "needs to be corrected" )

paste("PassengerID: ", titanicData[titanicData$Embarked == "62", 1],titanicData$Fare )

details_passenger_62 <- titanicData[titanicData$PassengerId == 62],titanicData$Fare
paste("PassengerID: ", titanicData[titanicData$PassengerId], "Fare:", titanicData[titanicData$Fare], "Name: ", titanicData[titanicData$Name] )



titanicData[titanicData$PassengerId == 62, c(3,6)]
