library(mice)

setwd("C:\\Users\\Alex Sunny\\Downloads\\")

titanicData <- read.csv("titanic.csv", header=T, na.strings=c(""), stringsAsFactors = T)
titanicData$Survived <- factor(titanicData$Survived, levels = c(0,1), labels = c("No","Yes"))

titanicData$Pclass <- as.factor(titanicData$Pclass)
titanicData <- titanicData[, -c(1,11)]


titanicData$Embarked[c(62, 830)] <- 'C'

mice_mod <- mice(titanicData[, !names(titanicData) %in% 
                               c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf') 
mice_output <- complete(mice_mod)
titanicData$Age <- mice_output$Age


titanicData$Child[titanicData$Age < 18] <- "Yes"
titanicData$Child[titanicData$Age >= 18] <- "No"
titanicData$Child <- factor(titanicData$Child)


rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

titanicData$Title <- gsub('(.*, )|(\\..*)', '', titanicData$Name)

titanicData$Title[titanicData$Title == 'Mlle']        <- 'Miss' 
titanicData$Title[titanicData$Title == 'Ms']          <- 'Miss'
titanicData$Title[titanicData$Title == 'Mme']         <- 'Mrs' 
titanicData$Title[titanicData$Title %in% rare_title]  <- 'Rare Title'
titanicData$Title <- as.factor(titanicData$Title)


titanicData$Name <- as.character(titanicData$Name) 
titanicData$Surname <-  sapply(titanicData$Name, 
                               FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
titanicData$Fsize <- titanicData$SibSp + titanicData$Parch + 1

titanicData[3] <- NULL
titanicData[7] <- NULL
titanicData[11] <- NULL


titanicData$FsizeD[titanicData$Fsize == 1] <- 'singleton'
titanicData$FsizeD[titanicData$Fsize < 5 & titanicData$Fsize > 1] <- 'small'
titanicData$FsizeD[titanicData$Fsize > 4] <- 'large'
titanicData$FsizeD <- as.factor(titanicData$FsizeD)


n <- sapply(titanicData, function(x) {is.numeric(x)})
n

numerics<- titanicData[, n]
summary(numerics)


normalize <- function(x) {return ((x - min(x))/(max(x) - min(x)))}
numericsNormal <- normalize(numerics)
summary(numericsNormal)

titanicDataKNN <- titanicData[, !n]
titanicDataKNN <- cbind(titanicDataKNN,numericsNormal)

install.packages("fastDummies")
library(fastDummies)
tkNN <- dummy_cols(titanicDataKNN[,-1])
#tkNN <- dummy_cols(titanicDataKNN[,-1], remove_first_dummy = TRUE)
summary(tkNN)

Survived <- titanicDataKNN$Survived







