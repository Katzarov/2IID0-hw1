library("class")
library("dplyr")
library("e1071")
library("gmodels")
library("rpart")
library("randomForest")  
library("class")
library("gmodels")
library("DMwR")
library("e1071")
library("Boruta")
library("mlbench")
library("caret")
library("mice")
library("VIM")


spdate = read.csv2('speed-dating-clean1.csv', sep=";", dec=".",
                   na.strings = "null", as.is = TRUE,
                   stringsAsFactors = FALSE)

#change type of date.match from bool to factor and rename
spdate$date.match <- factor(spdate$date.match, levels = c("TRUE", "FALSE"),
                        labels = c("MATCH", "NO_MATCH"))


#remove needless columns
spdate <- within(spdate, rm("id", "date.nr", "date.position", 
                            "subject.local.id",
                            "subject.gender.id",
                            "subject.position_start",
                            "subject.global.id",
                            "partner.global.id",
                            "partner.local.id",
                            "wave.nr",
                            "wave.n.people"))


#chack for missing data
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(spdate,2,pMiss)
#apply(spdate,1,pMiss)


#convert bins to numeric
bins <- function(x){is.na(x)}
apply(spdate,2,pMiss)





#select features that have less than 5% missing data
spdate <- select_if(spdate, apply(spdate,2,pMiss) < 5)

#check to see what is left
apply(spdate,2,pMiss)

#keep only logical and numeric columns
spdatec <- spdate["date.match"]
spdatel <- select_if(spdate, is.logical)
spdaten <- select_if(spdate, is.numeric)
spdate <- cbind(spdatec, spdatel, spdaten)


#do imputation on missing data
impute <- mice(spdate, m=3, seed = 123)

#see what has been done
impute

# Complete data
spdate <- complete(impute, 1)

#check to see if imputed
apply(spdate,2,pMiss)

# Feature Selection
set.seed(111)  #keep maxRuns much higher than 30, say 100, or even 500
boruta <- Boruta(date.match ~ ., data = spdate, doTrace = 2, maxRuns = 30)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)

attStats(boruta)

# see confirmed to be important features
getConfirmedFormula(boruta)

# Data Partition
#set.seed(222)
set.seed(455)
ind <- sample(2, nrow(spdate), replace = T, prob = c(0.7, 0.3))
train <- spdate[ind==1,]
test <- spdate[ind==2,]

# Random Forest Model
set.seed(333)
rf <- randomForest(getConfirmedFormula(boruta),data = train, ntree = 5)
rf

#getConfirmedFormula(boruta)

# Prediction & Confusion Matrix - Test
p <- predict(rf, test)
confusionMatrix(p, test$date.match)


# SVM Model
set.seed(333)
svm = svm(getConfirmedFormula(boruta), data = train)
svm

# Prediction & Confusion Matrix - Test
p <- predict(svm, test)
confusionMatrix(p, test$date.match)


# Naive Bayes Model
set.seed(333)
nb = naiveBayes(getConfirmedFormula(boruta), data=train)
nb

# Prediction & Confusion Matrix - Test
p <- predict(nb, test)
confusionMatrix(p, test$date.match)


## Training the model KNN
# k usually is the square root of number of observations
# in this case we have 8378 observations for the train set
knn <- knn(getConfirmedFormula(boruta), train = train, test = test, norm=TRUE, k = 91)

