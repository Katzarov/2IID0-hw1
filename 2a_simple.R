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
set.seed(123)  


#spdate <- read.csv("speed-dating.csv", stringsAsFactors = FALSE)
spdate = read.csv2('speed-dating-clean.csv', sep=",", dec=".", na.strings = "null", as.is = TRUE)


spdatel <- select_if(spdate, is.logical)
spdaten <- select_if(spdate, is.numeric)
spdate <- cbind(spdatel, spdaten)




# Missing data
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(spdate, 2, p)


spdate <- spdate[,5:20]

knnImputation(spdate, k = 10, scale = T, meth = "weighAvg",
              distData = NULL)










spdatel <- select_if(spdate, is.logical)
spdaten <- select_if(spdate, is.numeric)

#delete date.match column
spdatel <- spdatel[-3]
## Data preparation (training and test sets)
# split data via row numbers and keep all columns

spdatel_train <- spdatel[1:6500, ]
spdatel_test <- spdatel[6501:8378, ]

#prepare labes, get only first column that has the target feature
spdatel_train_labels <- spdate[1:6500, "date.match"]
spdatel_test_labels <- spdate[6501:8378, "date.match"]



## Training the model KNN
# k usually is the square root of number of observations
# in this case we have 469 observations for the train set
spdatel_test_pred <- knn(train = spdatel_train, test = spdatel_test,
                      cl = spdatel_train_labels, k = 91)
# we just got the predicted labes 


#### Evaluating the model

CrossTable(x = spdatel_test_labels, y = spdatel_test_pred,
           prop.chisq=FALSE)



spdatel <- select_if(spdate, is.logical)

rf <- randomForest(formula = date.match ~., data=spdatel, method="class")
print(rf)

