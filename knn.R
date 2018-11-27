#install.packages(class)

library("class")
library("gmodels")
library("DMwR")
library("e1071")

spdate <- read.csv("speed-dating.csv", stringsAsFactors = FALSE)

svm = svm(formula = date.match ~., data = spdate)

Naive_Bayes_Model = naiveBayes(date.match ~., data=spdate)
Naive_Bayes_Model

# Missing data
p <- function(x) {sum(x=="null")/length(x)*100}
p <- function(x) {sum(is.nan(x))/length(x)*100}

p <- function(x) {if(x=="null")(del)}

apply(spdate, 2, p)
md.pattern(spdate)
md.pairs(data)
marginplot(data[,c('Mileage', 'lc')])

str(spdate)
summary(spdate)

#delete id column, and other 2
spdate <- spdate[-1]
spdate <- spdate[-1]
spdate <- spdate[-1]

#take first some observations so it loads faster
spdate <- spdate[1:1000, ]

#take fist some features(columns) so it loads faster
spdate <- spdate[1:30]




#check match
table(spdate$date.match)

#see the ratio of matches
round(prop.table(table(spdate$date.match)) * 100, digits = 1)




## Data preparation (training and test sets)

#fill in NA
knnImputation(spdate, k = 10, scale = T, meth = "weighAvg",
              distData = NULL)

spdate <- na.omit(spdate)


#prepare labes, get only first column that has the TRUE result
spdate_train_labels <- spdate[1:800, "date.match"]
spdate_test_labels <- spdate[801:1000, "date.match"]


spdate <- spdate[1:3]


# split data via row numbers and keep all columns
spdate_train <- spdate[1:800, ]
spdate_test <- spdate[801:1000, ]






## Training the model
# k usually is the square root of number of observations
# in this case we have 800 observations for the train set
spdate_test_pred <- knn(train = spdate_train, test = spdate_test,
                      cl = spdate_train_labels, k = 28)
# we just got the predicted labes 



#### Evaluating the model

CrossTable(x = spdate_test_labels, y = spdate_test_pred,
           prop.chisq=FALSE)

