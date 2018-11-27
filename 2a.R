library(class)
library(ggplot2)
library(caret)

spdate <- read.csv("speed-dating.csv", stringsAsFactors = FALSE)

#to identify the missing data we need to delete all "null" variables

showNullPercentage <- function(x) {sum(x=="null")/length(x)*100}

nullPer <- apply(spdate, 2, showNullPercentage)


nullPer
sort(nullPer, decreasing = T)


barplot(nullPer)

zero = 0
one = 0
ten = 0
twenty = 0
forty = 0
sixty = 0
eighty = 0
above = 0


for(i in nullPer){
  if(i>80)
    above = above + 1
}


smoke <- matrix(c(zero,one,ten,twenty,forty, sixty, eighty, above),ncol=8,byrow=T)
colnames(smoke) <- c("0","<1","<10", "<20", "<40", "<60", "<80", ">=80")
#rownames(smoke) <- c("current","former","never")
smoke <- as.table(smoke)
smoke



# set up boundaries for intervals/bins
breaks <- c(0,1,5,10,20,40,60,80,100)
# specify interval/bin labels
labels <- c("<1%", "1-5%", "5-10%", "10-20%", "20-40%", "40-60%", "60-80%", ">=80%")
# bucketing data points into bins
bins <- cut(nullPer, breaks, include.lowest = T, right=FALSE, labels=labels)
# inspect bins
summary(bins)

plot(bins, main="Percentage of missing data", xlab="percentage of observations with missing data for a feature", ylab="number of features",col="bisque")



library(party)
cf1 <- cforest(date.match ~ . , data= spdate, control=cforest_unbiased(mtry=2,ntree=50)) # fit the random forest



varImp(cf1)


table(nullPer)


nullPer[5]



print(class(nullPer))

for(i in nullPer){
  if(i>5)
  print(i)
  }
