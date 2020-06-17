# attach the iris dataset to the environment
data(iris)
dataset <- iris

validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]

percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

x <- dataset[,1:4]
y <- dataset[,5]
par(mfrow=c(1,4))
for(i in 1:4) {boxplot(x[,i], main=names(iris)[i])}

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


