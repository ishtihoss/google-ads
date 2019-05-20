models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
                      "gamboost",  "gamLoess", "qda", 
                      "knn", "kknn", "loclda", "gam",
                      "rf", "ranger",  "wsrf", "Rborist", 
                      "avNNet", "mlp", "monmlp",
                      "adaboost", "gbm",
                      "svmRadial", "svmRadialCost", "svmRadialSigma")

library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")
#Building Fits

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

# Creating Prediction Matrix

pred <- sapply(fits,function(fits){
  predict(fits,mnist_27$test)
})

# Computing Accuracy 
c <- seq(1:23)
confusionvector <- sapply(c, function(c){
  + confusionMatrix(factor(pred[,c]), mnist_27$test$y)$overall["Accuracy"]})

# Majority Vote

df <- data.frame(matrix(unlist(pred), nrow=23, byrow=T))
colnames(df) <- seq(1:200)
rownames(df) <- models

col_index <- seq(1,ncol(df))
col_index <- seq(1,ncol(df), 1)
predict_vote <- map_df(col_index, function(j){
  vote <- ifelse(test = sum(df[,j] == 7) > 12, yes = 7, no = 2)
  return(data_frame(vote = vote))
})   # returns a df

predict_vote <- as.factor(predict_vote$vote) #  as factor

confusionMatrix(predict_vote,  mnist_27$test$y)


