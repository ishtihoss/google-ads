# Install Packages

install.packages("caret")
install.packages("tidyverse")
install.packages("kknn")
install.packages("adabag")
install.packages("gam")
install.packages("MASS")
install.packages("randomForest")
install.packages("Rborist")
install.packages("e1071")
install.packages("fastAdaboost")
install.packages("cloudml")
install.packages("readr")

# Library
library(caret)
library(tidyverse)
library(kknn)
library(adabag)
library(MASS)
library(randomForest)
library(Rborist)
library(e1071)
library(fastAdaboost)
library(cloudml)
library(readr)

# Prepping Data

data_dir <- gs_data_dir_local("gs://birds-nest-bucket")
jgs <- read_csv(file.path(data_dir, "jgs.csv"))
jgs <- jgs[,-1]

# Creating Test and Train Data

train_index <- createDataPartition(jgs$Conv_stat,times=1,p=.5,list=FALSE)
train_part <- jgs %>% slice(train_index)
test_part <- jgs %>% slice(-train_index)

#Model Stacking

models <- c("kknn","adaboost","gamLoess","qda","rf","Rborist")

# Training HULK model
set.seed(1)

fits <- lapply(models, function(model){ 
  print(model)
  train(Conv_stat~Clicks+Impressions+Pages_session+Avg.sessionduration_seconds_+Bouncerate+Position+SearchImpr.share, method = model, data = train_part)
}) 

names(fits) <- models

# Creating a matrix of predictions

fits_predicts <- sapply(fits, function(fits){
  predict(fits,test_part)
})

j <- seq(1:6)
confusionvector <- sapply(j, function(j){
  confusionMatrix(factor(fits_predicts[,j]),factor(test_part$Conv_stat))$overall["Accuracy"]})


#Majority Voting Method

df <- data.frame(matrix(unlist(fits_predicts), nrow=6, byrow=T))
colnames(df) <- seq(1:nrow(fits_predicts))
rownames(df) <- models

col_index <- seq(1,ncol(df), 1)
predict_vote <- map_df(col_index, function(j){
  vote <- ifelse(test = sum(df[,j] == "Hit") >= 1, yes = "Hit", no = "Miss")
  return(tibble(vote = vote))
})   # returns a df

predict_vote <- as.factor(predict_vote$vote) #  as factor

confusionMatrix(factor(predict_vote),  factor(test_part$Conv_stat))


results <- test_part %>% mutate(pred=predict_vote)

best_keywords <- results %>% filter(Conv_stat=="Hit"|pred=="Hit") %>% arrange(desc(Avg.sessionduration_seconds_))

write.csv(best_keywords,file="best_keywords.csv")