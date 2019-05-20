# Keyword Performance 

body_kwd <- statement(select = c("Criteria","CampaignName","AdGroupName","KeywordMatchType","CreativeQualityScore","Impressions","SearchImpressionShare","SearchExactMatchImpressionShare","AveragePageviews","AveragePosition","AverageTimeOnSite","BounceRate","Clicks","Ctr","Cost","AllConversions"),
                      report = "KEYWORDS_PERFORMANCE_REPORT",
                      start = "2018-01-22",
                      end = "2019-05-11")

kwd_data <- getData(clientCustomerId = '931-388-0493',
                    google_auth = doAuth(),
                    statement = body_kwd,
                    transformation= T,
                    changeNames = T)

names(kwd_data) <- str_replace_all(names(kwd_data),"[/()]","_")

kwd_data <- kwd_data %>% mutate(Conv_stat=ifelse(Allconv.>0,"Hit","Miss"))

jgs <- kwd_data %>% filter(Campaign=="Juris Global Solutions" & !Keyword=="Content")

# Creating Test and Train Data

train_index <- createDataPartition(jgs$Conv_stat,times=1,p=.5,list=FALSE)
train_part <- jgs %>% slice(train_index)
test_part <- jgs %>% slice(-train_index)

#Listing models

models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

# Training all models

library(caret)

fits <- lapply(models, function(model){ 
  print(model)
  train(Conv_stat~Clicks+Impressions+Pages_session+Avg.sessionduration_seconds_+Bouncerate+Position+SearchImpr.share+Cost, method = model, data = train_part)
}) 

names(fits) <- models

# Creating Prediction Matrix


pred <- sapply(fits, function(fits){
  predict(fits,test_part)
})

# Computing Accuracy

c <- seq(1:23)
confusionvector <- sapply(c, function(c){
  + confusionMatrix(factor(fits_predicts[,c]), factor(test_part$Conv_stat))$overall["Accuracy"]})