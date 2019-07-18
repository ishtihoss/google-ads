# Keyword Performance (fetching data from Google Ads API)

body_kwd <- statement(select = c("Criteria","CampaignName","AdGroupName","KeywordMatchType","CreativeQualityScore","Impressions","SearchImpressionShare","SearchExactMatchImpressionShare","AveragePageviews","AveragePosition","AverageTimeOnSite","BounceRate","Clicks","Ctr","Cost","AllConversions"),
                      report = "KEYWORDS_PERFORMANCE_REPORT",
                      start = "2018-01-01",
                      end = "2019-06-11")

kwd_data <- getData(clientCustomerId = '931-388-0493',
                    google_auth = doAuth(),
                    statement = body_kwd,
                    transformation= T,
                    changeNames = T)

names(kwd_data) <- str_replace_all(names(kwd_data),"[/()]","_")

kwd_data <- kwd_data %>% mutate(Conv_stat=ifelse(Allconv.>0,"Hit","Miss"))

jgs <- kwd_data %>% filter(Campaign=="Juris Global Solutions" & !is.na(SearchImpr.share) & !Keyword=="Content")


# Creating Predictor and To-Be-Predicted 

x <- jgs %>% select(Clicks,Impressions,Avg.sessionduration_seconds_,Pages_session,Bouncerate,Position,SearchImpr.share) %>% mutate(C_z=Clicks/sd(Clicks),Ads_z=Avg.sessionduration_seconds_/sd(Avg.sessionduration_seconds_)) %>% select(C_z,Ads_z,Bouncerate,Position,SearchImpr.share,Pages_session)
y <- jgs %>% select(Conv_stat)

# Test and Train Sets

set.seed(1989)
train_index <- createDataPartition(x$Ads_z,times=1,p=.5,list=FALSE)
x_train <- x %>% slice(train_index)
x_test <- x %>% slice(-train_index)
y_train <- y %>% slice(train_index)
y_test <- y %>% slice(-train_index)
#Model Stacking

models <- c("lda","glm","svmLinear","avNNet","svmRadialCost","rf","mlp","svmRadial")

# Training HULK model

set.seed(1)

fits <- lapply(models, function(model){ 
  print(model)
  train(x_train,as.factor(y_train$Conv_stat), method = model, metric = ifelse(is.factor(as.factor(y_train$Conv_stat)), "Accuracy","RMSE"))
})

names(fits) <- models

# Creating a matrix of predictions

fits_predicts <- sapply(fits, function(fits){
  predict(fits,x_test)
})

j <- seq(1:length(models))
confusionvector <- sapply(j, function(j){
  confusionMatrix(factor(fits_predicts[,j]),factor(y_test$Conv_stat))$overall["Accuracy"]})

#Majority Voting Method

df <- data.frame(matrix(unlist(fits_predicts), nrow=ncol(fits_predicts), byrow=T))
colnames(df) <- seq(1:nrow(fits_predicts))
rownames(df) <- models

col_index <- seq(1,ncol(df), 1)
predict_vote <- map_df(col_index, function(j){
  vote <- ifelse(test = sum(df[,j] == "Hit") > 1, yes = "Hit", no = "Miss")
  return(tibble(vote = vote))
})   # returns a df

predict_vote <- as.factor(predict_vote$vote) #  as factor

confusionMatrix(factor(predict_vote),  factor(y_test$Conv_stat))


results <-  y_test %>% mutate(pred=predict_vote)


