# Keyword Performance (fetching data from Google Ads API)

body_kwd <- statement(select = c("Criteria","CampaignName","AdGroupName","KeywordMatchType","CreativeQualityScore","Impressions","SearchImpressionShare","SearchExactMatchImpressionShare","AveragePageviews","AveragePosition","AverageTimeOnSite","BounceRate","Clicks","Ctr","Cost","AllConversions"),
                      report = "KEYWORDS_PERFORMANCE_REPORT",
                      start = "2018-01-22",
                      end = "2019-05-16")

kwd_data <- getData(clientCustomerId = '931-388-0493',
                    google_auth = doAuth(),
                    statement = body_kwd,
                    transformation= T,
                    changeNames = T)

names(kwd_data) <- str_replace_all(names(kwd_data),"[/()]","_")

kwd_data <- kwd_data %>% mutate(Conv_stat=ifelse(Allconv.>0,"Hit","Miss"))

jgs <- kwd_data %>% filter(Campaign=="Juris Global Solutions" & !is.na(SearchImpr.share) & !Keyword=="Content")


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


results <-  test_part %>% mutate(pred=predict_vote)
