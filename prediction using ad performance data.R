# Pulling Data from Google Ads API
body_adperformance <- statement(select=c("CampaignName","AdGroupName","HeadlinePart1","HeadlinePart2","AdType","Automated","Impressions","Clicks","Ctr","AllConversions","AveragePageviews","BounceRate","Cost","CallToActionText","Month","AdStrengthInfo","CreativeFinalUrls","MultiAssetResponsiveDisplayAdHeadlines","PercentNewVisitors","AveragePosition","AverageTimeOnSite"),
                                report = "AD_PERFORMANCE_REPORT",
                                start = "2018-01-22",end = "2019-05-29")
adperformance_data <- getData(clientCustomerId = '931-388-0493',google_auth = doAuth(),
                              statement=body_adperformance,
                              transformation = T,
                              changeNames = T)
#Basic Clean-up

names(adperformance_data) <- str_replace_all(names(adperformance_data),'[_/%()]','')
adperformance_data <- adperformance_data %>% mutate(Conv_stat=ifelse(Allconv.>0,"Hit","Miss"))
jgs_apr <- adperformance_data %>% filter(Campaign=="Juris Global Solutions")

# Create Test and Train Segment

train_index <- createDataPartition(jgs_apr$Conv_stat,times=1,p=.5,list=F)
train_part <- jgs_apr %>% slice(train_index)
test_part <- jgs_apr %>% slice(-train_index)

# Model Stacking

models <- c("lda",  "naive_bayes", 
              "gamLoess", "qda", 
             "kknn", "wsrf", 
             "monmlp","adaboost")

# Training Model
set.seed(1989)
fits <- lapply(models,function(model){
  print(model)
  train(Conv_stat~Clicks+Pagessession+Bouncerate+newsessions+Position+Avg.sessiondurationseconds,method=model,data=train_part)
})
names(fits) <- models

#Creating a matrix of predictions

fits_predicts <- sapply(fits, function(fits){
  predict(fits,test_part)
})

#Overall Accuracy of Different Algorithms
j <- seq(1:8)
confusionvector <- sapply(j, function(j){
  confusionMatrix(factor(fits_predicts[,j]),factor(test_part$Conv_stat))$overall["Accuracy"]})

sensitivity <- sapply(j,function(j){
  sensitivity(factor(fits_predicts[,j]),factor(test_part$Conv_stat))
})

sense8 <- data.frame(sensitivity) %>% mutate(model=c("lda",  "naive_bayes", 
                                                     "gamLoess", "qda", 
                                                     "kknn", "wsrf", 
                                                     "monmlp","adaboost"))

#Majority Voting Method

df <- data.frame(matrix(unlist(fits_predicts), nrow=8, byrow=T))
colnames(df) <- seq(1:53)
rownames(df) <- models

col_index <- seq(1,ncol(df), 1)
predict_vote <- map_df(col_index, function(j){
  vote <- ifelse(test = sum(df[,j] == "Hit") > 1, yes = "Hit", no = "Miss")
  return(tibble(vote = vote))
})   # returns a df

predict_vote <- as.factor(predict_vote$vote) #  as factor

confusionMatrix(factor(predict_vote),  factor(test_part$Conv_stat))


results <-  test_part %>% mutate(pred=predict_vote)
