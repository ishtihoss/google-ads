# Keyword Performance (fetching data from Google Ads API)

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

jgs <- kwd_data %>% filter(Campaign=="Juris Global Solutions")

#Listing Models
models <- c("adaboost")


# Creating Test and Train Data

train_index <- createDataPartition(jgs$Conv_stat,times=1,p=.5,list=FALSE)
train_part <- jgs %>% slice(train_index)
test_part <- jgs %>% slice(-train_index)

# Training Super Model Fit

train_sm_fit <- train(Conv_stat~Clicks+Pages_session+Avg.sessionduration_seconds_+Bouncerate+Position+SearchImpr.share+CTR+Cost,method=models,data=train_part)

pred <- predict(train_sm_fit,test_part)

test_view <- test_part %>% mutate(pred_score=pred)

# Confusion Matrix
confusionMatrix(factor(pred),factor(test_part$Conv_stat))
