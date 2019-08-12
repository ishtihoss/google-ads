# Ad Group Performance 

body_agp <- statement(select=c('AdGroupName','BounceRate','CampaignName','Impressions','Clicks','Cost','Conversions','Date','AdGroupType'),
                       report="ADGROUP_PERFORMANCE_REPORT",
                       start="2018-10-24",
                       end="2019-01-22")
adgroup_data <- getData(clientCustomerId='931-388-0493',
                      google_auth=doAuth(),
                      statement=body_agp, #object created with statement()
                      transformation = T, #data are transformed from xml text to R dataframe
                      changeNames = T) #column names are changed to more useful expressions

summary <- adgroup_data %>% group_by(Adgroup) %>% summarise(sImpr=sum(Impressions),sClicks=sum(Clicks),sCost=sum(Cost),sConv=sum(Conversions))

summary %>% ggplot(aes(sCost,sClicks,label=Adgroup,col=sConv)) + geom_col() + geom_text_repel()

# Keyword Performance 

body_kwd <- statement(select = c("Criteria","CampaignName","AdGroupName","KeywordMatchType","CreativeQualityScore","Impressions","SearchImpressionShare","SearchExactMatchImpressionShare","AveragePageviews","AveragePosition","AverageTimeOnSite","BounceRate","Clicks","Ctr","Cost","AllConversions"),
                      report = "KEYWORDS_PERFORMANCE_REPORT",
                      start = "2018-01-22",
                      end = "2019-06-27")

kwd_data <- getData(clientCustomerId = '931-388-0493',
                    google_auth = doAuth(),
                    statement = body_kwd,
                    transformation= T,
                    changeNames = T)

names(kwd_data) <- str_replace_all(names(kwd_data),"[/()]","_")


# Search Query Performance 

body_sqp <- statement(select=c('Query','KeywordTextMatchingQuery','QueryTargetingStatus','Impressions','Clicks','Cost','Conversions','Date','DayOfWeek','Month','MonthOfYear'),
                      report="SEARCH_QUERY_PERFORMANCE_REPORT",
                      start="2018-01-22",
                      end="2019-07-26")
sqp_data <- getData(clientCustomerId='931-388-0493',
                        google_auth=doAuth(),
                        statement=body_sqp, #object created with statement()
                        transformation = T, #data are transformed from xml text to R dataframe
                        changeNames = T) #column names are changed to more useful expressions

sqp_data %>% group_by(MonthofYear) %>% summarise(sImpr=sum(Impressions),sClicks=sum(Clicks),sCost=sum(Cost),sConv=sum(Conversions))
sqp_data %>% group_by(Dayofweek) %>% summarise(sImpr=sum(Impressions),sClicks=sum(Clicks),sCost=sum(Cost),sConv=sum(Conversions))
annual_summary <- sqp_data %>% group_by(MonthofYear) %>% summarise(sImpr=sum(Impressions),sClicks=sum(Clicks),sCost=sum(Cost),sConv=sum(Conversions))
annual_summary %>% ggplot(aes(sImpr,sClicks,label=MonthofYear,color=sConv,size=sConv)) + geom_point() + geom_text_repel() + xlab("Total Impressions") + ylab("Total Clicks") + ggtitle("Conversions between 22nd Jan 2018 and 22nd Jan 2019") + theme_light()

# Criteria Performance

body_criteria <- statement(select=c("AdGroupName","CampaignName","Clicks","Conversions","Impressions","Cost","Criteria","HasQualityScore","QualityScore","FinalUrls","Slot","CreativeQualityScore"),
                           report=("CRITERIA_PERFORMANCE_REPORT"),
                           start="2018-01-22",end="2019-01-22")

criteria_data <- getData(clientCustomerId='931-388-0493',
                         google_auth=doAuth(),
                         statement=body_criteria, #object created with statement()
                         transformation = T, #data are transformed from xml text to R dataframe
                         changeNames = T) #column names are changed to more useful expressions

# Ad Performance Report

body_adperformance <- statement(select=c("CampaignName","AdGroupName","HeadlinePart1","HeadlinePart2","AdType","Automated","Impressions","Clicks","Ctr","AllConversions","AveragePageviews","BounceRate"),
                                report = "AD_PERFORMANCE_REPORT",
                                start = "2018-01-22",end = "2019-01-22")
adperformance_data <- getData(clientCustomerId = '931-388-0493',google_auth = doAuth(),
                              statement=body_adperformance,
                              transformation = T,
                              changeNames = T)

# Conversion Lag Bucket 

convlagbuck <- statement(select=c("ConversionLagBucket","AdGroupName","CampaignName","HeadlinePart1","HeadlinePart2"),
                         report="AD_PERFORMANCE_REPORT",
                         start="2018-01-22", end="2019-01-22")
convlb_data <- getData(clientCustomerId = "931-388-0493",
                       google_auth = doAuth(),
                       statement = convlagbuck,
                       transformation = T,
                       changeNames=T)


# Audience performance report

body_audience <- statement(select=c("AdGroupName","AdGroupStatus","Criteria","Impressions","Clicks","FinalUrls","Cost","ClickType","CampaignStatus","CampaignName","AveragePosition","AverageCost","AllConversions","Date","DayOfWeek","Device","Month"),
                                report = "AUDIENCE_PERFORMANCE_REPORT",
                                start = "2018-01-01",end = "2019-07-26")
audience_data <- getData(clientCustomerId = '931-388-0493',google_auth = doAuth(),
                              statement=body_audience,
                              transformation = T,
                              changeNames = T)
