body_kwd <- statement(select = c("Criteria","Status","CampaignName","AdGroupName","KeywordMatchType","CreativeQualityScore","Impressions","SearchImpressionShare","SearchExactMatchImpressionShare","Clicks","AveragePageviews","AveragePosition","AverageTimeOnSite","Ctr","Cost","AllConversions","BounceRate","FinalUrls"),
                      report = "KEYWORDS_PERFORMANCE_REPORT",
                      start = "2019-06-01",
                      end = "2019-07-07")

kwd_data <- getData(clientCustomerId = '931-388-0493',
                    google_auth = doAuth(),
                    statement = body_kwd,
                    transformation= T,
                    changeNames = T)
kwd_data <- kwd_data %>% filter(!Keyword=="Content")
names(kwd_data) <- str_replace_all(names(kwd_data),'/',"_")

slim <- kwd_data %>% filter(Campaign=="Juris Global Solutions" & Clicks>0 & Keywordstate=="enabled") 

x <- as.matrix(slim %>% select(Impressions,Clicks,Pages_session,`Avg.sessionduration(seconds)`,Bouncerate,Cost,SearchImpr.share,Allconv.))

y <- as.matrix(kwd_data %>% select(Keyword))

x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
x <- sweep(x, 1, rowMeans(x, na.rm = TRUE))
rownames(x) <- slim$Keyword

heatmap(x)
