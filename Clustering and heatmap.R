# Clustering Keywords Script

slim <- kwd_data %>% filter(Campaign=="Juris Global Solutions" & Clicks>0 & Keywordstate=="enabled") 

x <- as.matrix(slim %>% select(`Avg.sessionduration(seconds)`,`Pages/session`,Clicks)

y <- as.matrix(kwd_data %>% select(Keyword))

x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
x <- sweep(x, 1, rowMeans(x, na.rm = TRUE))
rownames(x) <- slim$Keyword

d <- dist(x)

h <- hclust(d)

plot(h, cex = 0.65)

groups <- cutree(h, k = 20)
split(names(groups), groups)
