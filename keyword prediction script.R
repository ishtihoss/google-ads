# loand then assign historic_data
# load then assign latest_data 
historic_data<- dec12tosep15
latest_data <- sep16tooct16
historic_data <- historic_data %>% filter(clicks>0 & kw_status=='Enabled')
latest_data <- latest_data %>% filter(clicks>0 & kw_status=='Enabled')

historic_data <- historic_data %>% mutate(Y=clicks/9,sigma= sqrt((clicks-Y)^2/9)/sqrt(9))

ckw_latest_data <- latest_data %>% filter(latest_data$keyword %in% historic_data$keyword) %>% arrange(keyword)

ckw_historic_data <- historic_data %>% filter(historic_data$keyword %in% ckw_latest_data$keyword) %>% arrange(keyword)

ckw_latest_data <- ckw_latest_data %>% mutate(mu=clicks,tau=sqrt((clicks-ckw_historic_data$Y)^2/1)/sqrt(1))

ckw_latest_data %>% mutate(B=ckw_historic_data$sigma^2/(ckw_historic_data$sigma^2)+tau^2)

ckw_latest_data_B<- ckw_latest_data %>% mutate(B=ckw_historic_data$sigma^2/(ckw_historic_data$sigma^2+tau^2))

ckw_latest_data_B<- ckw_latest_data %>% mutate(B=ckw_historic_data$sigma^2/(ckw_historic_data$sigma^2+tau^2))

results <- ckw_latest_data_B %>% mutate(predictionEV = mu + (1-B)*(ckw_historic_data$Y-mu))