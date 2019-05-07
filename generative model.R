#kwd_data renamed to jgs
jgs <- kwd_data
#Createing binary classification of outcomes 
jgs <- jgs %>% mutate(Conv_Stat=ifelse(Allconv.>0,"Hit","Miss"))
#Defining predictor
y <- jgs$`Pages/session`
# Creating Test and Train sets
set.seed(1989)
test_index <- createDataPartition(y,times = 1,p=.5,list=F)
train_set <- jgs %>% slice(-test_index)
test_set <- jgs %>% slice(test_index)

#Estimating Parameters 
params <- train_set %>% group_by(Conv_Stat) %>% summarise(avg=mean(`Pages/session`),sd=sd(`Pages/session`))
pi <- train_set %>% summarise(pi=mean(Conv_Stat=="Hit")) %>% pull(pi)

#Using average and standard deviation to get rule 
x <- test_set$`Pages/session`

f_miss <- dnorm(x, params$avg[2], params$sd[2])
f_hit <- dnorm(x, params$avg[1], params$sd[1])

# Computing Biased Probability by Ignoring Prevalence 
p_hat_bayes <- f_hit*pi / (f_hit*pi + f_miss*(1 - pi))

#Controlling Prevalence 

p_hat_bayes_unbiased <- f_hit * 0.5 / (f_hit * 0.5 + f_miss * (1 - 0.5)) 
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased> 0.5, "Hit", "Miss")

#Computing Accuracy 

confusionMatrix(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$Conv_Stat))$overall["Accuracy"]

# Checking Result
result <- test_set %>% mutate(y_hat=y_hat_bayes_unbiased)