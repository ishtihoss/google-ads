## setup
library(googleAnalyticsR)

## This should send you to your browser to authenticate your email. Authenticate with an email that has access to the Google Analytics View you want to use.
ga_auth()

## get your accounts
account_list <- ga_account_list()

## account_list will have a column called "viewId"
account_list$viewId

## View account_list and pick the viewId you want to extract data from. 
ga_id <- 74413772
## simple query to test connection
google_analytics(ga_id,
                 date_range = c("2018-01-01", "2018-06-25"),
                 metrics = "sessions",
                 dimensions = "date")

# Experimental Chunk

ga_data <- google_analytics(ga_id, 
                            date_range = c("2018-01-01", "2018-06-25"),
                            metrics = c("sessions","pageviews","entrances","bounces","users","timeOnPage","pageLoadTime","goalCompletionsAll"),
                            dimensions = c("date","hour","adGroup","referralPath","country","city","adMatchedQuery","pageTitle","pageDepth"),
                            anti_sample = TRUE)