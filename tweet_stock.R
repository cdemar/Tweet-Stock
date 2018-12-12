#library(rvest)
#library(quantmod)
library(twitteR)

# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "7eOqWifupF32hLuSYjbJxxxr0"
consumer_secret <- "4J72qZDCgYzpfdiYXgfiYjT4np7efx7pqe8PZ06HM6lxYk9e41"
access_token <- "1046463050671222784-5AJ0kLLviglrYa2m1hPPyAWh9x2Hbe"
access_secret <- "AQX0GJyUReNIU46oLEShzENcK0HrUDROzJNME3F4EicdJ"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#trump <- userTimeline('realDonaldTrump')

tweets <- searchTwitter('from:realDonaldTrump+election', lang='en',
                        since='2018-01-01', until='2018-10-31')

print(tweets)

#Sys.sleep(.1)

?searchTwitter
